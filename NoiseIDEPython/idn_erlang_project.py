import os
import operator
import wx
from idn_cache import ErlangCache
from idn_colorschema import ColorSchema
from idn_config import Config
from idn_connect import CompileErrorInfo
from idn_console import ErlangIDEConsole, ErlangProjectConsole
from idn_erlang_constats import *
from idn_erlang_explorer import ErlangProjectExplorer
from idn_erlang_project_form import ErlangProjectFrom
from idn_erlangstc import ErlangHighlightedSTCBase, ErlangSTCReadOnly
from idn_errors_table import ErrorsTableGrid
from idn_global import GetMainFrame, GetToolMgr, GetTabMgr, Log
from idn_notebook import ErlangCompileOptionPanel
from idn_project import Project
import idn_projectexplorer as exp
from idn_utils import readFile, writeFile, pystr

__author__ = 'Yaroslav'

class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules', 'noiseide', 'ebin')
    EXPLORER_TYPE = ErlangProjectExplorer

    def OnLoadProject(self):
        GetMainFrame().CheckRuntimes()

        if not self.GetErlangRuntime() or self.GetErlangRuntime() not in Config.Runtimes():
            self.userData[CONFIG_ERLANG_RUNTIME] = Config.Runtimes().keys()[0]
            self.SaveData()

        self.errors = {}
        self.consoleTabs = {}

        ErlangCache.Init(self)

        self.SetupDirs()
        self.AddTabs()
        self.AddConsoles()

        self.explorer.ProjectFilesCreatedEvent += self.OnProjectFilesCreated
        self.explorer.ProjectFilesModifiedEvent += self.OnProjectFilesModified
        self.explorer.ProjectFilesDeletedEvent += self.OnProjectFilesDeleted
        self.explorer.ProjectDirsCreatedEvent += self.OnProjectDirsCreated

    def ErlangCacheChecked(self):
        ErlangCache.LoadCacheFromDir(os.path.join("runtimes", self.GetErlangRuntime()))

    def SetupMenu(self):
        Project.SetupMenu(self)

        self.menu.AppendSeparator()
        self.menu.AppendMenuItem("Rebuild project", self.window, lambda e: self.CompileProject())
        GetMainFrame().erlangMenu.AppendMenuItem("Regenerate erlang cache", self, lambda e: self.RegenerateErlangCache())

    def OnEditProject(self, event):
        ErlangProjectFrom(self).ShowModal()

    def AppsPath(self):
        if not CONFIG_APPS_DIR in self.projectData or not self.projectData[CONFIG_APPS_DIR]:
            return self.projectDir
        return os.path.join(self.projectDir, self.projectData[CONFIG_APPS_DIR])

    def GetErlangRuntime(self):
        return None if not CONFIG_ERLANG_RUNTIME in self.userData else self.userData[CONFIG_ERLANG_RUNTIME]

    def GetErlangPath(self):
        runtime = self.GetErlangRuntime()
        return Config.Runtimes()[runtime]


    def SetupDirs(self):
        projectCacheDir = os.path.join(ErlangCache.CACHE_DIR, self.ProjectName())
        if not os.path.isdir(projectCacheDir):
            os.makedirs(projectCacheDir)
        self.flyDir = os.path.join(GetMainFrame().cwd, "data", "erlang", "fly")
        if not os.path.isdir(self.flyDir):
            os.makedirs(self.flyDir)
        for file in os.listdir(self.flyDir):
            if file.endswith(".erl"):
                os.remove(os.path.join(self.flyDir, file))

    def RegenerateErlangCache(self):
        ErlangCache.CleanDir(os.path.join("runtimes", self.GetErlangRuntime()))
        self.GenerateErlangCache()

    def GenerateErlangCache(self):
        #print "generate erlang cache"
        self.GetShell().GenerateErlangCache()

    def GetShell(self):
        return self.shellConsole.shell

    def GetApp(self, path):
        if not path.lower().startswith(self.AppsPath().lower()):
            return
        app = path.lower().replace(self.AppsPath().lower() + os.sep, "")
        return app[:app.index(os.sep)]

    def Compile(self, path):
        hrls = set()
        erls = set()
        yrls = set()
        def addByType(file):
            if file.endswith(".hrl"):
                hrls.add(file)
            elif file.endswith(".erl"):
                erls.add(file)
            elif file.endswith(".yrl"):
                yrls.add(file)
        if isinstance(path, list):
            for p in path:
                addByType(p)
        else:
            addByType(path)

        while len(hrls) > 0:
            for hrl in hrls:
                self.GetShell().GenerateFileCache(hrl)
                dependent = ErlangCache.GetDependentModules(os.path.basename(hrl))
                for d in dependent:
                    addByType(d)

        self.GetShell().CompileYrls(yrls)
        for erl in erls:
            app = self.GetApp(erl)
            if app in self.projectData[CONFIG_EXCLUDED_DIRS]:
                return
            self.GetShell().CompileProjectFile(erl, app)

    def CompileOption(self, path, option):
        if not path.endswith(".erl"): return
        app = self.GetApp(path)
        if app in self.projectData[CONFIG_EXCLUDED_DIRS]:
            return
        self.GetShell().CompileOption(path, app, option)

    def OnCompileOptionResult(self, path, option, data):
        for page in GetTabMgr().Pages():
            if isinstance(page, ErlangSTCReadOnly):
                if page.filePath == path and page.option == option:
                    page.SetNewText(data)
                    return
        title = "'{}' {}".format(option, os.path.basename(path))
       # Log(title)
        panel = ErlangCompileOptionPanel(GetTabMgr(), path, option, data)
        GetTabMgr().AddCustomPage(panel, title)
#            dlg = CompileWithOptionResult(title, data)
#            dlg.Show()
        #dlg.Show(True)

    def AddConsoles(self):
        #self.shellConsole = ErlangIDEConsole(GetMainFrame(), self.IDE_MODULES_DIR)
        #self.shellConsole.Hide()
        self.SetupShellConsole()

        self.UpdatePaths()

        self.UpdateProjectConsoles()

    def OnIDEConnectionClosed(self):
        if self.shellConsole:
            GetToolMgr().ClosePage(GetToolMgr().FindPageIndexByWindow(self.shellConsole))
        self.SetupShellConsole()

    def SetupShellConsole(self):
        self.connected = False
        self.shellConsole = ErlangIDEConsole(GetToolMgr(), self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", os.path.normcase(ErlangCache.CACHE_DIR))
        self.shellConsole.shell.SetProp("project_dir", os.path.normcase(self.AppsPath()))
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        self.shellConsole.DataReceivedEvent += self.OnShellDataReceived
        GetToolMgr().AddPage(self.shellConsole, "IDE Console")

    def OnShellDataReceived(self, text):
        if "started on:" in text:
            self.shellConsole.DataReceivedEvent -= self.OnShellDataReceived
            self.shellConsole.shell.TaskAddedEvent += self.OnTaskAdded
            self.shellConsole.shell.SocketDataReceivedEvent += self.OnSocketDataReceived
            self.shellConsole.shell.ConnectToSocket()

    def OnTaskAdded(self, task):
        #print task
        self.AddTask(task)

    def OnSocketDataReceived(self, response, js):
        #print response
        if response == "connect":
            self.connected = True
            #self.shellConsole.shell.SocketDataReceivedEvent -= self.OnSocketDataReceived
            self.OnSocketConnected()
        elif response == "compile" or response == "compile_fly":
            errorsData = js["errors"]
            path = pystr(js["path"])

            if response == "compile":
                done = (TASK_COMPILE, path.lower())
            else:
                done = (TASK_COMPILE_FLY, path.lower())
            self.TaskDone("Compiled {}".format(path), done)

            errors = []
            for error in errorsData:
                if error["line"] == "none":
                    return
                errors.append(CompileErrorInfo(path, error["type"], error["line"], error["msg"]))
                #print "compile result: {} = {}".format(path, errors)
            self.AddErrors(path, errors)

        elif response == "gen_file_cache":
            path = pystr(js["path"])
            cachePath = pystr(js["cache_path"])
            ErlangCache.AddToLoad(cachePath)
            self.TaskDone("Generated cache for {}".format(path), (TASK_GEN_FILE_CACHE, path.lower()))
        elif response == "gen_erlang_cache":
            self.ErlangCacheChecked()
        elif response == "compile_option":
            path = pystr(js["path"])
            option = js["option"]
            data = js["result"]
            self.OnCompileOptionResult(path, option, data)

    def OnSocketConnected(self):
        self.SetCompilerOptions()
        self.CreateProgressDialog("Compiling project")
        self.CompileProject()
        self.GenerateErlangCache()
        #print 'on connected ..'

    def UpdatePaths(self):
        dirs = ""
        for app in self.GetApps(True):
            appPath = os.path.join(self.AppsPath(), app)
            ebinDir = os.path.join(appPath, "ebin")
            self.shellConsole.shell.AddPath(ebinDir)
            dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)
        self.dirs = dirs

    def UpdateProjectConsoles(self):
        self.consoles = {}

        consoles = self.projectData[CONFIG_CONSOLES]

        for console in self.consoleTabs:
            if console not in consoles:
                c = self.consoleTabs[console]
                c.Stop()
                index = GetToolMgr().FindPageIndexByWindow(c)
                GetToolMgr().DeletePage(index)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data[CONFIG_CONSOLE_SNAME])
            params.append("-setcookie " + data[CONFIG_CONSOLE_COOKIE])
            params.append(data[CONFIG_CONSOLE_PARAMS])

            params.append("-pa " + self.dirs)

            if title in self.consoleTabs:
                self.consoleTabs[title].SetParams(params)
            else:
                self.consoles[title] = ErlangProjectConsole(GetToolMgr(), self.AppsPath(), params)
                self.consoles[title].SetStartCommand(data[CONFIG_CONSOLE_COMMAND])
                GetToolMgr().AddPage(self.consoles[title], '<{}> Console'.format(title))
                self.consoleTabs[title] = self.consoles[title]

    def GetApps(self, all = False):
        apps = []
        for app in os.listdir(self.AppsPath()):
            if not all and app in self.projectData[CONFIG_EXCLUDED_DIRS]:
                continue
            appPath = os.path.join(self.AppsPath(), app)
            if not os.path.isdir(appPath):
                continue
            apps.append(app)
        return apps

    def AddTabs(self):
        self.errorsTable = ErrorsTableGrid(GetToolMgr(), self)
        GetToolMgr().AddPage(self.errorsTable, 'Errors: 0, Warnings: 0')

    def Close(self):
        ErlangCache.StopCheckingFolder(self.ProjectName())
        self.shellConsole.Stop()
        for title, console in self.consoles.items():
            console.Stop()
        Project.Close(self)
        for w in self.consoleTabs.values() + [self.errorsTable, self.shellConsole]:
            GetToolMgr().ClosePage(GetToolMgr().FindPageIndexByWindow(w))

    def OnProjectFilesModified(self, files):
        toCompile = []
        for file in files:
            editor = GetTabMgr().FindPageByPath(file)
            if editor:
                text = readFile(file)
                if not text: continue
                if unicode(editor.savedText) != unicode(text):
                    dial = wx.MessageDialog(None,
                        'File "{}" was modified.\nDo you want to reload document?'.format(file),
                        'File modified',
                        wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                    if dial.ShowModal() == wx.ID_YES:
                        editor.LoadFile(file)
                        toCompile += file
            else:
                toCompile += file
        self.Compile(toCompile)


    def FileSaved(self, file):
        #Log("saved", file)
        self.Compile(file)


    def OnProjectFilesDeleted(self, files):
        for file in files:
            self.AddErrors(file, [])
            self.RemoveUnusedBeams()
            editor = GetTabMgr().FindPageByPath(file)
            page = GetTabMgr().FindPageIndexByPath(file)

            if editor:
                dial = wx.MessageDialog(None,
                    'File "{}" was deleted.\nDo you want to close tab with deleted document?'.format(file),
                    "File deleted",
                    wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                result = dial.ShowModal()
                if result == wx.ID_YES:
                    GetTabMgr().ClosePage(page)
                else:
                    editor.OnSavePointLeft(None)
                    editor.Changed()

    def OnProjectFilesCreated(self, files):
        self.Compile(files)

    def OnProjectDirsCreated(self, dirs):
        for dir in dirs:
            appPath = dir
            (root, app) = os.path.split(appPath)
            if root == self.AppsPath():
                self.UpdatePaths()
                self.UpdateProjectConsoles()

    def GetEditorTypes(self):
        return {".config": ErlangHighlightedSTCBase,
                ".src": ErlangHighlightedSTCBase,
                ".app": ErlangHighlightedSTCBase}

    def CompileProject(self):
        #print "compile project"
        self.CreateProgressDialog("Compiling project")
        ErlangCache.CleanDir(self.ProjectName())
        filesToCompile = set()
        filesToCache = set()
        yrlToCompile = set()
        for app in self.GetApps():
            srcPath = os.path.join(os.path.join(self.AppsPath(), app), "src")
            includePath = os.path.join(os.path.join(self.AppsPath(), app), "include")
            for path in [srcPath, includePath]:
                #if not os.path.isdir(path): continue
                for root, _, files in os.walk(path):
                    for file in files:
                        file = os.path.join(root, file)
                        if file.endswith(".erl"):
                            filesToCompile.add((file, app))
                        if file.endswith(".yrl"):
                            yrlToCompile.add(file)
                        elif file.endswith(".hrl"):
                            filesToCache.add(file)

        for file in list(yrlToCompile):
            (path, ext) = os.path.splitext(file)
            erl = path + ".erl"
            key = (erl, self.GetApp(erl))
            if key in filesToCompile:
                filesToCompile.remove(key)

        filesToCompile = sorted(list(filesToCompile))
        filesToCache = sorted(list(filesToCache))

        self.GetShell().CompileYrls(yrlToCompile)
        self.GetShell().GenerateFileCaches(filesToCache)
        self.GetShell().CompileProjectFiles(filesToCompile)
        self.RemoveUnusedBeams()

    def RemoveUnusedBeams(self):
        srcFiles = set()
        for app in self.GetApps():
            path = os.path.join(self.AppsPath(), app, "src")
            for root, _, files in os.walk(path):
                for fileName in files:
                    (file, ext) = os.path.splitext(fileName)
                    if ext == ".erl":
                        srcFiles.add(file)
            path = os.path.join(self.AppsPath(), app, "ebin")
            for root, _, files in os.walk(path):
                for fileName in files:
                    (file, ext) = os.path.splitext(fileName)
                    if ext == ".beam":
                        if file not in srcFiles:
                            os.remove(os.path.join(root, fileName))

    def IsFlyCompileEnabled(self):
        return Config.GetProp("erlang_fly_compilation", True)

    def AddErrors(self, path, errors):
        self.errors[path] = errors
        editor = GetTabMgr().FindPageByPath(path)
        if editor:
            editor.HighlightErrors(errors)

        self.errorsTable.AddErrors(path, errors)
        errorCount = 0
        warningCount = 0
        pathErrors = {}
        pathErrors[path] = False
        for (path, err) in self.errors.items():
            for e in err:
                if e.type == CompileErrorInfo.WARNING:
                    warningCount += 1
                else:
                    errorCount += 1
                    pathErrors[path] = True
        pathErrors = sorted(pathErrors.iteritems(), key = operator.itemgetter(1))
        self.explorer.SetPathErrors(pathErrors)
        #wx.CallAfter(GetTabMgr().HighlightErrorPaths, pathErrors)
        GetTabMgr().HighlightErrorPaths(pathErrors)
        index = GetToolMgr().FindPageIndexByWindow(self.errorsTable)
        GetToolMgr().SetPageText(index, "Errors: {}, Warnings: {}".format(errorCount, warningCount))

    def GetErrors(self, path):
        if path not in self.errors: return []
        else: return self.errors[path]

    def CompileFileFly(self, file, realPath, data):
        flyPath = os.path.join(self.flyDir, "fly_" + file)
        writeFile(flyPath, data)
        self.GetShell().CompileFileFly(realPath, flyPath)

    def CompilerOptions(self, projectData = None):
        if not projectData:
            projectData = self.projectData
        return "" if not CONFIG_COMPILER_OPTIONS in projectData else projectData[CONFIG_COMPILER_OPTIONS]

    def UpdateProject(self):
        self.UpdatePaths()
        self.UpdateProjectConsoles()
        self.SetCompilerOptions()
        self.SaveData()
        self.SaveUserData()

    def SetCompilerOptions(self):
        options = self.CompilerOptions().replace("\n", ", ")
        self.shellConsole.shell.SetProp("compiler_options", options)
        if self.CompilerOptions() != self.CompilerOptions(self.oldProjectData):
            self.CompileProject()

class CompileWithOptionResult(wx.Frame):
    def __init__(self, title, text):
        wx.Frame.__init__(self, GetMainFrame(), title = title, size = (800, 800))
        sizer = wx.BoxSizer()
        editor = ErlangHighlightedSTCBaseReadOnly(self, text)
        sizer.Add(editor, 1, wx.EXPAND)
        self.SetSizer(sizer)
        #print "set text"
