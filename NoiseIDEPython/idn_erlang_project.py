import os
import wx
from idn_cache import ErlangCache
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
from idn_utils import readFile, writeFile

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
        self.SetCompilerOptions()
        self.GenerateErlangCache() #test

        self.CompileProject() #test

        self.explorer.Bind(exp.EVT_PROJECT_FILE_CREATED, self.OnProjectFileCreated)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_DELETED, self.OnProjectFileDeleted)
        self.explorer.Bind(exp.EVT_PROJECT_DIR_CREATED, self.OnProjectDirCreated)




        #ErlangCache.LoadCacheFromDir(self.ProjectName())
        #ErlangCache.StartCheckingFolder(self.ProjectName())

    #def TaskDone(self, description, task = None):
    #    Project.TaskDone(self, description, task)

    def ErlangCacheChecked(self):
        ErlangCache.LoadCacheFromDir(os.path.join("erlang", self.GetErlangRuntime()))

    def SetupMenu(self):
        Project.SetupMenu(self)

        self.menu.AppendSeparator()
        self.menu.AppendMenuItem("Regenerate erlang cache", self.window, lambda e: self.RegenerateErlangCache())
        self.menu.AppendMenuItem("Rebuild project", self.window, lambda e: self.CompileProject())

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
        ErlangCache.CleanDir(os.path.join("erlang", self.GetErlangRuntime()))
        self.GenerateErlangCache()

    def GenerateErlangCache(self):
        print "generate erlang cache"
        self.GetShell().GenerateErlangCache()

    def GetShell(self):
        return self.shellConsole.shell

    def GetApp(self, path):
        if not path.lower().startswith(self.AppsPath().lower()):
            return
        app = path.lower().replace(self.AppsPath().lower() + os.sep, "")
        return app[:app.index(os.sep)]

    def Compile(self, path):
        #print path
        if path.endswith(".hrl"):
            self.GetShell().GenerateFileCache(path)
            for module in ErlangCache.GetDependentModules(os.path.basename(path)):
                self.Compile(module)
        elif path.endswith(".yrl"):
            self.GetShell().CompileYrls([path])
        else:
            app = self.GetApp(path)
            if app in self.projectData[CONFIG_EXCLUDED_DIRS]:
                return
            self.GetShell().CompileProjectFile(path, app)

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
        Log(title)
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
        self.shellConsole = ErlangIDEConsole(GetToolMgr(), self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", os.path.normcase(ErlangCache.CACHE_DIR))
        self.shellConsole.shell.SetProp("project_dir", os.path.normcase(self.AppsPath()))
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        GetToolMgr().AddPage(self.shellConsole, "IDE Console")

    def UpdatePaths(self):
        dirs = ""
        for app in self.GetApps():
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

    def GetApps(self):
        apps = []
        for app in os.listdir(self.AppsPath()):
            if app in self.projectData[CONFIG_EXCLUDED_DIRS]:
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
        Project.Close(self)
        self.shellConsole.Stop()
        for title, console in self.consoles.items():
            console.Stop()
        for w in self.consoleTabs.values() + [self.errorsTable, self.shellConsole]:
            GetToolMgr().ClosePage(GetToolMgr().FindPageIndexByWindow(w))

    def OnProjectFileModified(self, event):
        event.Skip()
        file = event.File
        editor = GetTabMgr().FindPageByPath(file)
        if editor:
            text = readFile(file)
            if not text: return
            if unicode(editor.savedText) != unicode(text):
                dial = wx.MessageDialog(None,
                    'File "{}" was modified.\nDo you want to reload document?'.format(file),
                    'File modified',
                    wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                if dial.ShowModal() == wx.ID_YES:
                    wx.CallAfter(editor.LoadFile, file)
                    wx.CallAfter(self.Compile, file)
        else:
            self.Compile(file)


    def FileSaved(self, file):
        #Log("saved", file)
        self.Compile(file)


    def OnProjectFileDeleted(self, event):
        file = event.File
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
                wx.CallAfter(GetTabMgr().ClosePage, page)
            else:
                editor.OnSavePointLeft(None)
                editor.Changed()
        event.Skip()

    def OnProjectFileCreated(self, event):
        file = event.File
        self.Compile(file)
        event.Skip()

    def OnProjectDirCreated(self, event):
        appPath = event.File
        (root, app) = os.path.split(appPath)
        if root == self.AppsPath():
            self.UpdatePaths()
            self.UpdateProjectConsoles()
        event.Skip()

    def GetEditorTypes(self):
        return {".config": ErlangHighlightedSTCBase,
                ".src": ErlangHighlightedSTCBase,
                ".app": ErlangHighlightedSTCBase}

    def CompileProject(self):
        print "compile project"
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
        filesToCompile = sorted(list(filesToCompile))
        filesToCache = sorted(list(filesToCache))
        self.GetShell().CompileProjectFiles(filesToCompile)
        self.GetShell().GenerateFileCaches(filesToCache)
        self.GetShell().CompileYrls(yrlToCompile)
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
        for (_, err) in self.errors.items():
            for e in err:
                if e.type == CompileErrorInfo.WARNING:
                    warningCount += 1
                else:
                    errorCount += 1
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
