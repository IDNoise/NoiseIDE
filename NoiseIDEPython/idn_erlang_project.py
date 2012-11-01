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
from idn_errors_table import ErrorsTableGrid, XrefTableGrid
from idn_global import Log
from idn_notebook import ErlangCompileOptionPanel
from idn_project import Project
import idn_projectexplorer as exp
from idn_utils import readFile, writeFile, pystr, Menu, GetImage

__author__ = 'Yaroslav'

class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules', 'noiseide', 'ebin')
    EXPLORER_TYPE = ErlangProjectExplorer

    def OnLoadProject(self):
        self.window.CheckRuntimes()

        if not self.GetErlangRuntime() or self.GetErlangRuntime() not in Config.Runtimes():
            self.userData[CONFIG_ERLANG_RUNTIME] = Config.Runtimes().keys()[0]
            self.SaveData()

        self.errors = {}
        self.consoleTabs = {}

        self.errorCount = 0
        self.warningCount = 0

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

        self.window.projectMenu.AppendSeparator()
        self.window.projectMenu.AppendMenuItem("Rebuild project", self.window, lambda e: self.CompileProject())
        self.window.projectMenu.AppendMenuItem("XRef check", self.window, lambda e: self.StartXRef())

        self.window.erlangMenu.AppendMenuItem("Regenerate erlang cache", self.window, lambda e: self.RegenerateErlangCache())
        self.window.erlangMenu.AppendSeparator()
        self.window.erlangMenu.AppendCheckMenuItem("Fly Compilation", self.window, self.OnCheckErlangFlyCompilation, Config.GetProp("erlang_fly_compilation", True))
        self.window.erlangMenu.AppendCheckMenuItem("Highlight whole line on error", self.window, self.OnCheckErlangHighlightErrorBackground, Config.GetProp("highlight_error_background", False))

        self.window.viewMenu.AppendMenuItem("Errors/Warnings", self.window, lambda e: self.ShowErrorsTable())
        self.window.viewMenu.AppendMenuItem("IDE Console", self.window, lambda e: self.ShowIDEConsole())
        self.window.viewMenu.AppendMenuItem("XRef Result", self.window, lambda e: self.ShowXrefTable())

        self.consoleMenu = Menu()

        self.window.viewMenu.AppendMenu(wx.NewId(), "Consoles", self.consoleMenu)

        self.window.toolbar.AddSeparator()
        self.rebuildT = self.window.toolbar.AddLabelTool(wx.NewId(), 'Rebuild project', GetImage('build.png'), shortHelp = 'Rebuild project')
        self.xrefCheckT = self.window.toolbar.AddLabelTool(wx.NewId(), 'XRef check', GetImage('xrefCheck.png'), shortHelp = 'XRef check')

        self.window.Bind(wx.EVT_TOOL, lambda e: self.CompileProject(), self.rebuildT)
        self.window.Bind(wx.EVT_TOOL, lambda e: self.StartXRef(), self.xrefCheckT)

        self.window.toolbar.Realize()

    def OnCheckErlangFlyCompilation(self, event):
        currentValue = Config.GetProp("erlang_fly_compilation")
        Config.SetProp("erlang_fly_compilation", not currentValue)

    def OnCheckErlangHighlightErrorBackground(self, event):
        currentValue = Config.GetProp("highlight_error_background")
        Config.SetProp("highlight_error_background", not currentValue)

    def ShowIDEConsole(self):
        if self.window.ToolMgr.FindPageIndexByWindow(self.shellConsole) == None:
            self.window.ToolMgr.AddPage(self.shellConsole, "IDE Console")
        self.window.ToolMgr.FocusOnWidget(self.shellConsole)

    def ShowConsole(self, title, consoleWidget):
        if self.window.ToolMgr.FindPageIndexByWindow(consoleWidget) == None:
            self.window.ToolMgr.AddPage(consoleWidget, "Console <{}>".format(title))
        self.window.ToolMgr.FocusOnWidget(consoleWidget)

    def ShowErrorsTable(self):
        if self.window.ToolMgr.FindPageIndexByWindow(self.errorsTable) == None:
            self.window.ToolMgr.AddPage(self.errorsTable, '')
        index = self.window.ToolMgr.FindPageIndexByWindow(self.errorsTable)
        self.window.ToolMgr.SetPageText(index, "Errors: {}, Warnings: {}".format(self.errorCount, self.warningCount))
        self.window.ToolMgr.FocusOnWidget(self.errorsTable)

    def ShowXrefTable(self):
        if self.window.ToolMgr.FindPageIndexByWindow(self.xrefTable) == None:
            self.window.ToolMgr.AddPage(self.xrefTable, 'XRef result')
        self.window.ToolMgr.FocusOnWidget(self.xrefTable)

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
        self.flyDir = os.path.join(self.window.cwd, "data", "erlang", "fly", self.ProjectName())
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

    def StartXRef(self):
        filesForXref = set()
        for app in self.GetApps():
            path = os.path.join(os.path.join(self.AppsPath(), app), "src")
            for root, _, files in os.walk(path):
                for file in files:
                    file = os.path.join(root, file)
                    if file.endswith(".erl"):
                        filesForXref.add(file)
        self.xrefModules = set()
        for file in filesForXref:
            module = os.path.basename(file)[:-4]
            self.GetShell().XRef(module)
            self.xrefModules.add(module)
        #print "start", self.xrefModules
        self.xrefTable.Clear()
        #self.ShowXrefTable()

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
            toRemove = []
            for hrl in hrls:
                self.GetShell().GenerateFileCache(hrl)
                dependent = ErlangCache.GetDependentModules(os.path.basename(hrl))
                for d in dependent:
                    addByType(d)
                toRemove.append(hrl)
            for h in toRemove:
                hrls.remove(h)

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
        for page in self.window.TabMgr.Pages():
            if isinstance(page, ErlangSTCReadOnly):
                if page.filePath == path and page.option == option:
                    page.SetNewText(data)
                    return
        title = "'{}' {}".format(option, os.path.basename(path))
       # Log(title)
        panel = ErlangCompileOptionPanel(self.window.TabMgr, path, option, data)
        self.window.TabMgr.AddCustomPage(panel, title)

    def AddConsoles(self):
        #self.shellConsole = ErlangIDEConsole(self.window, self.IDE_MODULES_DIR)
        #self.shellConsole.Hide()
        self.SetupShellConsole()

        self.UpdatePaths()

        self.UpdateProjectConsoles()

    def OnIDEConnectionClosed(self):
        if self.shellConsole:
            index = self.window.ToolMgr.FindPageIndexByWindow(self.shellConsole)
            if index != None:
                self.window.ToolMgr.ClosePage(index)
        self.SetupShellConsole()

    def SetupShellConsole(self):
        self.connected = False
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr, self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", os.path.normcase(ErlangCache.CACHE_DIR))
        self.shellConsole.shell.SetProp("project_dir", os.path.normcase(self.AppsPath()))
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        self.shellConsole.onlyHide = True
        self.ShowIDEConsole()
        self.shellConsole.DataReceivedEvent += self.OnShellDataReceived


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

        elif response == "xref_module":
            module = pystr(js["module"])

            self.xrefModules.remove(module)
            if not module in ErlangCache.moduleData: return
            undefined = [((u["where_m"], u["where_f"], u["where_a"]),
                          (u["what_m"], u["what_f"], u["what_a"])) for u in js["undefined"]]
            #print "xref result", module, undefined
            self.AddXRefErrors(ErlangCache.moduleData[module].file, undefined)
            #print "result", module, self.xrefModules
            if len(self.xrefModules) == 0:
                self.ShowXrefTable()
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

    def AddXRefErrors(self, path, errors):
        self.xrefTable.AddErrors(path, errors)

    def UpdateProjectConsoles(self):
        self.consoles = {}

        consoles = self.projectData[CONFIG_CONSOLES]

        for console in self.consoleTabs:
            if console not in consoles:
                c = self.consoleTabs[console]
                c.Stop()
                index = self.window.ToolMgr.FindPageIndexByWindow(c)
                if index != None:
                    self.window.ToolMgr.DeletePage(index, True)
                id = self.consoleMenu.FindItem('Console <{}>'.format(console))
                if id != wx.NOT_FOUND:
                    self.consoleMenu.Delete(id)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data[CONFIG_CONSOLE_SNAME])
            params.append("-setcookie " + data[CONFIG_CONSOLE_COOKIE])
            params.append(data[CONFIG_CONSOLE_PARAMS])

            params.append("-pa " + self.dirs)

            if title in self.consoleTabs:
                self.consoleTabs[title].SetParams(params)
                self.consoleTabs[title].SetStartCommand(data[CONFIG_CONSOLE_COMMAND])
            else:
                self.consoles[title] = ErlangProjectConsole(self.window.ToolMgr, self.AppsPath(), params)
                self.consoles[title].onlyHide = True
                self.consoles[title].SetStartCommand(data[CONFIG_CONSOLE_COMMAND])
                self.consoleTabs[title] = self.consoles[title]
                self.ShowConsole(title, self.consoles[title])
                #self.window.ToolMgr.AddPage(self.consoles[title], 'Console <{}>'.format(title))

            def showConsole(title):
                return lambda e: self.ShowConsole(title, self.consoleTabs[title])
            self.consoleMenu.AppendMenuItem("Console <{}>".format(title), self.window, showConsole(title))

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
        self.errorsTable = ErrorsTableGrid(self.window.ToolMgr, self)
        self.errorsTable.onlyHide = True

        self.xrefTable = XrefTableGrid(self.window.ToolMgr, self)
        self.xrefTable.onlyHide = True
        self.xrefTable.Hide()

        self.ShowErrorsTable()

    def Close(self):
        ErlangCache.StopCheckingFolder(self.ProjectName())
        self.shellConsole.Stop()
        for title, console in self.consoles.items():
            console.Stop()
        Project.Close(self)
        for w in self.consoleTabs.values() + [self.errorsTable, self.shellConsole, self.xrefTable]:
            index = self.window.ToolMgr.FindPageIndexByWindow(w)
            #print "try delete page", w, index
            if index != None:
                #print "delete page", w
                self.window.ToolMgr.DeletePage(index, True)
        self.window.toolbar.DeleteTool(self.xrefCheckT.GetId())
        self.window.toolbar.DeleteTool(self.rebuildT.GetId())
        self.window.toolbar.DeleteToolByPos(self.window.toolbar.GetToolsCount() - 1)

    def OnProjectFilesModified(self, files):
        toCompile = []
        for file in files:
            editor = self.window.TabMgr.FindPageByPath(file)
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
            self.ClearCacheForFile(file)
            editor = self.window.TabMgr.FindPageByPath(file)
            page = self.window.TabMgr.FindPageIndexByPath(file)

            if editor:
                dial = wx.MessageDialog(None,
                    'File "{}" was deleted.\nDo you want to close tab with deleted document?'.format(file),
                    "File deleted",
                    wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                result = dial.ShowModal()
                if result == wx.ID_YES:
                    self.window.TabMgr.ClosePage(page)
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

    def ClearCacheForFile(self, path):
        name = os.path.basename(path)
        if name.endswith(".erl"):
            name = name[:-4]
        ErlangCache.UnloadModule(name)

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

#        print "compile: ", filesToCompile
#        print "yrl compile: ", yrlToCompile
#        print "cache: ", filesToCache

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
        editor = self.window.TabMgr.FindPageByPath(path)
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
        #wx.CallAfter(self.window.TabMgr.HighlightErrorPaths, pathErrors)
        self.window.TabMgr.HighlightErrorPaths(pathErrors)
        index = self.window.ToolMgr.FindPageIndexByWindow(self.errorsTable)
        self.errorCount = errorCount
        self.warningCount = warningCount
        if index != None:
            self.ShowErrorsTable()

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
