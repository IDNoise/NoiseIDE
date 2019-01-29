import yaml

__author__ = 'Yaroslav'

import os
import operator
import wx

from idn_cache import ErlangCache, IgorCache
from idn_config import Config
from idn_connect import CompileErrorInfo
from idn_console import ErlangIDEConsole, ErlangProjectConsole
from idn_erlang_constats import *
from idn_erlang_dialogs import ErlangDialyzerDialog
from idn_erlang_explorer import ErlangProjectExplorer
from idn_erlang_project_form import ErlangProjectFrom
from idn_erlangstc import ErlangHighlightedSTCBase, ErlangSTCReadOnly
from idn_errors_table import ErrorsTableGrid, XrefTableGrid, DialyzerTableGrid
from idn_notebook import ErlangCompileOptionPanel
from idn_project import Project
from idn_utils import readFile, writeFile, pystr, Menu, GetImage, decode
from idn_erlang_utils import IsBeam, IsInclude, IsYrl, IsModule, IsIgor, IsAppSrc
import core

def LoadProject(filePath):
    projectData = yaml.load(file(filePath, 'r'))
    project = None
    if not CONFIG_PROJECT_TYPE in projectData or projectData[CONFIG_PROJECT_TYPE] == MULTIPLE_APP_PROJECT:
        projectData[CONFIG_PROJECT_TYPE] = MULTIPLE_APP_PROJECT
        project = MultipleAppErlangProject
    elif projectData[CONFIG_PROJECT_TYPE] == SINGLE_APP_PROJECT:
        project = SingleAppErlangProject
    return project(core.MainFrame, filePath, projectData)

class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules', 'apps', 'noiseide', 'ebin')
    EXPLORER_TYPE = ErlangProjectExplorer

    def OnLoadProject(self):
        self.CheckRuntimes()

        if not CONFIG_PROJECT_TYPE in self.projectData:
            self.projectData[CONFIG_PROJECT_TYPE] = MULTIPLE_APP_PROJECT

        if self.ProjectType() == MULTIPLE_APP_PROJECT:
            if not CONFIG_DEPS_DIR in self.projectData:
                self.projectData[CONFIG_DEPS_DIR] = "deps"

            if not os.path.isdir(self.DepsPath()):
                os.mkdir(self.DepsPath())

        self.SaveData()

        self.errors = {}
        self.consoleTabs = {}

        self.errorCount = 0
        self.warningCount = 0

        ErlangCache.Init(self)
        IgorCache.Init(self)

        self.SetupDirs()
        ErlangCache.LoadCacheFromDir(self.ProjectName())
        ErlangCache.LoadCacheFromDir(os.path.join("runtimes", self.GetErlangRuntime()))
        self.AddTabs()
        self.AddConsoles()
        self.SetupProps()

        self.explorer.ProjectFilesCreatedEvent += self.OnProjectFilesCreated
        self.explorer.ProjectFilesDeletedEvent += self.OnProjectFilesDeleted

        self.CacheIgor()


    def ProjectType(self):
        return self.projectData[CONFIG_PROJECT_TYPE]

    def CheckRuntimes(self):
        self.window.CheckRuntimes()
        if self.GetErlangRuntime() and self.GetErlangRuntime() not in Config.AvailableRuntimes():
            wx.MessageBox("Project runtime '{}' has not existing path. Please specify proper path or other runtime will be selected.".format(self.GetErlangRuntime()), "Error")
            self.window.SetupRuntimes()

        if not self.GetErlangRuntime() or self.GetErlangRuntime() not in Config.AvailableRuntimes():
            self.userData[CONFIG_ERLANG_RUNTIME] = Config.Runtimes().keys()[0]
            self.SaveData()

    def SetupProps(self):
        if self.PltPath():
            self.GetShell().SetProp("plt", self.PltPath())
        if self.HomeDir():
            self.GetShell().SetHomeDir(self.HomeDir())

    def ErlangCacheChecked(self):
        d = os.path.join("runtimes", self.GetErlangRuntime())
        if os.path.isdir(d):
            ErlangCache.LoadCacheFromDir(d)

    def GetWorkDir(self):
        workDir = "" if not CONFIG_WORK_DIR in self.projectData else self.projectData[CONFIG_WORK_DIR]
        if workDir:
            return os.path.join(self.projectDir, workDir)
        else:
            return self.projectDir

    def SetupMenu(self):
        Project.SetupMenu(self)

        self.window.projectMenu.AppendSeparator()
        self.SetCompileMenuItems()
        self.window.projectMenu.AppendSeparator()
        self.window.projectMenu.AppendMenuItem("Rebuild tests", self.window, lambda e: self.RebuildTests(), "F9")
        self.window.projectMenu.AppendSeparator()
        self.window.projectMenu.AppendMenuItem("XRef check", self.window,
                                               lambda e: self.StartXRef())
        self.window.projectMenu.AppendMenuItem("XRef check (with deps)", self.window,
                                               lambda e: self.StartXRef(True))

        self.dialyzerMenu = Menu()
        self.dialyzerMenu.AppendMenuItem("Edit Options", self.window, self.OnEditDialyzerOptions)
        self.dialyzerMenu.AppendSeparator()
        self.dialyzerMenu.AppendMenuItem("Project", self.window,
                                         lambda e: self.DialyzeProject())
        self.dialyzerMenu.AppendMenuItem("Current module", self.window, self.OnDialyzerRunModule)
        self.window.projectMenu.AppendMenu(wx.ID_ANY, "Dialyzer", self.dialyzerMenu)

        self.window.erlangMenu.AppendMenuItem("Regenerate erlang cache", self.window,
                                              lambda e: self.RegenerateErlangCache())
        self.window.erlangMenu.AppendSeparator()
        self.window.erlangMenu.AppendCheckMenuItem("Fly Compilation", self.window,
                                                   self.OnCheckErlangFlyCompilation,
                                                   Config.GetProp("erlang_fly_compilation", True))
        self.window.erlangMenu.AppendCheckMenuItem("Highlight whole line on error", self.window,
                                                   self.OnCheckErlangHighlightErrorBackground,
                                                   Config.GetProp("highlight_error_background", False))

        self.window.viewMenu.AppendMenuItem("Errors/Warnings", self.window, lambda e: self.ShowErrorsTable())
        self.window.viewMenu.AppendMenuItem("IDE Console", self.window, lambda e: self.ShowIDEConsole())
        self.window.viewMenu.AppendMenuItem("XRef Result", self.window, lambda e: self.ShowXrefTable())

        self.consoleMenu = Menu()

        self.window.viewMenu.AppendMenu(wx.ID_ANY, "Consoles", self.consoleMenu)

        self.window.toolbar.AddSeparator()
        self.rebuildT = self.window.toolbar.AddLabelTool(wx.ID_ANY, 'Rebuild apps',
                                                         GetImage('build.png'),
                                                         shortHelp = 'Rebuild apps')
        self.xrefCheckT = self.window.toolbar.AddLabelTool(wx.ID_ANY, 'XRef check',
                                                           GetImage('xrefCheck.png'),
                                                           shortHelp = 'XRef check')

        self.window.Bind(wx.EVT_TOOL, lambda e: self.Rebuild(), self.rebuildT)
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
        ErlangProjectFrom(self, self.ProjectType()).ShowModal()

    def OnEditDialyzerOptions(self, event):
        ErlangDialyzerDialog(self.window, self).ShowModal()

    def OnDialyzerRunModule(self, event):
        editor = self.window.TabMgr.GetActiveEditor()
        if not editor:
            wx.MessageBox("No modules opened.", "Dialyzer")
            return
        if not IsModule(editor.filePath):
            wx.MessageBox("Current file is not erlang module.", "Dialyzer")
            return
        self.DialyzeModules(editor.filePath)

    def PltPath(self):
        return None if not CONFIG_PLT_FILE_PATH in self.userData else self.userData[CONFIG_PLT_FILE_PATH]

    def SetPltPath(self, path):
        self.userData[CONFIG_PLT_FILE_PATH] = path
        if not path: return
        self.GetShell().SetProp("plt", path)

    def HomeDir(self):
        return None if not CONFIG_HOME_PATH in self.userData else self.userData[CONFIG_HOME_PATH]

    def SetHomeDir(self, path):
        self.userData[CONFIG_HOME_PATH] = path
        if not path: return
        self.GetShell().SetHomeDir(path)

    def CheckPlt(self):
        if not self.PltPath():
            wx.MessageBox("Please specify plt for dialyzer. Project -> Dialyzer -> Edit Options.", "Dialyzer")
            return False
        return True

    def DialyzeModules(self, files):
        if not self.CheckPlt(): return
        beams = set()
        for f in files:
            if not IsModule(f): continue
            beamPath = self.GetBeamPathFromSrcPath(f)
            if not beamPath: continue
            beams.add(beamPath)
        if len(beams) == 0:
            wx.MessageBox("No modules to dialyze.", "Dialyzer")
            return
        self.GetShell().DialyzeModules(list(beams))

    def AppsPath(self):
        if self.projectData[CONFIG_APPS_DIR] == "":
            return self.projectDir;
        return os.path.join(self.projectDir, self.projectData[CONFIG_APPS_DIR])

    def DepsPath(self):
        if self.projectData[CONFIG_DEPS_DIR] == "":
            return self.projectDir;
        return os.path.join(self.projectDir, self.projectData[CONFIG_DEPS_DIR])

    def GetErlangRuntime(self):
        return None if not CONFIG_ERLANG_RUNTIME in self.userData else decode(self.userData[CONFIG_ERLANG_RUNTIME])

    def GetErlangPath(self):
        runtime = self.GetErlangRuntime()
        return Config.Runtimes()[runtime]

    def SetupDirs(self):
        projectCacheDir = os.path.join(ErlangCache.CacheDir(), self.ProjectName())
        if not os.path.isdir(projectCacheDir):
            os.makedirs(projectCacheDir)
        self.flyDir = os.path.join(core.TempDir(), "fly", self.ProjectName())
        if not os.path.isdir(self.flyDir):
            os.makedirs(self.flyDir)
        for f in os.listdir(self.flyDir):
            if IsModule(f):
                os.remove(os.path.join(self.flyDir, f))

    def RegenerateErlangCache(self):
        ErlangCache.CleanDir(os.path.join("runtimes", self.GetErlangRuntime()))
        self.GenerateErlangCache()

    def GenerateErlangCache(self):
        self.GetShell().GenerateErlangCache(self.GetErlangRuntime())

    def StartXRef(self, withDeps = False):
        self.xrefProblemsCount = 0
        modules = self.GetXRefModules(withDeps)
        self.xrefModules = set()
        for module in modules:
            self.GetShell().XRef(module)
            self.xrefModules.add(module)
        self.xrefTable.Clear()

    def GetShell(self):
        if not self.shellConsole:
            return None
        return self.shellConsole.shell

    def Compile(self, path, ingoreExclusion = False):
        hrls = set()
        erls = set()
        igors = set()
        def addByType(path):
            isSrc = self.IsSrcPath(path)
            if IsInclude(path) and isSrc:
                hrls.add(path)
            elif IsModule(path) and isSrc:
                erls.add(path)
            elif IsYrl(path) and isSrc:
                erls.add(path)
            elif IsAppSrc(path) and isSrc:
                erls.add(path)
            elif IsIgor(path):
                igors.add(path)

        if isinstance(path, list):
            for p in path:
                addByType(p)
        else:
            addByType(path)
        while len(hrls) > 0:
            toRemove = []
            for hrl in hrls:
                self.GetShell().GenerateFileCache(hrl)
                dependent = ErlangCache.GetDependentModules(hrl)
                for d in dependent:
                    addByType(d)
                toRemove.append(hrl)
            for h in toRemove:
                hrls.remove(h)

        for erl in erls:
            if ingoreExclusion or self.CanCompileModule(erl) and not self.GetShell() is None:
                self.GetShell().Compile(erl)

        for igor in igors:
            IgorCache.GenerateForFile(igor)

    def CanCompileModule(self, erl):
        return True

    def CompileOption(self, path, option):
        if not IsModule(path): return
        self.GetShell().CompileOption(path, option)

    def OnCompileOptionResult(self, path, option, data):
        for page in self.window.TabMgr.Pages():
            if isinstance(page, ErlangSTCReadOnly):
                if page.filePath == path and page.option == option:
                    page.SetNewText(data)
                    return
        title = "'{}' {}".format(option, os.path.basename(path))
        panel = ErlangCompileOptionPanel(self.window.TabMgr, path, option, data)
        self.window.TabMgr.AddCustomPage(panel, title)

    def AddConsoles(self):
        self.SetupShellConsole()

        self.UpdatePaths()

        self.UpdateProjectConsoles()

    def OnIDEConnectionClosed(self):
        if self.shellConsole:
            index = self.window.ToolMgr.FindPageIndexByWindow(self.shellConsole)
            if index != None:
                self.window.ToolMgr.ClosePage(index)
        self.SetupShellConsole()

    def OnProjectClientApiConnectionClosed(self, console):
        console.WriteToConsoleOut("API CONNECTION CLOSED :(/n")

    def OnProjectClientApiDataRecieved(self, console, action, js):
        #core.Log("api: " + action + ". data: " + str(js))
        try:
            if action == "goto_line":
                module = js["module"]
                line = js["line"]
                core.TabMgr.LoadFileLine(ErlangCache.modules[module].file, line - 1, False)
            elif action == "goto_file":
                filePath = pystr(js["file"])
                line = js["line"]
                core.TabMgr.LoadFileLine(filePath, line - 1, False)
            elif action == "goto_mfa":
                module = js["module"]
                fun = js["fun"]
                arity = js["arity"]
                f = ErlangCache.ModuleFunction(module, fun, arity)
                core.TabMgr.LoadFileLine(f.file, f.line - 1, False)
            else:
                core.Log("Unknown data in client api: " + action)
        except Exception, e:
            import traceback
            core.Log("Error in socket: {}".format(e))
            traceback.print_exc()

    def SetupShellConsole(self):
        self.connected = False
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr, self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", ErlangCache.CacheDir())
        self.shellConsole.shell.SetProp("project_dir", self.projectDir)
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        self.shellConsole.onlyHide = True
        self.ShowIDEConsole()
        self.shellConsole.DataReceivedEvent += self.OnShellDataReceived
        self.shellConsole.shell.ClosedConnectionEvent += self.OnIDEConnectionClosed

    def OnShellDataReceived(self, text):
        if "started on:" in text:
            self.shellConsole.DataReceivedEvent -= self.OnShellDataReceived
            self.shellConsole.shell.TaskAddedEvent += self.OnTaskAdded
            self.shellConsole.shell.SocketDataReceivedEvent += self.OnSocketDataReceived
            self.shellConsole.shell.ConnectToSocket()

    def OnTaskAdded(self, task):
        self.AddTask(task)

    def OnSocketDataReceived(self, response, js):
        try:
            if response == "connect":
                self.connected = True
                self.OnSocketConnected()
            elif response == "compile" or response == "compile_fly":
                errorsData = js["errors"]
                path = pystr(js["path"])
                if response == "compile":
                    done = (TASK_COMPILE, path.lower())
                else:
                    done = (TASK_COMPILE_FLY, path.lower())
                if not self.explorer.FindItemByPath(path): return
                self.TaskDone("Compiled {}".format(path), done)

                errors = []
                for error in errorsData:
                    if error["line"] == "none":
                        errors.append(CompileErrorInfo(path, error["type"], 0, error["msg"]))
                    errors.append(CompileErrorInfo(path, error["type"], error["line"], error["msg"]))
                self.AddErrors(path, errors)

            elif response == "compile_app":
                resultData = js["result"]
                path = pystr(js["path"])
                self.TaskDone("App compiled {}".format(path), (TASK_COMPILE_APP, path.lower()))
                for eRec in resultData:
                    p = pystr(eRec["path"])
                    if 'nt' == os.name:
                        p = p[0].upper() + p[1:]
                    if not self.explorer.FindItemByPath(p): continue
                    errors = []
                    for error in eRec["errors"]:
                        if error["line"] == "none":
                            errors.append(CompileErrorInfo(p, error["type"], 0, error["msg"]))
                        errors.append(CompileErrorInfo(p, error["type"], error["line"], error["msg"]))
                    self.AddErrors(p, errors)

            elif response == "cache_app":
                path = pystr(js["path"])
                self.TaskDone("App cached {}".format(path), (TASK_CACHE_APP, path.lower()))
            elif response == "xref_module":
                module = pystr(js["module"])
                if module in self.xrefModules:
                    self.xrefModules.remove(module)
                if not module in ErlangCache.modules: return
                undefined = [((u["where_m"], u["where_f"], u["where_a"]),
                              (u["what_m"], u["what_f"], u["what_a"])) for u in js["undefined"]]
                self.AddXRefErrors(ErlangCache.modules[module].file, undefined)
                self.xrefProblemsCount += len(undefined)
                if len(self.xrefModules) == 0:
                    self.xrefTable.PrepareResult()
                    self.ShowXrefTable()
            elif response == "dialyzer":
                warnings = js["warnings"]
                self.ShowDialyzerResult(warnings)
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
        except Exception, e:
            import traceback
            core.Log("Error in socket: {}".format(e))
            traceback.print_exc()

    def OnSocketConnected(self):
        self.SetCompilerOptions()

    def AddXRefErrors(self, path, errors):
        self.xrefTable.AddErrors(path, errors)

    def ShowDialyzerResult(self, warnings):
        dialyzerTable = DialyzerTableGrid(self.window.ToolMgr, self)
        dialyzerTable.SetWarnings(warnings)
        self.window.ToolMgr.AddPage(dialyzerTable, "Dialyzer result")
        self.window.ToolMgr.FocusOnWidget(dialyzerTable)

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
        for item in self.consoleMenu.MenuItems:
            self.consoleMenu.DeleteItem(item)

        def project_console_conn_closed(console):
            return lambda: self.OnProjectClientApiConnectionClosed(console)

        def project_console_data_recv(console):
            return lambda action, js: self.OnProjectClientApiDataRecieved(console, action, js)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data[CONFIG_CONSOLE_SNAME])
            params.append("-setcookie " + data[CONFIG_CONSOLE_COOKIE])
            params.append(data[CONFIG_CONSOLE_PARAMS])

            params.append("-pa " + self.dirs)

            if title in self.consoleTabs:
                console = self.consoleTabs[title]
                console.SetParams(params)
                console.SetCWD(self.GetWorkDir())
                console.SetStartCommand(data[CONFIG_CONSOLE_COMMAND])
            else:
                console = ErlangProjectConsole(self.window.ToolMgr, self.GetWorkDir(), params)
                console.shell.ClosedConnectionEvent += project_console_conn_closed(console)
                console.shell.SocketDataReceivedEvent += project_console_data_recv(console)
                console.onlyHide = True
                console.SetStartCommand(data[CONFIG_CONSOLE_COMMAND])
                self.consoleTabs[title] = console
                self.consoles[title] = console
                self.ShowConsole(title, console)

            def showConsole(title):
                return lambda e: self.ShowConsole(title, self.consoleTabs[title])
            self.consoleMenu.AppendMenuItem("Console <{}>".format(title), self.window, showConsole(title))

    def AddTabs(self):
        self.errorsTable = ErrorsTableGrid(self.window.ToolMgr, self)
        self.errorsTable.onlyHide = True

        self.xrefTable = XrefTableGrid(self.window.ToolMgr, self)
        self.xrefTable.onlyHide = True
        self.xrefTable.Hide()

        self.ShowErrorsTable()

    def Close(self):
        self.shellConsole.Stop(True)
        for title, console in self.consoles.items():
            console.Stop(True)

        self.window.ToolMgr.CloseAll()
        self.window.toolbar.DeleteTool(self.xrefCheckT.GetId())
        self.window.toolbar.DeleteTool(self.rebuildT.GetId())
        self.window.toolbar.DeleteToolByPos(self.window.toolbar.GetToolsCount() - 1)
        ErlangCache.Stop()
        Project.Close(self)

    def OnProjectFilesModified(self, files):
        toCompile = []
        for f in files:
            editor = self.window.TabMgr.FindPageByPath(f)
            if editor:
                text = readFile(f)
                if not text: continue
                if editor.savedText != text:
                    dial = wx.MessageDialog(None,
                        'File "{}" was modified.\nDo you want to reload document?'.format(f),
                        'File modified',
                        wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                    if dial.ShowModal() == wx.ID_YES:
                        editor.LoadFile(f)
                        toCompile.append(f)
            else:
                toCompile.append(f)
        self.Compile(toCompile)


    def FileSaved(self, path):
        self.Compile(path, True)

    def OnProjectFilesDeleted(self, files):
        for f in files:
            self.AddErrors(f, [])
            self.RemoveUnusedBeams()
            if IsInclude(f):
                self.Compile(ErlangCache.GetDependentModules(f))
            self.ClearCacheForFile(f)
            editor = self.window.TabMgr.FindPageByPath(f)
            page = self.window.TabMgr.FindPageIndexByPath(f)

            if editor:
                dial = wx.MessageDialog(None,
                    'File "{}" was deleted.\nDo you want to close tab with deleted document?'.format(f),
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

    def ClearCacheForFile(self, path):
        name = os.path.basename(path)
        if IsModule(name):
            name = name[:-4]
        ErlangCache.UnloadModule(name, self.GetApp(path))

    def GetEditorTypes(self):
        return {".config": ErlangHighlightedSTCBase,
                ".src": ErlangHighlightedSTCBase,
                ".app": ErlangHighlightedSTCBase}

    def IsFlyCompileEnabled(self):
        return Config.GetProp("erlang_fly_compilation", True)

    def AddErrors(self, path, errors):
        self.errors[path] = errors
        editor = self.window.TabMgr.FindPageByPath(path)
        if editor and hasattr(editor, 'HighlightErrors'):
            editor.HighlightErrors(errors)
        self.errorsTable.AddErrors(path, errors)
        errorCount = 0
        warningCount = 0
        pathErrors = {}
        for (path, err) in self.errors.items():
            pathErrors[path] = False
            for e in err:
                if e.type == CompileErrorInfo.WARNING:
                    warningCount += 1
                else:
                    errorCount += 1
                    pathErrors[path] = True
        pathErrors = sorted(pathErrors.iteritems(), key = operator.itemgetter(1))
        self.explorer.SetPathErrors(pathErrors)
        self.window.TabMgr.HighlightErrorPaths(pathErrors)
        index = self.window.ToolMgr.FindPageIndexByWindow(self.errorsTable)
        self.errorCount = errorCount
        self.warningCount = warningCount
        if index != None:
            self.ShowErrorsTable()

    def GetErrors(self, path):
        if path not in self.errors: return []
        else: return self.errors[path]

    def CompileFileFly(self, fileName, realPath, data):
        if not self.IsSrcPath(realPath): return
        flyPath = os.path.join(self.flyDir, "fly_" + fileName)
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
        self.shellConsole.shell.SetProp(CONFIG_COMPILER_OPTIONS, options)
        if self.CompilerOptions() != self.CompilerOptions(self.oldProjectData):
            self.FullRebuild()

    def CheckBackgroundTasks(self, msg = "Compiling..."):
        if len(self.tasks) > 0:
            self.CreateProgressDialog(msg)

    def GetModulesInPath(self, path):
        result = set()
        for root, _, files in os.walk(path):
            for fileName in files:
                (f, ext) = os.path.splitext(fileName)
                if IsModule(fileName):
                    result.add(f)
        return result

    def GetBeamsInPath(self, path):
        result = set()
        for root, _, files in os.walk(path):
            for fileName in files:
                if IsBeam(fileName):
                    result.add(fileName)
        return result

    def ModuleName(self, path):
        return os.path.basename(path)[:-4]

    def CacheIgor(self):
        for f in self.explorer.GetAllFiles():
            if IsIgor(f): IgorCache.GenerateForFile(f)

    def GetXRefModules(self, withDeps=False):
        modules = []
        for app in self.GetApps():
            path = os.path.join(self.GetAppPath(app), "src")
            modules += self.GetModulesInPath(path)
        if withDeps:
            for dep in self.GetDeps():
                path = os.path.join(self.GetAppPath(dep), "src")
                modules += self.GetModulesInPath(path)
        return modules

    def GetExcludedPaths(self): return [] if not CONFIG_EXCLUDED_PATHS in self.projectData else self.projectData[CONFIG_EXCLUDED_PATHS]

    def IsSrcPath(self, path): raise Exception("override in child class")
    def GetBeamPathFromSrcPath(self, path): raise Exception("override in child class")
    def GetApp(self, path): raise Exception("override in child class")
    def FullRebuild(self): raise Exception("override in child class")
    def UpdatePaths(self): raise Exception("override in child class")
    def RemoveUnusedBeams(self): raise Exception("override in child class")
    def SetCompileMenuItems(self): raise Exception("override in child class")
    def RebuildTests(self): raise Exception("override in child class")
    def DialyzeProject(self): raise Exception("override in child class")
    def Rebuild(self): raise Exception("override in child class")
    def Recache(self): raise Exception("override in child class")

class SingleAppErlangProject(ErlangProject):
    def OnLoadProject(self):
        for d in [self.SrcDir(), self.IncludeDir(), self.DepsDir()]:
            if not os.path.isdir(d):
                os.mkdir(d)

        ErlangProject.OnLoadProject(self)
        self.explorer.ProjectDirsCreatedEvent += self.OnProjectDirsCreated

    def FullRebuild(self):
        self.Rebuild()
        self.RebuildDeps()

    def GetAppPath(self, app):
        if app == os.path.basename(self.projectDir):
            return self.projectDir
        dpath = os.path.join(self.DepsPath(), app)
        if os.path.isdir(dpath):
            return dpath
        return ""

    def EbinDirForApp(self, app):
        return os.path.join(self.GetAppPath(app), "ebin")

    def CompileTestSubset(self, apps):
        [self.GetShell().CompileTests(self.GetAppPath(app)) for app in apps]
        self.CheckBackgroundTasks("Compiling tests...")

    def CompileSubset(self, apps):
        [self.GetShell().CompileApp(self.GetAppPath(app)) for app in apps]
        self.CheckBackgroundTasks()

    def CacheSubset(self, apps):
        [self.GetShell().CacheApp(self.GetAppPath(app)) for app in apps]
        self.CheckBackgroundTasks("Caching...")

    def DialyzeApps(self, paths):
        if not self.CheckPlt(): return
        apps = set()
        for path in paths:
            app = self.GetApp(path)
            if not app: continue
            apps.add(self.EbinDirForApp(app))
        if len(apps) == 0:
            wx.MessageBox("No app to dialyze.", "Dialyzer")
            return
        self.GetShell().DialyzeApps(list(apps))

    def DialyzeProject(self):
        if not self.CheckPlt(): return
        apps = set()
        for app in self.GetApps():
            apps.add(self.EbinDirForApp(app))
        self.GetShell().DialyzeApps(list(apps))

    def RebuildTests(self):
        self.CompileTestSubset(self.GetAppsAndDeps())

    def Rebuild(self):
        self.CompileSubset(self.GetApps())
        self.CompileTestSubset(self.GetApps())

    def RebuildDeps(self):
        self.CompileSubset(self.GetDeps())

    def Recache(self):
        self.CacheSubset(self.GetApps(True))

    def RecacheDeps(self):
        self.CompileSubset(self.GetDeps(True))

    def RemoveUnusedBeams(self):
        modules = self.GetModulesInPath(self.SrcDir())

        beamPath = os.path.join(self.EbinDir())
        beams = self.GetBeamsInPath(beamPath)
        for beam in beams:
            (f, ext) = os.path.splitext(beam)
            path = os.path.join(beamPath, beam)
            if f not in modules and os.path.exists(path):
                os.remove(path)

    def IsSrcPath(self, path):
        app = self.GetApp(path)
        if app and self.GetAppPath(app):
            appPath = self.GetAppPath(app)
            return any([path.startswith(os.path.join(appPath, d)) for d in ["src", "include", "test"]])
        else:
            return False

    def DepsPath(self):
        return self.DepsDir()

    def OnProjectDirsCreated(self, dirs):
        for d in dirs:
            appPath = d
            (root, app) = os.path.split(appPath)
            if root == self.DepsPath():
                self.UpdatePaths()
                self.UpdateProjectConsoles()

    def GetXRefModules(self, withDeps = False):
        modules = []
        for app in self.GetApps():
            path = os.path.join(self.GetAppPath(app), "src")
            modules += self.GetModulesInPath(path)
        if withDeps:
            for dep in self.GetDeps():
                path = os.path.join(self.GetAppPath(dep), "src")
                modules += self.GetModulesInPath(path)
        return modules

    def GetApps(self, ignoreExcluded = False):
        return [os.path.basename(self.projectDir)]

    def GetDeps(self, ignoreExcluded = False):
        apps = []
        for app in os.listdir(self.DepsDir()):
            appPath = os.path.join(self.DepsPath(), app)
            if not os.path.isdir(appPath):
                continue
            apps.append(app)
        return apps

    def GetAppsAndDeps(self, ignoreExcluded = False):
        return list(set(self.GetApps(ignoreExcluded) + self.GetDeps(ignoreExcluded)))

    def GetBeamPathFromSrcPath(self, path):
        app = self.GetApp(path)
        if not app:
            return None
        return os.path.join(self.EbinDirForApp(app), os.path.splitext(os.path.basename(path))[0] + ".beam")

    def SetCompileMenuItems(self):
        self.window.projectMenu.AppendMenuItem("Rebuild", self.window, lambda e: self.Rebuild(), "F7")
        self.window.projectMenu.AppendMenuItem("Recache", self.window, lambda e: self.Recache(), "Shift+F7")

        self.window.projectMenu.AppendSeparator()
        self.window.projectMenu.AppendMenuItem("Rebuild deps", self.window, lambda e: self.RebuildDeps(), "F8")
        self.window.projectMenu.AppendMenuItem("Recache deps", self.window, lambda e: self.RecacheDeps(), "Shift+F8")

    def EbinDir(self): return os.path.join(self.projectDir, "ebin")
    def SrcDir(self): return os.path.join(self.projectDir, "src")
    def IncludeDir(self): return os.path.join(self.projectDir, "include")
    def TestDir(self): return os.path.join(self.projectDir, "test")
    def DepsDir(self): return os.path.join(self.projectDir, "deps")

    def OnSocketConnected(self):
        ErlangProject.OnSocketConnected(self)
        #self.Rebuild()

    def UpdatePaths(self):
        dirs = ""
        for app in self.GetAppsAndDeps(True):
            ebinDir = os.path.join(self.GetAppPath(app), "ebin")
            self.shellConsole.shell.AddPath(ebinDir)
            dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)
        self.dirs = dirs

    def GetApp(self, path):
        if os.path.isfile(path):
            path = os.path.dirname(path)
        if path.startswith(self.DepsPath()):
            app = path.replace(self.DepsPath() + os.sep, "")
        elif path.startswith(self.projectDir):
            return os.path.basename(self.projectDir)
        else:
            pos = path.find(os.sep + "lib" + os.sep)
            if pos > 0:
                path = path[pos + 5:]
                appFolder = path.split(os.sep)[0]
                return appFolder.split("-")[0]
            return None
        if os.sep in app:
            return app[:app.index(os.sep)]
        return app


class MultipleAppErlangProject(ErlangProject):
    def OnLoadProject(self):
        ErlangProject.OnLoadProject(self)
        self.explorer.ProjectDirsCreatedEvent += self.OnProjectDirsCreated

    def FullRebuild(self):
        self.Rebuild()
        self.RebuildDeps()

    def GetAppPath(self, app):
        path = os.path.join(self.AppsPath(), app)
        if os.path.isdir(path):
            return path
        dpath = os.path.join(self.DepsPath(), app)
        if os.path.isdir(dpath):
            return dpath
        return ""

    def EbinDirForApp(self, app):
        return os.path.join(self.GetAppPath(app), "ebin")

    def CompileTestSubset(self, apps):
        [self.GetShell().CompileTests(self.GetAppPath(app)) for app in apps if app not in self.projectData[CONFIG_EXCLUDED_DIRS]]
        self.CheckBackgroundTasks("Compiling tests...")

    def CompileSubset(self, apps):
        [self.GetShell().CompileApp(self.GetAppPath(app)) for app in apps if app not in self.projectData[CONFIG_EXCLUDED_DIRS]]
        self.CheckBackgroundTasks()

    def CacheSubset(self, apps):
        [self.GetShell().CacheApp(self.GetAppPath(app)) for app in apps]
        self.CheckBackgroundTasks("Caching...")

    def DialyzeApps(self, paths):
        if not self.CheckPlt(): return
        apps = set()
        for path in paths:
            app = self.GetApp(path)
            if not app: continue
            apps.add(self.EbinDirForApp(app))
        if len(apps) == 0:
            wx.MessageBox("No app to dialyze.", "Dialyzer")
            return
        self.GetShell().DialyzeApps(list(apps))

    def DialyzeProject(self):
        if not self.CheckPlt(): return
        apps = set()
        for app in self.GetApps():
            apps.add(self.EbinDirForApp(app))
        self.GetShell().DialyzeApps(list(apps))

    def RebuildTests(self):
        self.CompileTestSubset(self.GetAppsAndDeps())

    def Rebuild(self):
        self.CompileSubset(self.GetApps())
        self.CompileTestSubset(self.GetApps())

    def RebuildDeps(self):
        self.CompileSubset(self.GetDeps())

    def Recache(self):
        #ErlangCache.CleanDir(self.ProjectName())
        self.CacheSubset(self.GetApps(True))

    def RecacheDeps(self):
        self.CompileSubset(self.GetDeps(True))

    def RemoveUnusedBeams(self):
        for app in self.GetApps(True):
            srcPath = os.path.join(self.GetAppPath(app), "src")
            modules = self.GetModulesInPath(srcPath)

            beamPath = os.path.join(self.GetAppPath(app), "ebin")
            beams = self.GetBeamsInPath(beamPath)
            for beam in beams:
                (f, ext) = os.path.splitext(beam)
                path = os.path.join(beamPath, beam)
                if f not in modules and os.path.exists(path):
                    os.remove(path)

    def IsSrcPath(self, path):
        app = self.GetApp(path)
        if app and self.GetAppPath(app):
            appPath = self.GetAppPath(app)
            return any([path.startswith(os.path.join(appPath, d)) for d in ["src", "include", "test"]])
        else:
            return False

    def OnProjectDirsCreated(self, dirs):
        for d in dirs:
            appPath = d
            (root, app) = os.path.split(appPath)
            if root == self.AppsPath() or root == self.DepsPath():
                self.UpdatePaths()
                self.UpdateProjectConsoles()

    def GetApps(self, ignoreExcluded = False):
        apps = []
        for app in os.listdir(self.AppsPath()):
            if not ignoreExcluded and app in self.projectData[CONFIG_EXCLUDED_DIRS]:
                continue
            appPath = os.path.join(self.AppsPath(), app)
            if not os.path.isdir(appPath):
                continue
            apps.append(app)
        return apps

    def GetDeps(self, ignoreExcluded = False):
        apps = []
        for app in os.listdir(self.DepsPath()):
            if not ignoreExcluded and app in self.projectData[CONFIG_EXCLUDED_DIRS]:
                continue
            appPath = os.path.join(self.DepsPath(), app)
            if not os.path.isdir(appPath):
                continue
            apps.append(app)
        return apps

    def GetAppsAndDeps(self, ignoreExcluded = False):
        return list(set(self.GetApps(ignoreExcluded) + self.GetDeps(ignoreExcluded)))

    def GetBeamPathFromSrcPath(self, path):
        app = self.GetApp(path)
        if not app:
            return None
        return os.path.join(self.EbinDirForApp(app), os.path.splitext(os.path.basename(path))[0] + ".beam")

    def SetCompileMenuItems(self):
        self.window.projectMenu.AppendMenuItem("Rebuild apps", self.window, lambda e: self.Rebuild(), "F7")
        self.window.projectMenu.AppendMenuItem("Recache apps", self.window, lambda e: self.Recache(), "Shift+F7")
        if self.AppsPath() != self.DepsPath():
            self.window.projectMenu.AppendSeparator()
            self.window.projectMenu.AppendMenuItem("Rebuild deps", self.window, lambda e: self.RebuildDeps(), "F8")
            self.window.projectMenu.AppendMenuItem("Recache deps", self.window, lambda e: self.RecacheDeps(), "Shift+F8")

    def OnSocketConnected(self):
        ErlangProject.OnSocketConnected(self)
        #self.Rebuild()

    def UpdatePaths(self):
        dirs = ""
        for app in self.GetAppsAndDeps(True):
            ebinDir = os.path.join(self.GetAppPath(app), "ebin")
            self.shellConsole.shell.AddPath(ebinDir)
            dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)
        self.dirs = dirs

    def GetApp(self, path):
        if os.path.isfile(path):
            path = os.path.dirname(path)
        if path.startswith(self.AppsPath()):
            app = path.replace(self.AppsPath() + os.sep, "")
        elif path.startswith(self.DepsPath()):
            app = path.replace(self.DepsPath() + os.sep, "")
        else:
            pos = path.find(os.sep + "lib" + os.sep)
            if pos > 0:
                path = path[pos + 5:]
                appFolder = path.split(os.sep)[0]
                return appFolder.split("-")[0]
            return None
        if os.sep in app:
            return app[:app.index(os.sep)]
        return app

    def CanCompileModule(self, erl):
        app = self.GetApp(erl)
        return not app in self.projectData[CONFIG_EXCLUDED_DIRS]