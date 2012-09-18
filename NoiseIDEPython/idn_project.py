import time
import operator

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import yaml
import wx
from wx.lib.agw import aui
from wx.grid import PyGridTableBase
from TextCtrlAutoComplete import TextCtrlAutoComplete
from idn_cache import ErlangCache, readFile
from idn_connect import CompileErrorInfo, ErlangIDEConnectAPI
from idn_findreplace import FindInProjectDialog
from idn_utils import writeFile, CreateButton, CreateLabel, Menu
import idn_projectexplorer as exp
from idn_console import ErlangIDEConsole, ErlangProjectConsole
from idn_global import GetTabMgr, GetToolMgr, GetMainFrame, Log
from idn_customstc import ErlangHighlightedSTCBase
from PyProgress import PyProgress

class ProgressTaskManagerDialog(wx.EvtHandler):
    def __init__(self):
        wx.EvtHandler.__init__(self)
        self.progressDialog = None
        self.progressTimer = wx.Timer(self, wx.NewId())
        self.progressTimer.Start(250)
        self.Bind(wx.EVT_TIMER, self.OnProgressTimer, self.progressTimer)
        self.tasks = set()
        self.lastTaskTime = time.time()

        self.taskDoneHistory = []
        self.taskDoneHistoryString = ""

    def AddTask(self, task):
        #print "add task", task
        self.tasks.add(task)

    def AddTaskDescToHistory(self, description):
        self.taskDoneHistory.append(description)
        self.taskDoneHistory = self.taskDoneHistory[-2:]
        self.taskDoneHistoryString = "\n".join(self.taskDoneHistory)

    def UpdatePulse(self, description):
        if self.progressDialog:
            self.AddTaskDescToHistory(description)
            self.progressDialog.UpdatePulse(self.taskDoneHistoryString)

    def TaskDone(self, description, task = None):
        self.UpdatePulse(description)
        self.lastTaskTime = time.time()
        if task:
            if task in self.tasks:
                #print "done", task
                self.tasks.remove(task)
            #else:
            #    print "task not in tasks", task
        if len(self.tasks) == 0:
            self.DestroyDialog()

    def CreateProgressDialog(self, text = "IDE Activities"):
        if self.progressDialog:
            pass
        else:
            self.progressDialog = PyProgress(message = text,
                agwStyle = wx.PD_APP_MODAL | wx.PD_ELAPSED_TIME, style = wx.BORDER_NONE)
            self.progressDialog.SetGaugeProportion(0.1)
            self.progressDialog.SetGaugeSteps(70)
            self.progressDialog.SetGaugeBackground(wx.BLACK)
            self.progressDialog.SetFirstGradientColour(wx.RED)
            self.progressDialog.SetSecondGradientColour(wx.GREEN)
            self.progressDialog.SetSize((600, 110))
            self.lastTaskTime = time.time()
            self.progressDialog.ShowDialog()

    def OnProgressTimer(self, event):
        if self.progressDialog:
            if (time.time() - self.lastTaskTime) > 0.5:
                self.UpdatePulse("Tasks left: {}".format(len(self.tasks)))

            #if (time.time() - self.lastTaskTime > 10 and len(self.tasks) > 0):
                #Log("####\n 10 seconds from last task done. Tasks left ", len(self.tasks))
                #Log("\n\t".join([str(t) for t in self.tasks]))
            if (time.time() - self.lastTaskTime > 15 and len(self.tasks) > 0):
                #Log("####\n 15 seconds from last task done. Tasks left ", len(self.tasks))
                #Log("\n\t".join([str(t) for t in self.tasks]))
                Log("tasks left:", self.tasks)
                self.DestroyDialog()

    def DestroyDialog(self):
        if self.progressDialog:
            self.progressDialog.Destroy()
            self.progressDialog = None

class Project(ProgressTaskManagerDialog):

    EXPLORER_TYPE = exp.ProjectExplorer
    USER_DATA_FOLDER = os.path.join(os.getcwd(), 'userdata')

    CONFIG_LAST_OPENED_FILES = "last_opened_files"
    CONFIG_HIDDEN_PATHS = "hidden_paths"
    CONFIG_MASK = "mask"
    CONFIG_PROJECT_NAME = "project_name"
    CONFIG_PROJECT_TYPE = "project_type"
    CONFIG_TAB_PERSP = "tab_perspective"
    CONFIG_TOOL_PERSP = "tool_perspective"
    CONFIG_GLOBAL_PERSP = "global_perspective"


    def __init__(self, window, filePath, projectData):
        ProgressTaskManagerDialog.__init__(self)
        self.window = window
        window.project = self
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData

        if not os.path.isdir(self.USER_DATA_FOLDER):
            os.makedirs(self.USER_DATA_FOLDER)

        self.userDataFile = os.path.join(self.USER_DATA_FOLDER, "{}.project.user".format(self.ProjectName()))
        if os.path.isfile(self.userDataFile):
            self.userData = yaml.load(open(self.userDataFile))
        else:
            self.userData = {}

        self.CreateExplorer()
        self.OnLoadProject()
        self.OpenLastFiles()

        self.menu = Menu()
        self.SetupMenu()
        self.menuPos = window.MenuBar().GetMenuCount() - 1
        self.window.MenuBar().Insert(self.menuPos, self.menu, "&Project")
        self.SetupPerspective()

    def SetupMenu(self):
        pass

    def ProjectName(self):
        return self.projectData[self.CONFIG_PROJECT_NAME]

    def LastOpenedFiles(self):
        if self.CONFIG_LAST_OPENED_FILES in self.userData:
            return self.userData[self.CONFIG_LAST_OPENED_FILES]
        else:
            return []

    def HiddenPathsList(self):
        if self.CONFIG_HIDDEN_PATHS in self.userData:
            return set(self.userData[self.CONFIG_HIDDEN_PATHS])
        else:
            return set()

    def OpenLastFiles(self):
        removedFiles = []
        for file in self.LastOpenedFiles():
            if not os.path.isfile(file):
                removedFiles.append(file)
            else:
                GetTabMgr().LoadFileLine(file)
        if removedFiles:
            self.userData[self.CONFIG_LAST_OPENED_FILES] = \
                [file for file in self.userData[self.CONFIG_LAST_OPENED_FILES] if file not in removedFiles]

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorer = self.EXPLORER_TYPE(self.window, self)
        self.explorer.SetRoot(self.projectDir)
        self.explorer.SetHiddenList(self.HiddenPathsList())
        self.window.WinMgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Project Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600).MinSize(100, 100)
            .MinimizeMode(aui.AUI_MINIMIZE_POS_LEFT | aui.AUI_MINIMIZE_CAPT_SMART))
        self.window.WinMgr.Update()
        print "create explorer"

    def GetMask(self):
        if self.CONFIG_MASK in self.userData:
            return self.userData[self.CONFIG_MASK]
        return None

    def Close(self):
        self.SaveUserData()
        self.explorer.StopTrackingProject()
        self.window.WinMgr.DetachPane(self.explorer)
        self.window.WinMgr.Update()
        self.explorer.Destroy()
        GetTabMgr().CloseAll()
        self.window.MenuBar().Remove(self.menuPos)

    def SaveUserData(self):
        #print "save user data"
        openedFiles = []
        for path in GetTabMgr().OpenedFiles():
            if path.lower().startswith(self.projectDir.lower()):
                openedFiles.append(path)
        self.userData[self.CONFIG_LAST_OPENED_FILES] = openedFiles
        self.userData[self.CONFIG_HIDDEN_PATHS] = self.explorer.hiddenPaths
        self.userData[self.CONFIG_MASK] = self.explorer.GetCustomMask()

        #self.userData[self.CONFIG_TAB_PERSP] = GetTabMgr().SavePerspective()
        #self.userData[self.CONFIG_TOOL_PERSP] = GetToolMgr().SavePerspective()
        #self.userData[self.CONFIG_GLOBAL_PERSP] = GetWinMgr().SavePerspective()

        #print(self.userData[self.CONFIG_MASK])
        #print(self.userData[self.CONFIG_HIDDEN_PATHS])
        yaml.dump(self.userData, open(self.userDataFile, 'w'))

    def GetEditorTypes(self): return []

    def SetupPerspective(self):
        pass
        #if self.CONFIG_TAB_PERSP in self.userData:
         #   GetTabMgr().LoadPerspective(self.userData[self.CONFIG_TAB_PERSP])
        #    GetTabMgr().Update()
        #if self.CONFIG_TOOL_PERSP in self.userData:
        #    GetToolMgr().LoadPerspective(self.userData[self.CONFIG_TOOL_PERSP])
        #    GetToolMgr().Update()
#        if self.CONFIG_GLOBAL_PERSP in self.userData:
#            GetWinMgr().LoadPerspective(self.userData[self.CONFIG_GLOBAL_PERSP])
#            GetWinMgr().Update()

    def SaveData(self):
        stream = file(self.projectFilePath, 'w')
        yaml.dump(self.projectData, stream)


class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules', 'noiseide', 'ebin')
    EXPLORER_TYPE = exp.ErlangProjectExplorer

    CONFIG_ERLANG_PATH = "erlang_path"
    CONFIG_EXCLUDED_DIRS = "excluded_dirs"
    CONFIG_FLY_COMPILE = "fly_compile"
    CONFIG_APPS_DIR = "apps_dir"

    CONFIG_CONSOLES = "consoles"
    CONFIG_CONSOLE_SNAME = "sname"
    CONFIG_CONSOLE_COOKIE = "cookie"
    CONFIG_CONSOLE_PARAMS = "params"
    CONFIG_CONSOLE_COMMAND = "command"

    def OnLoadProject(self):
        path = self.GetErlangPath()
        if not os.path.isfile(path):
            dlg = wx.FileDialog(self.window, "Please select valid Erlang path.")
            if dlg.ShowModal() == wx.ID_OK:
                self.projectData[self.CONFIG_ERLANG_PATH] = dlg.GetPath()
                self.SaveData()

        self.errors = {}
        self.consoleTabs = {}

        ErlangCache.Init(self)

        self.SetupDirs()
        self.AddTabs()
        self.AddConsoles()
        self.GenerateErlangCache() #test

        self.CompileProject() #test

        self.explorer.Bind(exp.EVT_PROJECT_FILE_CREATED, self.OnProjectFileCreated)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_DELETED, self.OnProjectFileDeleted)
        self.explorer.Bind(exp.EVT_PROJECT_DIR_CREATED, self.OnProjectDirCreated)


        GetTabMgr().Parent.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

        #ErlangCache.LoadCacheFromDir(self.ProjectName())
        #ErlangCache.StartCheckingFolder(self.ProjectName())

    #def TaskDone(self, description, task = None):
    #    Project.TaskDone(self, description, task)

    def ErlangCacheChecked(self):
        ErlangCache.LoadCacheFromDir("erlang")

    def SetupMenu(self):
        self.mEditProject = self.menu.AppendMenuItem('Edit Project', self.window, self.OnEditProject)
        self.mEditProject.Enable(False)

        self.menu.AppendSeparator()

        self.menu.AppendMenuItem("Generate erlang cache", self.window, lambda e: self.GenerateErlangCache())
        self.menu.AppendMenuItem("Rebuild project", self.window, lambda e: self.CompileProject())

    def OnEditProject(self, event):
        ErlangProjectFrom(self).ShowModal()

    def AppsPath(self):
        if not self.CONFIG_APPS_DIR in self.projectData or not self.projectData[self.CONFIG_APPS_DIR]:
            return self.projectDir
        return os.path.join(self.projectDir, self.projectData[self.CONFIG_APPS_DIR])

    def GetErlangPath(self):
        return self.projectData[self.CONFIG_ERLANG_PATH]

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

    def GenerateErlangCache(self):
        print "generate erlang cache"
        self.GetShell().GenerateErlangCache()

    def GetShell(self):
        return self.shellConsole.shell

    def Compile(self, path):
        #print path
        if path.endswith(".hrl"):
            self.GetShell().GenerateFileCache(path)
            [self.Compile(module) for module in ErlangCache.GetDependentModules(path)]
        elif path.endswith(".yrl"):
            self.GetShell().CompileYrls([path])
        else:
            #print self.AppsPath()
            if not path.lower().startswith(self.AppsPath().lower()):
                return
            app = path.lower().replace(self.AppsPath().lower() + os.sep, "")
            app = app[:app.index(os.sep)]
            #print app
            if app in self.projectData[self.CONFIG_EXCLUDED_DIRS]:
                return
            self.GetShell().CompileProjectFile(path, app)

    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(GetToolMgr(), self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", os.path.normcase(ErlangCache.CACHE_DIR))
        self.shellConsole.shell.SetProp("project_dir", os.path.normcase(self.AppsPath()))
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        GetToolMgr().AddPage(self.shellConsole, "IDE Console")

        self.UpdatePaths()

        self.UpdateProjectConsoles()

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

        consoles = self.projectData[self.CONFIG_CONSOLES]

        for console in self.consoleTabs:
            if console not in consoles:
                c = self.consoleTabs[console]
                c.Stop()
                index = GetToolMgr().FindPageIndexByWindow(c)
                GetToolMgr().DeletePage(index)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data[self.CONFIG_CONSOLE_SNAME])
            params.append("-setcookie " + data[self.CONFIG_CONSOLE_COOKIE])
            params.append(data[self.CONFIG_CONSOLE_PARAMS])

            params.append("-pa " + self.dirs)

            if title in self.consoleTabs:
                self.consoleTabs[title].SetParams(params)
            else:
                self.consoles[title] = ErlangProjectConsole(GetToolMgr(), self.AppsPath(), params)
                self.consoles[title].SetStartCommand(data[self.CONFIG_CONSOLE_COMMAND])
                GetToolMgr().AddPage(self.consoles[title], '<{}> Console'.format(title))
                self.consoleTabs[title] = self.consoles[title]

    def GetApps(self):
        apps = []
        for app in os.listdir(self.AppsPath()):
            if app in self.projectData[self.CONFIG_EXCLUDED_DIRS]:
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
            if editor.saved == False and unicode(editor.savedText) != unicode(text):
                dial = wx.MessageDialog(None,
                    'File "{}" was modified.\nDo you want to reload document?'.format(file),
                    'File modified',
                    wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                if dial.ShowModal() == wx.ID_YES:
                    wx.CallAfter(editor.LoadFile, file)
        #else:
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
        return self.CONFIG_FLY_COMPILE in self.projectData and self.projectData[self.CONFIG_FLY_COMPILE]

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

    def OnKeyDown(self, event):
        if event.GetKeyCode() == ord('O') and event.ControlDown():
            dialog = FastProjectFileOpenDialog(GetTabMgr(), self)
            dialog.ShowModal()
        elif event.GetKeyCode() == ord('F') and event.ControlDown() and event.ShiftDown():
            dialog = FindInProjectDialog.GetDialog(GetTabMgr())
            dialog.Show()
            dialog.findText.SetFocus()
        else:
            event.Skip()

    def CompileFileFly(self, file, realPath, data):
        flyPath = os.path.join(self.flyDir, "fly_" + file)
        writeFile(flyPath, data)
        self.GetShell().CompileFileFly(realPath, flyPath)

    def UpdateProject(self):
        self.UpdatePaths()
        self.UpdateProjectConsoles()

class FastProjectFileOpenDialog(wx.Dialog):
    def __init__(self, parent, project):
        wx.Dialog.__init__(self, parent, size = (600, 55))
        choices = self.PrepareChoices(project)
        self.cb = TextCtrlAutoComplete(
            self,
            colNames = ["File", "Path"],
            colFetch = 1,
            multiChoices = choices,
            selectCallback = self.OnSelectCallback)
        self.cb.dropdownlistbox.SetColumnWidth(0, 150)
        self.cb.dropdownlistbox.SetColumnWidth(1, 450)
        sizer = wx.BoxSizer()
        sizer.Add(self.cb, 1, wx.EXPAND)
        self.SetSizer(sizer)
        self.Layout()
        self.CenterOnParent()
        self.cb.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.cb.SetFocus()

    def PrepareChoices(self, project):
        files = project.explorer.GetAllFiles()
        result = []
        for file in files:
            result.append((os.path.basename(file), file))
        return result

    def OnKeyDown(self, event):
        if event.GetKeyCode() == wx.WXK_ESCAPE:
            self.Close()
        elif event.GetKeyCode() == wx.WXK_RETURN:
            value = self.cb.GetValue()
            if os.path.isfile(value):
                self.Close()
                GetTabMgr().LoadFileLine(value)
            else:
                event.Skip()
        else:
            event.Skip()

    def OnSelectCallback(self, values):
        self.Close()
        GetTabMgr().LoadFileLine(values[1])


class ErrorsTable(PyGridTableBase):
    def __init__(self, data):
        PyGridTableBase.__init__(self)
        self.data = data
        self.colLabels = ["File", "Line", "Type", "Message"]

    def GetNumberRows(self):
        return len(self.data)

    def GetNumberCols(self):
         return len(self.colLabels)

    def IsEmptyCell(self, row, col):
        try:
            return not self.data[row][col]
        except IndexError:
            return True

    def GetValue(self, row, col):
        try:
            return self.data[row][col]
        except IndexError:
            return ''

    def GetTypeName(self, row, col):
        if col == 1:
            return wx.grid.GRID_VALUE_NUMBER
        else:
            return wx.grid.GRID_VALUE_STRING

    def GetColLabelValue(self, col):
         return self.colLabels[col]

    def SetValue(self, row, col, value):
        self.data[row][col] = value

    def ResetView(self, grid, currentRows):
        """
        (Grid) -> Reset the grid view.   Call this to
        update the grid if rows and columns have been added or deleted
        """
        grid.BeginBatch()

        for current, new, delmsg, addmsg in [
            (currentRows, self.GetNumberRows(), wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED, wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED)
        ]:

            if new < current:
                msg = wx.grid.GridTableMessage(self,delmsg,new,current-new)
                grid.ProcessTableMessage(msg)
            elif new > current:
                msg = wx.grid.GridTableMessage(self,addmsg,new-current)
                grid.ProcessTableMessage(msg)
            self.UpdateValues(grid)

        grid.EndBatch()

        # update the scrollbars and the displayed part of the grid
        grid.AdjustScrollbars()
        grid.ForceRefresh()


    def UpdateValues(self, grid):
        """Update all displayed values"""
        # This sends an event to the grid table to update all of the values
        msg = wx.grid.GridTableMessage(self, wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        grid.ProcessTableMessage(msg)

class ErrorsTableGrid(wx.grid.Grid):
    def __init__(self, parent, project):
        wx.grid.Grid.__init__(self, parent, -1)
        self.project = project
        self.table = ErrorsTable([])
        self.SetTable(self.table, True)
        self.AutoSizeColumns(False)
        self.SetRowLabelSize(0)
        self.SetMargins(0,0)
        self.SetColSize(0, 450)
        self.SetColSize(1, 50)
        self.SetColSize(2, 100)
        self.SetColSize(3, 750)
        self.SetColMinimalAcceptableWidth(50)

        self.DisableCellEditControl()
        self.DisableDragCell()
        self.DisableDragColMove()
        self.DisableDragColSize()
        self.DisableDragGridSize()
        self.DisableDragRowSize()
        self.Bind(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self.OnLeftDClick)

    def OnLeftDClick(self, event):
        row = event.GetRow()
        rowData = self.table.data[row]
        file = os.path.join(self.project.AppsPath(), rowData[0])
        line = rowData[1]
        GetTabMgr().LoadFileLine(file, line)

    def AddErrors(self, path, errors):
        currentRows = len(self.table.data)
        newPath = path.replace(self.project.AppsPath() + os.sep, "")
        data = list(filter(lambda x: x[0] != newPath, self.table.data))
        for e in errors:
            data.append((newPath, e.line + 1, e.TypeToStr(), e.msg))
        data = sorted(data, key = operator.itemgetter(2, 0))
        self.table.data = data
        self.table.ResetView(self, currentRows)


class ErlangProjectFrom(wx.Dialog):
    def __init__(self, project = None):
        wx.Dialog.__init__(self, GetMainFrame(), size = (390, 510), title = "Create\Edit project",
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.consoles = {}

        self.CreateForm()

        self.project = project
        if self.project:
            self.SetCurrentValues()

    def CreateForm(self):
        self.projectNameTB = wx.TextCtrl(self, value = "Project_name", size = (270, 20), validator = NotEmptyTextValidator("Title"))
        self.projectNameTB.SetToolTipString("Project name")

        self.projectPathTB = wx.TextCtrl(self, value = "C:\\YourProjectFolder", size = (270, 20), validator = NotEmptyTextValidator("Project dir"))
        self.projectPathTB.SetToolTipString("Path to folder")
        self.projectPathTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.projectPathButton = CreateButton(self, "...", self.OnSelectProjectPath)
        self.projectPathButton.MinSize = (25, 25)

        self.appsDirTB = wx.TextCtrl(self, value = "apps", size = (270, 20))
        self.appsDirTB.SetToolTipString("Apps folder name")
        self.appsDirTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.flyCB = wx.CheckBox(self, label = "Fly compilation")
        self.flyCB.SetValue(True)
        self.flyCB.SetToolTipString("Enable fly compilation. Highlights errors while you enter code")

        self.excludedDirList = wx.CheckListBox(self, choices = [], size = (220, 150))
        self.excludedDirList.SetToolTipString("Directories to exclude from compilation")

        self.erlangPathTB = wx.TextCtrl(self, value = "C:\\Programming\\erl5.9\\bin\\erl.exe", size = (270, 20), validator = PathExistsValidator("Erlang path"))
        self.erlangPathTB.SetToolTipString("Path to erlang executeable. Example: 'C:\\Programming\\erl5.9\\bin\\erl.exe'")

        self.erlangPathButton = CreateButton(self, "...", self.OnSelectErlangPath)
        self.erlangPathButton.MinSize = (25, 25)

        self.consolesList = wx.ListBox(self, size = (200, 70))
        self.consolesList.SetToolTipString("Consoles list for project. Each console has own params and can be started independent")

        self.addConsoleButton = CreateButton(self, "Add", self.OnAddConsole)
        self.editConsoleButton = CreateButton(self, "Edit", self.OnEditConsole)
        self.removeConsoleButton = CreateButton(self, "Remove", self.OnRemoveConsole)

        self.closeButton = CreateButton(self, "Close", lambda e: self.Close())
        self.saveButton = CreateButton(self, "Save", self.OnSave)

        sizer = wx.BoxSizer(wx.VERTICAL)
        gSizer = wx.GridBagSizer(2, 2)

        gSizer.Add(CreateLabel(self, "Title:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectNameTB, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Project dir:"), (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectPathTB, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.projectPathButton, (1, 2), flag = wx.ALIGN_CENTER)

        gSizer.Add(CreateLabel(self, "Apps dir:"), (2, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.appsDirTB, (2, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Erlang path:"), (3, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.erlangPathTB, (3, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.erlangPathButton, (3, 2), flag = wx.ALIGN_CENTER)

        gSizer.Add(self.flyCB, (4, 1), flag = wx.ALL | wx.ALIGN_LEFT, border = 4)

        sizer.AddSizer(gSizer)

        excludedAppsSizer = wx.StaticBoxSizer(wx.StaticBox(self, label = "Excluded dirs"), wx.HORIZONTAL)

        excludedAppsSizer.Add(self.excludedDirList, 1, flag = wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border = 4)
        sizer.AddSizer(excludedAppsSizer, flag = wx.EXPAND)

        cSizerH = wx.StaticBoxSizer(wx.StaticBox(self, label = "Consoles"), wx.HORIZONTAL)

        bSizer = wx.BoxSizer(wx.VERTICAL)
        bSizer.Add(self.addConsoleButton)
        bSizer.Add(self.editConsoleButton)
        bSizer.Add(self.removeConsoleButton)

        cSizerH.Add(self.consolesList, 1, flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        cSizerH.AddSizer(bSizer, flag = wx.ALL | wx.ALIGN_CENTER, border = 4)

        sizer.AddSizer(cSizerH, flag =  wx.EXPAND)

        bSizerH = wx.BoxSizer(wx.HORIZONTAL)
        bSizerH.Add(self.closeButton, flag = wx.ALL | wx.ALIGN_LEFT, border = 4)
        bSizerH.AddStretchSpacer()
        bSizerH.Add(self.saveButton, flag = wx.ALL | wx.ALIGN_RIGHT, border = 4)

        sizer.AddSizer(bSizerH, 1, flag = wx.EXPAND)

        self.SetSizer(sizer)
        self.Layout()

    def SetCurrentValues(self):
        self.projectNameTB.Value = self.project.projectData[Project.CONFIG_PROJECT_NAME]
        self.projectNameTB.Disable()
        self.projectPathTB.Value = self.project.projectDir
        self.projectPathTB.Disable()
        self.projectPathButton.Disable()
        self.appsDirTB.Value = self.project.projectData[ErlangProject.CONFIG_APPS_DIR]
        self.erlangPathTB.Value = self.project.projectData[ErlangProject.CONFIG_ERLANG_PATH]
        self.flyCB.Value = self.project.projectData[ErlangProject.CONFIG_FLY_COMPILE]

        self.excludedDirList.SetItems(self.project.projectData[ErlangProject.CONFIG_EXCLUDED_DIRS] + self.project.GetApps())
        self.excludedDirList.SetCheckedStrings(self.project.projectData[ErlangProject.CONFIG_EXCLUDED_DIRS])

        self.consoles = self.project.projectData[ErlangProject.CONFIG_CONSOLES]
        self.UpdateConsoles()


    def OnPathChanged(self, event):
        dir = self.projectPathTB.Value
        if dir and os.path.isdir(dir):
            apps = self.appsDirTB.Value
            appsDir = os.path.join(dir, apps)
            if apps and os.path.isdir(appsDir):
                dir = appsDir

            allDirs = [d for d in os.listdir(dir) if os.path.isdir(os.path.join(dir, d))]
            self.excludedDirList.SetItems(allDirs)


    def OnSelectErlangPath(self, event):
        dlg = wx.FileDialog(self, defaultFile = self.erlangPathTB.Value)
        if dlg.ShowModal() == wx.ID_OK:
            self.erlangPathTB.Value = dlg.GetPath()

    def OnSelectProjectPath(self, event):
        dlg = wx.DirDialog(self, defaultPath = self.projectPathTB.Value)
        if dlg.ShowModal() == wx.ID_OK:
            self.projectPathTB.Value = dlg.GetPath()

    def UpdateConsoles(self):
        self.consolesList.Clear()
        for console in self.consoles:
            self.consolesList.Append(console)

    def OnAddConsole(self, event):
        dlg = ConsoleCreateEditDialog(self)
        dlg.ShowModal()
        self.UpdateConsoles()

    def OnRemoveConsole(self, event):
        del self.consoles[self.consolesList.GetStringSelection()]
        self.UpdateConsoles()

    def OnEditConsole(self, event):
        dlg = ConsoleCreateEditDialog(self, self.consolesList.GetStringSelection())
        dlg.ShowModal()
        self.UpdateConsoles()

    def OnSave(self, event):

        if not self.Validate(): return

        title = self.projectNameTB.Value
        path = self.projectPathTB.Value
        apps = self.appsDirTB.Value
        erlang = self.erlangPathTB.Value
        flyCB = self.flyCB.Value
        excludedDirs = list(self.excludedDirList.GetCheckedStrings())

        data = {}
        data[Project.CONFIG_PROJECT_NAME] = title
        data[Project.CONFIG_PROJECT_TYPE] = "erlang"
        data[ErlangProject.CONFIG_APPS_DIR] = apps
        data[ErlangProject.CONFIG_ERLANG_PATH] = erlang
        data[ErlangProject.CONFIG_FLY_COMPILE] = flyCB
        data[ErlangProject.CONFIG_EXCLUDED_DIRS] = excludedDirs

        data[ErlangProject.CONFIG_CONSOLES] = self.consoles

        pFile = os.path.join(path, title + ".noiseide.project")
        if not os.path.isdir(path):
            os.makedirs(path)
        if apps:
            appsPath = os.path.join(path, apps)
            if not os.path.isdir(appsPath):
                os.mkdir(appsPath)

        stream = file(pFile, 'w')
        yaml.dump(data, stream)

        if self.project:
            self.project.projectData = data
            wx.CallAfter(self.project.UpdateProject)
        else:
            wx.CallAfter(GetMainFrame().OpenProject, pFile)

        self.Close()

class ConsoleCreateEditDialog(wx.Dialog):
    def __init__(self, parent, console = None):
        wx.Dialog.__init__(self, parent, title = "Console props",
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.currentConsole = console

        self.titleTB = wx.TextCtrl(self, value = "New console", size = (250, 20), validator = NotEmptyTextValidator("Title"))
        self.snameTB = wx.TextCtrl(self, value = "sname", size = (250, 20), validator = NotEmptyTextValidator("SName"))
        self.cookieTB = wx.TextCtrl(self, value = "123", size = (250, 20), validator = NotEmptyTextValidator("Cookie"))
        self.paramsTB = wx.TextCtrl(self, value = "", size = (250, 20))
        self.commandTB = wx.TextCtrl(self, value = "", size = (250, 20))

        self.saveB = CreateButton(self, "Save", self.OnSave)
        self.cancelB = CreateButton(self, "Cancel", lambda e: self.Close())

        if console:
            data = self.Parent.consoles[console]
            self.titleTB.SetValue(console)
            self.snameTB.SetValue(data[ErlangProject.CONFIG_CONSOLE_SNAME])
            self.cookieTB.SetValue(data[ErlangProject.CONFIG_CONSOLE_COOKIE])
            self.paramsTB.SetValue(data[ErlangProject.CONFIG_CONSOLE_PARAMS])
            self.commandTB.SetValue(data[ErlangProject.CONFIG_CONSOLE_COMMAND])

        gSizer = wx.GridBagSizer(2, 2)

        gSizer.Add(CreateLabel(self, "Title:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.titleTB, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "SName:"), (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.snameTB, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Cookie:"), (2, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.cookieTB, (2, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Params:"), (3, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.paramsTB, (3, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Start command:"), (4, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.commandTB, (4, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(self.cancelB, (5, 0), flag = wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border = 4)
        gSizer.Add(self.saveB, (5, 1), flag = wx.ALL | wx.ALIGN_RIGHT | wx.EXPAND, border = 4)

        self.SetSizer(gSizer)
        self.Layout()

    def OnSave(self, event):

        if not self.Validate(): return

        title = self.titleTB.Value
        sname = self.snameTB.Value
        cookie = self.cookieTB.Value
        params = self.paramsTB.Value
        command = self.commandTB.Value

        if (title and self.currentConsole
            and self.currentConsole != title
            and not title in self.Parent.consoles ):
            del self.Parent.consoles[self.currentConsole]

        data = {}
        data[ErlangProject.CONFIG_CONSOLE_SNAME] = sname
        data[ErlangProject.CONFIG_CONSOLE_COOKIE] = cookie
        data[ErlangProject.CONFIG_CONSOLE_PARAMS] = params
        data[ErlangProject.CONFIG_CONSOLE_COMMAND] = command

        self.Parent.consoles[title] = data
        self.Close()


class BasicValidator(wx.PyValidator):
    def __init__(self, title):
        wx.PyValidator.__init__(self)
        self.title = title

    def TransferToWindow(self):
        return True

    def TransferFromWindow(self):
        return True

    def Clone(self):
        return type(self)(self.title)

    def Error(self):
        wnd = self.GetWindow()
        wnd.SetBackgroundColour("pink")
        wnd.SetFocus()
        wnd.Refresh()
        return False

    def Ok(self):
        wnd = self.GetWindow()
        wnd.SetBackgroundColour(
            wx.SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW))
        wnd.Refresh()
        return True

class NotEmptyTextValidator(BasicValidator):
    def Validate(self, win):
        textCtrl = self.GetWindow()
        text = textCtrl.GetValue()

        if len(text) == 0:
            wx.MessageBox(self.title + " must be not empty", "Error")
            return self.Error()
        else:
            return self.Ok()

class PathExistsValidator(BasicValidator):
    def __init__(self, title, isDir = False):
        BasicValidator.__init__(self, title)
        self.isDir = isDir

    def Validate(self, win):
        textCtrl = self.GetWindow()
        text = textCtrl.GetValue()

        if self.isDir:
            result = os.path.isdir(text)
        else:
            result = os.path.isfile(text)

        if not result:
            wx.MessageBox(self.title + " must be existing " + ("dir" if self.isDir else "file"), "Error")
            return self.Error()
        else:
            return self.Ok()

def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData[Project.CONFIG_PROJECT_TYPE]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)