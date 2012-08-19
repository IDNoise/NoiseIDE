from wx.grid import PyGridTableBase
from TextCtrlAutoComplete import TextCtrlAutoComplete
from idn_cache import ErlangCache, readFile
from idn_directoryinfo import DirectoryChecker
from idn_findreplace import FindInFileDialog, FindInProjectDialog

__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import wx
from wx.lib.masked import combobox
from wx import combo
import time
import idn_projectexplorer as exp
from wx.lib.agw import aui
from idn_console import ErlangIDEConsole, ErlangProjectConsole
from idn_global import GetTabMgr, GetToolMgr, GetMainFrame

class Project:
    EXPLORER_TYPE = exp.ProjectExplorer
    USER_DATA_FOLDER = os.path.join(os.getcwd(), 'userdata')

    CONFIG_LAST_OPENED_FILES = "last_opened_files"
    CONFIG_HIDDEN_PATHS = "hidden_paths"

    def __init__(self, window, filePath, projectData):
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

    def ProjectName(self):
        return self.projectData["project_name"]

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
        for file in self.LastOpenedFiles():
            GetTabMgr().LoadFile(file)

    def AppsPath(self):
        return os.path.join(self.projectDir, self.projectData["apps_dir"])

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorer = self.EXPLORER_TYPE(self.window)
        self.explorer.SetRoot(self.projectDir)
        self.explorer.SetHiddenList(self.HiddenPathsList())
        self.window.WinMgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Project Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600))
        self.window.WinMgr.Update()

    def Close(self):
        self.explorer.StopTrackingProject()
        self.SaveUserData()

    def SaveUserData(self):
        #print "save user data"
        openedFiles = []
        for path in GetTabMgr().OpenedFiles():
            if path.lower().startswith(self.projectDir.lower()):
                openedFiles.append(path)
        self.userData[self.CONFIG_LAST_OPENED_FILES] = openedFiles
        self.userData[self.CONFIG_HIDDEN_PATHS] = self.explorer.hiddenPaths
        #print(self.userData[self.CONFIG_LAST_OPENED_FILES])
        #print(self.userData[self.CONFIG_HIDDEN_PATHS])
        yaml.dump(self.userData, open(self.userDataFile, 'w'))


class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules')
    EXPLORER_TYPE = exp.ErlangProjectExplorer


    def OnLoadProject(self):
        self.errors = {}

        ErlangCache.Init()
        self.SetupDirs()
        self.AddConsoles()
        self.AddTabs()
        self.GenerateErlangCache() #test

        self.CompileProject() #test

        self.explorer.Bind(exp.EVT_PROJECT_FILE_CREATED, self.OnProjectFileCreated)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)
        self.explorer.Bind(exp.EVT_PROJECT_FILE_DELETED, self.OnProjectFileDeleted)
        ErlangCache.LoadCacheFromDir("erlang")
        ErlangCache.LoadCacheFromDir(self.ProjectName())
        ErlangCache.StartCheckingFolder(self.ProjectName())

        GetTabMgr().Parent.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def SetupDirs(self):
        projectCacheDir = os.path.join(ErlangCache.CACHE_DIR, self.ProjectName())
        if not os.path.isdir(projectCacheDir):
            os.makedirs(projectCacheDir)
        flyDir = os.path.join(GetMainFrame().cwd, "data", "erlang", "fly")
        if not os.path.isdir(flyDir):
            os.makedirs(flyDir)

    def GenerateErlangCache(self):
        self.shellConsole.shell.GenerateErlangCache()

    def GetShell(self):
        return self.shellConsole.shell

    def Compile(self, path):
        if path.endswith(".hrl"):
            self.GetShell().GenerateFileCache(path)

        else:
            if not path.lower().startswith(self.AppsPath().lower()):
                return
            app = path.replace(self.AppsPath() + os.sep, "")
            app = app[:app.index(os.sep)]
            #print app, self.projectData["apps"]
            if not app in self.projectData["apps"]:
                return
            self.GetShell().CompileProjectFile(path, app)

    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(GetToolMgr(), self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", ErlangCache.CACHE_DIR)
        self.shellConsole.shell.SetProp("project_dir", self.AppsPath())
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        GetToolMgr().AddPage(self.shellConsole, "IDE Console")

        self.consoles = {}
        consoles = self.projectData["consoles"]

        dirs = ""
        for app in self.projectData["apps"]:
            appPath = os.path.join(self.AppsPath(), app)
            if os.path.isdir(appPath):
                ebinDir = os.path.join(appPath, "ebin")
                self.shellConsole.shell.AddPath(ebinDir)
                dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data["sname"])
            params.append("-cookie " + data["cookie"])
            params.append("-config " + data["config"])

            params.append("-pa " + dirs)
            self.consoles[title] = ErlangProjectConsole(GetToolMgr(), self.AppsPath(), params)
            self.consoles[title].SetStartCommand(data["command"])
            GetToolMgr().AddPage(self.consoles[title], '<{}> Console'.format(title))

    def AddTabs(self):
        self.errorsTable = ErrorsTableGrid(GetToolMgr())
        GetToolMgr().AddPage(self.errorsTable, 'Errors')

    def Close(self):
        ErlangCache.StopCheckingFolder(self.ProjectName())
        Project.Close(self)
        self.shellConsole.Stop()
        for title, console in self.consoles.items():
            console.Stop()

    def OnProjectFileModified(self, event):
        file = event.File
        editor = GetTabMgr().FindPageByPath(file)
        if editor:
            if editor.GetText() != readFile(file):
                dial = wx.MessageDialog(None,
                    'File "{}" was modified.\nDo you want to reload document?'.format(file),
                    'File modified',
                    wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
                if dial.ShowModal() == wx.YES:
                    editor.LoadFile(page.filePath)
                    editor.saved = False
                    editor.changed = True
        else:
            self.Compile(file)
        event.Skip()

    def FileSaved(self, file):
        self.Compile(file)
        if file.endswith(".hrl"):
            [self.Compile(module) for module in ErlangCache.GetDependentModules(file)]

    def OnProjectFileDeleted(self, event):
        file = event.File
        self.RemoveUnusedBeams()
        page = GetTabMgr().FindPageIndexByPath(file)
        if page >= 0:
            dial = wx.MessageDialog(None,
                'File "{}" was deleted.\nDo you want to close tab with deleted document?'.format(file),
                'File deleted',
                wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
            result = dial.ShowModal()
            if result == wx.ID_YES:
                GetTabMgr().ClosePage(page)
        event.Skip()

    def OnProjectFileCreated(self, event):
        file = event.File
        self.Compile(file)
        event.Skip()

    def CompileProject(self):
        #print "compile project"
        filesToCompile = set()
        filesToCache = set()
        for app in self.projectData["apps"]:
            srcPath = os.path.join(os.path.join(self.AppsPath(), app), "src")
            includePath = os.path.join(os.path.join(self.AppsPath(), app), "include")
            for path in [srcPath, includePath]:
                for root, _, files in os.walk(path):
                    for file in files:
                        file = os.path.join(root, file)
                        if file.endswith(".erl"):
                            filesToCompile.add((file, app))
                        elif file.endswith(".hrl"):
                            filesToCache.add(file)
        filesToCompile = sorted(list(filesToCompile))
        filesToCache = sorted(list(filesToCache))
        self.GetShell().CompileProjectFiles(filesToCompile)
        self.GetShell().GenerateFileCaches(filesToCache)
        self.RemoveUnusedBeams()

    def RemoveUnusedBeams(self):
        srcFiles = set()
        for app in self.projectData["apps"]:
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
        return "fly_compile" in self.projectData and self.projectData["fly_compile"]

    def AddErrors(self, path, errors):
        self.errors[path] = errors
        editor = GetTabMgr().FindPageByPath(path)
        if editor:
            editor.HighlightErrors(errors)
        self.errorsTable.AddErrors(path, errors)

    def GetErrors(self, path):
        if path not in self.errors: return []
        else: return self.errors[path]

    def OnKeyDown(self, event):
        if event.GetKeyCode() == ord('O') and event.ControlDown():
            dialog = FastProjectFileOpenDialog(GetTabMgr(), self)
            dialog.ShowModal()
        elif event.GetKeyCode() == ord('F') and event.ControlDown() and event.ShiftDown():
            dialog = FindInProjectDialog(GetTabMgr())
            dialog.Show()
        elif event.GetKeyCode() == ord('F') and event.ControlDown():
            dialog = FindInFileDialog(GetTabMgr())
            dialog.Show()
        else:
            event.Skip()

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
                GetTabMgr().LoadFile(value)
            else:
                event.Skip()
        else:
            event.Skip()

    def OnSelectCallback(self, values):
        self.Close()
        GetTabMgr().LoadFile(values[1])


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

class ErrorsTableGrid(wx.grid.Grid):
    def __init__(self, parent):
        wx.grid.Grid.__init__(self, parent, -1)
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
        file = rowData[0]
        line = rowData[1]
        editor = GetTabMgr().LoadFile(file)
        editor.GotoLine(line)

    def AddErrors(self, path, errors):
        #print path, errors
        current = len(self.table.data)

        data = list(filter(lambda x: x[0] != path, self.table.data))
        for e in errors:
            data.append((e.path, e.line, e.type, e.msg))
        data.sort()
        #print data
        self.table.data = data
        new = len(self.table.data)
        if new < current:
            msg = wx.grid.GridTableMessage(self.table, wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED,
                new, current-new)
            self.ProcessTableMessage(msg)
        elif new > current:
            msg = wx.grid.GridTableMessage(self.table, wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED,
                new - current)
            self.ProcessTableMessage(msg)
        msg = wx.grid.GridTableMessage(self.table, wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        self.ProcessTableMessage(msg)
        self.ForceRefresh()

def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)