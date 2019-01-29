from sys import exc_clear
import time
from wx.lib.agw.aui import AuiToolBar, ITEM_NORMAL
from idn_config import Config
from idn_findreplace import FindInProjectDialog
from idn_project_dialogs import FastProjectFileOpenDialog
from idn_utils import readFile, CreateButton, GetImage

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import yaml
import wx
from wx.lib.agw import aui
import idn_projectexplorer as exp
import core
from PyProgress import PyProgress

class ProgressTaskManagerDialog(wx.EvtHandler):
    def __init__(self):
        wx.EvtHandler.__init__(self)
        self.progressDialog = None
        self.progressTimer = wx.Timer(self, wx.ID_ANY)
        self.progressTimer.Start(250)
        self.Bind(wx.EVT_TIMER, self.OnProgressTimer, self.progressTimer)
        self.tasks = set()
        self.lastTaskTime = time.time()

    def AddTask(self, task):
        self.tasks.add(task)

    def UpdatePulse(self, description):
        if self.progressDialog:
            self.progressDialog.UpdatePulse(description)

    def TaskDone(self, description, task = None):
        if task:
            if task in self.tasks:
                self.lastTaskTime = time.time()
                self.tasks.remove(task)
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
            self.progressDialog.SetSize((600, 110))
            self.lastTaskTime = time.time()
            self.progressDialog.ShowDialog()

    def OnProgressTimer(self, event):
        if self.progressDialog:
            self.UpdatePulse("Tasks left: {}".format(len(self.tasks)))
            if (time.time() - self.lastTaskTime > 20 and len(self.tasks) > 0):
                core.Log("tasks left:", self.tasks)
                self.DestroyDialog()

    def DestroyDialog(self):
        if self.progressDialog:
            self.progressDialog.ReenableOtherWindows()
            self.progressDialog.Destroy()
            self.progressDialog = None

    def Close(self):
        self.progressTimer.Stop()

class Project(ProgressTaskManagerDialog):

    TYPE_PROJECT_DICT = {
    }

    EXPLORER_TYPE = exp.ProjectExplorer

    CONFIG_LAST_OPENED_FILES = "last_opened_files"
    CONFIG_EXPANDED_PATHS = "expanded_paths"
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
        core.Project = self
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData
        self.oldProjectData = None
        self.openedFilesChecker = None

        if not os.path.isdir(core.UserDataDir()):
            os.makedirs(core.UserDataDir())

        self.userDataFile = os.path.join(core.UserDataDir(), "{}.project.user".format(self.ProjectName()))
        if os.path.isfile(self.userDataFile):
            self.userData = yaml.load(open(self.userDataFile))
        else:
            self.userData = {}

        self.SetupMenu()

        self.CreateExplorer()
        self.OnLoadProject()
        self.OpenLastFiles()

        self.SetupPerspective()

        self.explorer.ProjectFilesModifiedEvent += self.OnProjectFilesModified

        core.TabMgr.Parent.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def SetRefreshInterval(self, interval):
        self.explorer.dirChecker.SetInterval(interval)
        if interval == 0 and not self.refreshTool:
            self.AddRefreshTool()
        elif interval > 0 and self.refreshTool:
            self.explorerToolbar.DeleteTool(self.refreshTool.GetId())
            self.refreshTool = None
        self.explorerToolbar.Realize()

    def SetupMenu(self):
        self.window.projectMenu.AppendMenuItem('Project Settings', self.window, self.OnEditProject)
        self.window.projectMenu.AppendMenuItem('Go to file', self.window, lambda e: self.ShowFastOpen(), "Ctrl-O")
        self.window.projectMenu.AppendMenuItem('Find/Replace in project', self.window, lambda e: self.ShowFindInProject(), "Ctrl-Shift-F")

    def ShowFindInProject(self):
        dialog = FindInProjectDialog.GetDialog(core.TabMgr)
        dialog.Show()
        stc = core.TabMgr.GetActiveEditor()
        if stc:
            if stc.SelectedText:
                dialog.findText.Value = stc.SelectedText
                dialog.findText.SetInsertionPointEnd()
        dialog.findText.SetFocus()

    def OnEditProject(self, event):
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
        if not Config.GetProp(Config.OPEN_LAST_FILES, True): return
        removedFiles = []
        for f in self.LastOpenedFiles():
            if not os.path.isfile(f):
                removedFiles.append(f)
            else:
                core.TabMgr.LoadFileLine(f)
        if removedFiles:
            self.userData[self.CONFIG_LAST_OPENED_FILES] = \
                [f for f in self.userData[self.CONFIG_LAST_OPENED_FILES] if f not in removedFiles]

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorerPanel = wx.Panel(self.window)
        explorerPanelSizer = wx.BoxSizer(wx.VERTICAL)
        self.explorerPanel.SetSizer(explorerPanelSizer)

        self.explorer = self.EXPLORER_TYPE(self.explorerPanel, self)
        self.explorer.SetRoot(self.projectDir)
        if self.CONFIG_EXPANDED_PATHS in self.userData:
            self.explorer.ExpandPaths(self.userData[self.CONFIG_EXPANDED_PATHS])
        self.explorer.SetHiddenList(self.HiddenPathsList())

        self.explorerToolbar = AuiToolBar(self.explorerPanel)
        self.showAllTool = self.explorerToolbar.AddToggleTool(wx.ID_ANY, GetImage('showAllFiles.png'), wx.NullBitmap, True, short_help_string = "Show all files = ignore masks")
        self.explorerToolbar.Bind(wx.EVT_TOOL, lambda e: self.explorer.OnMenuShowAllFiles(None), self.showAllTool)
        self.showHiddenTool = self.explorerToolbar.AddToggleTool(wx.ID_ANY, GetImage('showHidden.png'), wx.NullBitmap, True, short_help_string = "Show hidden dirs/files")
        self.explorerToolbar.Bind(wx.EVT_TOOL, lambda e: self.explorer.OnMenuShowHide(None), self.showHiddenTool)
        self.setupFileExtensionTool = self.explorerToolbar.AddTool(wx.ID_ANY, "Ext", GetImage('extensions_explorer.png'), wx.NullBitmap, True, short_help_string = "Setup file extensions")
        self.explorerToolbar.Bind(wx.EVT_TOOL, lambda e: self.explorer.OnMenuSetupMasks(None), self.setupFileExtensionTool)

        self.refreshTool = None
        self.AddRefreshTool()

        self.explorerToolbar.Realize()

        explorerPanelSizer.Add(self.explorerToolbar, 0)
        explorerPanelSizer.Add(self.explorer, 1, wx.EXPAND | wx.ALL, 2)

        self.window.WinMgr.AddPane1(self.explorerPanel, aui.AuiPaneInfo().Left().Layer(1).Caption("Project Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600).MinSize(100, 100)
            .MinimizeMode(aui.AUI_MINIMIZE_POS_LEFT | aui.AUI_MINIMIZE_CAPT_SMART))
        self.window.WinMgr.Update()

    def AddRefreshTool(self):
        self.refreshTool = self.explorerToolbar.AddTool(wx.ID_ANY, "", GetImage('refresh.png'), wx.NullBitmap, ITEM_NORMAL, short_help_string = "Refresh")
        self.explorerToolbar.Bind(wx.EVT_TOOL, lambda e: self.explorer.OnMenuRefresh(None), self.refreshTool)

    def GetMask(self):
        if self.CONFIG_MASK in self.userData:
            return self.userData[self.CONFIG_MASK]
        return None

    def Close(self):
        self.SaveUserData()
        self.explorer.OnClose()
        self.window.WinMgr.DetachPane(self.explorerPanel)
        self.window.WinMgr.Update()
        self.explorerPanel.Destroy()
        core.TabMgr.CloseAll()
        for item in self.window.projectMenu.GetMenuItems():
            self.window.projectMenu.Remove(item.GetId())

    def SaveUserData(self):
        openedFiles = []
        for path in core.TabMgr.OpenedFiles():
            if path.lower().startswith(self.projectDir.lower()):
                openedFiles.append(path)
        self.userData[self.CONFIG_LAST_OPENED_FILES] = openedFiles
        self.userData[self.CONFIG_HIDDEN_PATHS] = self.explorer.hiddenPaths
        self.userData[self.CONFIG_MASK] = self.explorer.GetCustomMask()
        self.userData[self.CONFIG_EXPANDED_PATHS] = self.explorer.ExpandedPaths()
        yaml.dump(self.userData, open(self.userDataFile, 'w'))

    def GetEditorTypes(self): return []

    def SetupPerspective(self):
        pass

    #def GetExcludedPaths(self):
    #    return [] if not CONFIG_EXCLUDED_PATHS in self.projectData else self.projectData[CONFIG_EXCLUDED_PATHS]

    def SaveData(self):
        stream = file(self.projectFilePath, 'w')
        yaml.dump(self.projectData, stream)

    def OnKeyDown(self, event):
        event.Skip()

    def ShowFastOpen(self):
        dialog = FastProjectFileOpenDialog(core.TabMgr, self)
        dialog.ShowModal()

    def OnProjectFilesModified(self, files):
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
