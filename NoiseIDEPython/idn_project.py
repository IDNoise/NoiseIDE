import time
from idn_project_dialogs import FastProjectFileOpenDialog

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
            if (time.time() - self.lastTaskTime > 8 and len(self.tasks) > 0):
                core.Log("tasks left:", self.tasks)
                self.DestroyDialog()

    def DestroyDialog(self):
        if self.progressDialog:
            self.progressDialog.ReenableOtherWindows()
            self.progressDialog.Destroy()
            self.progressDialog = None

class Project(ProgressTaskManagerDialog):

    TYPE_PROJECT_DICT = {
    }

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
        core.Project = self
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData
        self.oldProjectData = None

        if not os.path.isdir(self.USER_DATA_FOLDER):
            os.makedirs(self.USER_DATA_FOLDER)

        self.userDataFile = os.path.join(self.USER_DATA_FOLDER, "{}.project.user".format(self.ProjectName()))
        if os.path.isfile(self.userDataFile):
            self.userData = yaml.load(open(self.userDataFile))
        else:
            self.userData = {}

        self.SetupMenu()

        self.CreateExplorer()
        self.OnLoadProject()
        self.OpenLastFiles()

        self.SetupPerspective()

        core.TabMgr.Parent.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def SetupMenu(self):
        self.window.projectMenu.AppendMenuItem('Project Settings', self.window, self.OnEditProject)
        self.window.projectMenu.AppendMenuItem('Go to file', self.window, lambda e: self.ShowFastOpen(), "Ctrl-O")

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
        removedFiles = []
        for file in self.LastOpenedFiles():
            if not os.path.isfile(file):
                removedFiles.append(file)
            else:
                core.TabMgr.LoadFileLine(file)
        if removedFiles:
            self.userData[self.CONFIG_LAST_OPENED_FILES] = \
                [file for file in self.userData[self.CONFIG_LAST_OPENED_FILES] if file not in removedFiles]

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorer = self.EXPLORER_TYPE(self.window, self)
        self.explorer.SetRoot(self.projectDir)
        self.explorer.SetHiddenList(self.HiddenPathsList())
        self.window.WinMgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Layer(1).Caption("Project Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600).MinSize(100, 100)
            .MinimizeMode(aui.AUI_MINIMIZE_POS_LEFT | aui.AUI_MINIMIZE_CAPT_SMART))
        self.window.WinMgr.Update()

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
        yaml.dump(self.userData, open(self.userDataFile, 'w'))

    def GetEditorTypes(self): return []

    def SetupPerspective(self):
        pass

    def SaveData(self):
        stream = file(self.projectFilePath, 'w')
        yaml.dump(self.projectData, stream)

    def OnKeyDown(self, event):
        event.Skip()

    def ShowFastOpen(self):
        dialog = FastProjectFileOpenDialog(core.TabMgr, self)
        dialog.ShowModal()
