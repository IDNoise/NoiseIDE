import os
import wx
import yaml
from idn_config import Config
from idn_erlang_constats import *
import core
from idn_project import Project
from idn_utils import CreateButton, CreateLabel
from idn_window_utils import NotEmptyTextValidator

__author__ = 'Yaroslav'

class ErlangProjectFrom(wx.Dialog):
    def __init__(self, project = None):
        wx.Dialog.__init__(self, core.MainFrame, size = (450, 600), title = "Create\Edit project",
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.consoles = {}

        self.CreateForm()

        self.project = project
        if self.project:
            self.SetCurrentValues()
            self.project.oldProjectData = self.project.projectData.copy()

    def CreateForm(self):
        self.projectNameTB = wx.TextCtrl(self, value = "Project_name", size = (300, 20), validator = NotEmptyTextValidator("Title"))
        self.projectNameTB.SetToolTipString("Project name")

        self.projectPathTB = wx.TextCtrl(self, value = "C:\\YourProjectFolder", size = (300, 20), validator = NotEmptyTextValidator("Project dir"))
        self.projectPathTB.SetToolTipString("Path to folder")
        self.projectPathTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.projectPathButton = CreateButton(self, "...", self.OnSelectProjectPath)
        self.projectPathButton.MinSize = (25, 25)

        self.appsDirTB = wx.TextCtrl(self, value = "apps", size = (300, 20), validator = NotEmptyTextValidator("Apps dir"))
        self.appsDirTB.SetToolTipString("Apps folder name")
        self.appsDirTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.depsDirTB = wx.TextCtrl(self, value = "deps", size = (300, 20), validator = NotEmptyTextValidator("Deps dir"))
        self.depsDirTB.SetToolTipString("Deps folder name")
        self.depsDirTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.compilerOptionsTB = wx.TextCtrl(self, value = "", size = (300, 60), style = wx.TE_MULTILINE)
        self.compilerOptionsTB.SetToolTipString("Compiler options in form: \n{d, Macro} or {d, Macro, Value}")

        self.excludedDirList = wx.CheckListBox(self, choices = [], size = (220, 150))
        self.excludedDirList.SetToolTipString("Directories to exclude from compilation")

        self.erlangCMB = wx.ComboBox(self, choices = Config.Runtimes().keys(), value = Config.Runtimes().keys()[0])
        self.erlangCMB.SetToolTipString("Select project runtime")

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

        gSizer.Add(CreateLabel(self, "Deps dir:"), (3, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.depsDirTB, (3, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Erlang runtime:"), (4, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.erlangCMB, (4, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Compiler options:"), (5, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.compilerOptionsTB, (5, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

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
        self.projectNameTB.Value = self.project.ProjectName()
        self.projectNameTB.Disable()
        self.projectPathTB.Value = self.project.projectDir
        self.projectPathTB.Disable()
        self.projectPathButton.Disable()
        self.appsDirTB.Value = self.project.projectData[CONFIG_APPS_DIR]
        self.depsDirTB.Value = self.project.projectData[CONFIG_DEPS_DIR]

        runtime = self.project.GetErlangRuntime()
        if runtime:
            self.erlangCMB.Value = runtime

        self.compilerOptionsTB.Value = self.project.CompilerOptions()

        self.excludedDirList.SetItems(self.project.projectData[CONFIG_EXCLUDED_DIRS] + self.project.GetApps())
        self.excludedDirList.SetCheckedStrings(self.project.projectData[CONFIG_EXCLUDED_DIRS])

        self.consoles = self.project.projectData[CONFIG_CONSOLES]
        self.UpdateConsoles()


    def OnPathChanged(self, event):
        dirs = []
        dir = self.projectPathTB.Value
        if dir and os.path.isdir(dir):
            apps = self.appsDirTB.Value
            appsDir = os.path.join(dir, apps)
            if apps and os.path.isdir(appsDir):
                dir = appsDir

            dirs = [d for d in os.listdir(dir) if os.path.isdir(os.path.join(dir, d))]

            deps = self.appsDirTB.Value
            depsDir = os.path.join(dir, deps)
            if deps and os.path.isdir(depsDir):
                dir = depsDir
            dirs += [d for d in os.listdir(dir) if os.path.isdir(os.path.join(dir, d))]

        self.excludedDirList.SetItems(dirs)

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
        deps = self.depsDirTB.Value
        erlang = self.erlangCMB.Value
        compilerOptions = self.compilerOptionsTB.Value
        excludedDirs = list(self.excludedDirList.GetCheckedStrings())

        data = {}
        data[Project.CONFIG_PROJECT_NAME] = title
        data[Project.CONFIG_PROJECT_TYPE] = "erlang"
        data[CONFIG_APPS_DIR] = apps
        data[CONFIG_DEPS_DIR] = deps
        data[CONFIG_EXCLUDED_DIRS] = excludedDirs
        data[CONFIG_COMPILER_OPTIONS] = compilerOptions

        data[CONFIG_CONSOLES] = self.consoles

        userData = {}
        userData[CONFIG_ERLANG_RUNTIME] = erlang

        pFile = os.path.join(path, title + ".noiseide")
        if not os.path.isdir(path):
            os.makedirs(path)

        appsPath = os.path.join(path, apps)
        if not os.path.isdir(appsPath):
            os.mkdir(appsPath)

        depsPath = os.path.join(path, deps)
        if not os.path.isdir(depsPath):
            os.mkdir(depsPath)

        stream = file(pFile, 'w')
        yaml.dump(data, stream)

        yaml.dump(userData, open(os.path.join(Project.USER_DATA_FOLDER, "{}.project.user".format(title)), 'w'))

        if self.project:
            self.project.projectData = data
            self.project.userData[CONFIG_ERLANG_RUNTIME] = erlang
            wx.CallAfter(self.project.UpdateProject)
        else:
            wx.CallAfter(core.MainFrame.OpenProject, pFile)

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
            self.snameTB.SetValue(data[CONFIG_CONSOLE_SNAME])
            self.cookieTB.SetValue(data[CONFIG_CONSOLE_COOKIE])
            self.paramsTB.SetValue(data[CONFIG_CONSOLE_PARAMS])
            self.commandTB.SetValue(data[CONFIG_CONSOLE_COMMAND])

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
        data[CONFIG_CONSOLE_SNAME] = sname
        data[CONFIG_CONSOLE_COOKIE] = cookie
        data[CONFIG_CONSOLE_PARAMS] = params
        data[CONFIG_CONSOLE_COMMAND] = command

        self.Parent.consoles[title] = data
        self.Close()