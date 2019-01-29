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
    def __init__(self, project = None, projectType = None):
        wx.Dialog.__init__(self, core.MainFrame, title = "Create\Edit project",
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.consoles = {}
        self.projectType = projectType
        if projectType == SINGLE_APP_PROJECT:
            self.CreateSingleAppForm()
        elif projectType == MULTIPLE_APP_PROJECT:
            self.CreateMultipleAppsForm()

        self.project = project
        if self.project:
            self.SetCurrentValues()
            self.project.oldProjectData = self.project.projectData.copy()

    def CreateCommonElements(self):
        self.projectNameTB = wx.TextCtrl(self, value = "Project_name", size = (300, 20), validator = NotEmptyTextValidator("Title"))
        self.projectNameTB.SetToolTipString("Project name")

        self.projectPathTB = wx.TextCtrl(self, value = "C:\\YourProjectFolder", size = (300, 20), validator = NotEmptyTextValidator("Project dir"))
        self.projectPathTB.SetToolTipString("Path to folder")

        self.projectPathButton = CreateButton(self, "...", self.OnSelectProjectPath)
        self.projectPathButton.MinSize = (25, 25)

        self.compilerOptionsTB = wx.TextCtrl(self, value = "", size = (300, 60), style = wx.TE_MULTILINE)
        self.compilerOptionsTB.SetToolTipString("Compiler options in form: \n{d, Macro} or {d, Macro, Value}")

        self.erlangCMB = wx.ComboBox(self, choices = Config.Runtimes().keys(), value = Config.Runtimes().keys()[0])
        self.erlangCMB.SetToolTipString("Select project runtime")

        self.consolesList = wx.ListBox(self, size = (200, 70))
        self.consolesList.SetToolTipString("Consoles list for project. Each console has own params and can be started independent")

        self.addConsoleButton = CreateButton(self, "Add", self.OnAddConsole)
        self.editConsoleButton = CreateButton(self, "Edit", self.OnEditConsole)
        self.removeConsoleButton = CreateButton(self, "Remove", self.OnRemoveConsole)

        self.closeButton = CreateButton(self, "Close", lambda e: self.Close())
        self.saveButton = CreateButton(self, "Save", self.OnSave)

        self.consoleSizer = wx.StaticBoxSizer(wx.StaticBox(self, label = "Consoles"), wx.HORIZONTAL)

        bSizer = wx.BoxSizer(wx.VERTICAL)
        bSizer.Add(self.addConsoleButton)
        bSizer.Add(self.editConsoleButton)
        bSizer.Add(self.removeConsoleButton)

        self.consoleSizer.Add(self.consolesList, 1, flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        self.consoleSizer.AddSizer(bSizer, flag = wx.ALL | wx.ALIGN_CENTER, border = 4)

        self.saveCloseButtonSizer = wx.BoxSizer(wx.HORIZONTAL)
        self.saveCloseButtonSizer.Add(self.closeButton, flag = wx.ALL | wx.ALIGN_LEFT, border = 4)
        self.saveCloseButtonSizer.AddStretchSpacer()
        self.saveCloseButtonSizer.Add(self.saveButton, flag = wx.ALL | wx.ALIGN_RIGHT, border = 4)

    def CreateSingleAppForm(self):
        self.CreateCommonElements()

        sizer = wx.BoxSizer(wx.VERTICAL)
        gSizer = wx.GridBagSizer(2, 2)
        i = 0
        gSizer.Add(CreateLabel(self, "Title:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectNameTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Project dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectPathTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.projectPathButton, (i, 2), flag = wx.ALIGN_CENTER)
        i += 1
        gSizer.Add(CreateLabel(self, "Erlang runtime:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.erlangCMB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Compiler options:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.compilerOptionsTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        sizer.AddSizer(gSizer)

        sizer.AddSizer(self.consoleSizer, flag =  wx.EXPAND)
        sizer.AddSizer(self.saveCloseButtonSizer, 1, flag = wx.EXPAND)

        self.SetSizer(sizer)
        self.Layout()
        sizer.SetSizeHints(self)

    def CreateMultipleAppsForm(self):
        self.CreateCommonElements()

        self.projectPathTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.appsDirTB = wx.TextCtrl(self, value = "apps", size = (300, 20))
        self.appsDirTB.SetToolTipString("Apps folder name")
        self.appsDirTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.depsDirTB = wx.TextCtrl(self, value = "deps", size = (300, 20))
        self.depsDirTB.SetToolTipString("Deps folder name")
        self.depsDirTB.Bind(wx.EVT_TEXT, self.OnPathChanged)

        self.workDirTB = wx.TextCtrl(self, value = "", size = (300, 20))
        self.workDirTB.SetToolTipString("Work dir. Empty if root.")

        self.excludedDirList = wx.CheckListBox(self, choices = [], size = (220, 150))
        self.excludedDirList.SetToolTipString("Apps to exclude from compilation")

        self.excludedPathList = wx.ListBox(self, choices=[], size=(220, 150))
        self.excludedPathList.SetToolTipString("Directories to exclude from project (requires restart)")

        self.excludedPathAddButton = CreateButton(self, "Add", self.OnAddExcludedPath)
        self.excludedPathRemoveButton = CreateButton(self, "Remove", self.OnRemoveExcludedPath)

        sizer = wx.BoxSizer(wx.VERTICAL)
        gSizer = wx.GridBagSizer(2, 2)
        i = 0
        gSizer.Add(CreateLabel(self, "Title:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectNameTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Project dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.projectPathTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.projectPathButton, (i, 2), flag = wx.ALIGN_CENTER)
        i += 1
        gSizer.Add(CreateLabel(self, "Apps dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.appsDirTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Deps dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.depsDirTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Work dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.workDirTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Erlang runtime:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.erlangCMB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        i += 1
        gSizer.Add(CreateLabel(self, "Compiler options:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.compilerOptionsTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        sizer.AddSizer(gSizer)

        excludedAppsSizer = wx.StaticBoxSizer(wx.StaticBox(self, label = "Excluded apps"), wx.HORIZONTAL)
        excludedAppsSizer.Add(self.excludedDirList, 1, flag = wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border = 4)
        sizer.AddSizer(excludedAppsSizer, flag = wx.EXPAND)

        excludedPathsSizer = wx.StaticBoxSizer(wx.StaticBox(self, label="Excluded paths"), wx.HORIZONTAL)
        excludedPathsSizer.Add(self.excludedPathList, 1, flag=wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border=4)
        excludedPathButtonSizer = wx.BoxSizer(wx.VERTICAL)
        excludedPathButtonSizer.Add(self.excludedPathAddButton)
        excludedPathButtonSizer.Add(self.excludedPathRemoveButton)
        excludedPathsSizer.AddSizer(excludedPathButtonSizer, flag=wx.ALL | wx.ALIGN_RIGHT, border=4)
        sizer.AddSizer(excludedPathsSizer, flag=wx.EXPAND)
        sizer.AddSizer(excludedPathButtonSizer)

        sizer.AddSizer(self.consoleSizer, flag =  wx.EXPAND)
        sizer.AddSizer(self.saveCloseButtonSizer, 1, flag = wx.EXPAND)

        self.SetSizer(sizer)
        self.Layout()
        sizer.SetSizeHints(self)

    def OnAddExcludedPath(self, event):
        selector = wx.DirSelector("Choose a folder", defaultPath = self.project.projectDir)
        path = selector.strip().replace(self.project.projectDir + os.sep, "")
        if path and not path in self.excludedPathList.GetItems():
            self.excludedPathList.Append(path)

    def OnRemoveExcludedPath(self, event):
        selectedIndexes = self.excludedPathList.GetSelections()
        for i in reversed(selectedIndexes):
            self.excludedPathList.Delete(i)

    def SetCurrentValues(self):
        self.projectNameTB.Value = self.project.ProjectName()
        self.projectNameTB.Disable()
        self.projectPathTB.Value = self.project.projectDir
        self.projectPathTB.Disable()
        self.projectPathButton.Disable()

        runtime = self.project.GetErlangRuntime()
        if runtime:
            self.erlangCMB.Value = runtime

        self.compilerOptionsTB.Value = self.project.CompilerOptions()

        self.consoles = self.project.projectData[CONFIG_CONSOLES]
        self.UpdateConsoles()

        if self.projectType == MULTIPLE_APP_PROJECT:
            self.appsDirTB.Value = self.project.projectData[CONFIG_APPS_DIR]
            self.depsDirTB.Value = self.project.projectData[CONFIG_DEPS_DIR]
            self.workDirTB.Value = self.project.projectData[CONFIG_WORK_DIR] if CONFIG_WORK_DIR in self.project.projectData else ""

            self.excludedDirList.SetItems(self.project.projectData[CONFIG_EXCLUDED_DIRS] + self.project.GetAppsAndDeps())
            self.excludedDirList.SetCheckedStrings(self.project.projectData[CONFIG_EXCLUDED_DIRS])

        self.excludedPathList.SetItems(self.project.projectData[CONFIG_EXCLUDED_PATHS])

    def OnPathChanged(self, event):
        dirs = set()
        if self.projectPathTB.Value and os.path.isdir(self.projectPathTB.Value):
            apps = self.appsDirTB.Value
            appsDir = os.path.join(self.projectPathTB.Value, apps)
            if apps and os.path.isdir(appsDir):
                [dirs.add(d) for d in os.listdir(appsDir) if os.path.isdir(os.path.join(appsDir, d))]

            deps = self.depsDirTB.Value
            depsDir = os.path.join(self.projectPathTB.Value, deps)
            if deps and os.path.isdir(depsDir):
                [dirs.add(d) for d in os.listdir(depsDir) if os.path.isdir(os.path.join(depsDir, d))]
        dirs = list(sorted(list(dirs)))
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
        erlang = self.erlangCMB.Value
        compilerOptions = self.compilerOptionsTB.Value

        data = {}
        data[Project.CONFIG_PROJECT_NAME] = title
        data[Project.CONFIG_PROJECT_TYPE] = "erlang"
        data[CONFIG_COMPILER_OPTIONS] = compilerOptions
        data[CONFIG_CONSOLES] = self.consoles
        data[CONFIG_PROJECT_TYPE] = self.projectType
        data[CONFIG_EXCLUDED_PATHS] = list(self.excludedPathList.GetItems())

        if self.projectType == MULTIPLE_APP_PROJECT:
            apps = self.appsDirTB.Value
            deps = self.depsDirTB.Value
            workDir = self.workDirTB.Value

            data[CONFIG_APPS_DIR] = apps
            data[CONFIG_DEPS_DIR] = deps
            data[CONFIG_WORK_DIR] = workDir
            data[CONFIG_EXCLUDED_DIRS] =  list(self.excludedDirList.GetCheckedStrings())

            appsPath = os.path.join(path, apps)
            depsPath = os.path.join(path, deps)

            dirs = [appsPath, depsPath]
            if workDir:
                workDirPath = os.path.join(path, workDir)
                dirs.append(workDirPath)

            for d in dirs:
                if not os.path.isdir(d):
                    os.mkdir(d)

        userData = self.project.userData.copy() if self.project else {}
        userData[CONFIG_ERLANG_RUNTIME] = erlang

        pFile = os.path.join(path, title + ".noiseide")
        if not os.path.isdir(path):
            os.makedirs(path)

        stream = file(pFile, 'w')
        yaml.dump(data, stream)

        yaml.dump(userData, file(os.path.join(core.UserDataDir(), "{}.project.user".format(title)), 'w'))

        if self.project:
            if self.project.GetErlangRuntime() != erlang:
                self.project.userData[CONFIG_ERLANG_RUNTIME] = erlang
                wx.CallAfter(core.MainFrame.OpenProject, pFile)
            else:
                self.project.projectData = data
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
        gSizer.SetSizeHints(self)

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