import os
from idn_config import Config
from idn_utils import CreateButton, CreateLabel
from idn_window_utils import NotEmptyTextValidator

__author__ = 'Yaroslav'

import wx

class ErlangOptionsDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, size = (320, 150), title = "Erlang options")
        self.panel = ErlangOptionsPanel(self)

class ErlangOptionsPanel(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)

        self.runtimes = Config.Runtimes().copy()

        self.runtimesList = wx.ListBox(self, size = (200, 60), choices = self.runtimes.keys())
        self.addRuntimeButton = CreateButton(self, "Add", self.OnAddRuntime)
        self.editRuntimeButton = CreateButton(self, "Edit", self.OnEditRuntime)
        self.removeRuntimeButton = CreateButton(self, "Remove", self.OnRemoveRuntime)


        runtimesSizer = wx.StaticBoxSizer(wx.StaticBox(self, label = "Runtimes:"), wx.HORIZONTAL)

        runtimesSizer.Add(self.runtimesList, flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        bSizer = wx.BoxSizer(wx.VERTICAL)
        bSizer.Add(self.addRuntimeButton)
        bSizer.Add(self.editRuntimeButton)
        bSizer.Add(self.removeRuntimeButton)

        runtimesSizer.AddSizer(bSizer, flag = wx.ALL | wx.ALIGN_CENTER, border = 4)

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.AddSizer(runtimesSizer)
        self.SetSizer(sizer)

    def OnAddRuntime(self, event):
        dlg = RuntimeCreateEditDialog(self)
        dlg.ShowModal()
        self.UpdateRuntimes()

    def OnRemoveRuntime(self, event):
        del self.runtimes[self.runtimesList.GetStringSelection()]
        self.UpdateRuntimes()

    def OnEditRuntime(self, event):
        dlg = RuntimeCreateEditDialog(self, self.runtimesList.GetStringSelection())
        dlg.ShowModal()
        self.UpdateRuntimes()

    def UpdateRuntimes(self):
        self.runtimesList.Clear()
        for runtime in self.runtimes:
            self.runtimesList.Append(runtime)
        Config.SetProp(Config.RUNTIMES, self.runtimes)

class RuntimeCreateEditDialog(wx.Dialog):
    def __init__(self, parent, runtime = None):
        wx.Dialog.__init__(self, parent, title = "Runtime props",
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.currentRuntime = runtime

        self.titleTB = wx.TextCtrl(self, value = "erlang", size = (250, 20), validator = NotEmptyTextValidator("Title"))
        self.pathTB = wx.TextCtrl(self, value = "", size = (250, 20), validator = NotEmptyTextValidator("Erlang path"))
        self.erlangPathButton = CreateButton(self, "...", self.OnSelectErlangPath)
        self.erlangPathButton.MinSize = (25, 25)

        self.saveB = CreateButton(self, "Save", self.OnSave)
        self.cancelB = CreateButton(self, "Cancel", lambda e: self.Close())

        if runtime:
            self.titleTB.SetValue(runtime)
            self.pathTB.SetValue(Config.Runtimes()[runtime])

        gSizer = wx.GridBagSizer(2, 2)

        gSizer.Add(CreateLabel(self, "Title:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.titleTB, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Path:"), (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.pathTB, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.erlangPathButton, (1, 2), flag = wx.ALIGN_CENTER)

        gSizer.Add(self.cancelB, (2, 0), flag = wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border = 4)
        gSizer.Add(self.saveB, (2, 1), flag = wx.ALL | wx.ALIGN_RIGHT | wx.EXPAND, border = 4)

        self.SetSizer(gSizer)
        self.Layout()

    def OnSelectErlangPath(self, event):
        dlg = wx.FileDialog(self, defaultFile = self.pathTB.Value)
        if dlg.ShowModal() == wx.ID_OK:
            self.pathTB.Value = dlg.GetPath()

    def OnSave(self, event):

        if not self.Validate(): return

        title = self.titleTB.Value
        path = self.pathTB.Value

        if (title and self.currentRuntime
            and self.currentRuntime != title
            and not title in self.Parent.runtimes ):
            del self.Parent.runtimes[self.currentRuntime]

        self.Parent.runtimes[title] = path
        self.Close()

class ErlangDialyzerDialog(wx.Dialog):
    def __init__(self, parent, project):
        wx.Dialog.__init__(self, parent, title = "Dialyzer options", size = (370, 120),
            style = wx.DEFAULT_DIALOG_STYLE | wx.WS_EX_VALIDATE_RECURSIVELY)

        self.project = project

        self.homeTB = wx.TextCtrl(self, value = "", size = (250, 20))
        self.pathTB = wx.TextCtrl(self, value = "", size = (250, 20), validator = NotEmptyTextValidator("Plt file path"))
        self.pltPathButton = CreateButton(self, "...", self.OnSelectPltPath)
        self.pltPathButton.MinSize = (25, 25)
        self.homePathButton = CreateButton(self, "...", self.OnSelectHomePath)
        self.homePathButton.MinSize = (25, 25)

        self.saveB = CreateButton(self, "Save", self.OnSave)
        self.cancelB = CreateButton(self, "Cancel", lambda e: self.Close())

        pltFilePath = self.project.PltPath()
        home = self.project.HomeDir()
        if pltFilePath:
            self.pathTB.SetValue(pltFilePath)
        if home:
            self.homeTB.SetValue(home)

        gSizer = wx.GridBagSizer(2, 2)

        gSizer.Add(CreateLabel(self, "Path:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.pathTB, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.pltPathButton, (0, 2), flag = wx.ALIGN_CENTER)

        gSizer.Add(CreateLabel(self, "Home:"), (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.homeTB, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)
        gSizer.Add(self.homePathButton, (1, 2), flag = wx.ALIGN_CENTER)

        gSizer.Add(self.cancelB, (2, 0), flag = wx.ALL | wx.ALIGN_LEFT | wx.EXPAND, border = 4)
        gSizer.Add(self.saveB, (2, 1), flag = wx.ALL | wx.ALIGN_RIGHT | wx.EXPAND, border = 4)

        self.SetSizer(gSizer)
        self.Layout()

    def OnSelectPltPath(self, event):
        dlg = wx.FileDialog(self, defaultFile = self.pathTB.Value, wildcard = "*.plt")
        if dlg.ShowModal() == wx.ID_OK:
            self.pathTB.Value = dlg.GetPath()

    def OnSelectHomePath(self, event):
        dlg = wx.DirDialog(self, defaultFile = self.homeTB.Value)
        if dlg.ShowModal() == wx.ID_OK:
            self.homeTB.Value = dlg.GetPath()


    def OnSave(self, event):
        if not self.Validate(): return

        plt = self.pathTB.Value
        home = self.homeTB.Value

        if home and not os.path.isdir(home):
            wx.MessageBox("Home dir {} doesn't exist.".format(home))
            return

        if not os.path.isfile(plt):
            wx.MessageBox("Plt file {} doesn't exist".format(plt))
            return

        self.project.SetHomeDir(home)
        self.project.SetPltPath(plt)
        self.Close()