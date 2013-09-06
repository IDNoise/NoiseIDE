import os
import wx
from TextCtrlAutoComplete import TextCtrlAutoComplete
import core
import idn_erlang_explorer
from idn_utils import extension
from idn_erlang_constats import *

__author__ = 'Yaroslav'

class FastProjectFileOpenDialog(wx.Dialog):
    def __init__(self, parent, project):
        wx.Dialog.__init__(self, parent, title = "Open file", size = (600, 50))
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
        for f in files:
            if (f.startswith(project.AppsPath()) or f.startswith(project.DepsPath())
             or f.startswith(os.path.join(project.projectDir, "protocol"))):
                result.append((os.path.basename(f), f))
        return result

    def OnKeyDown(self, event):
        if event.GetKeyCode() == wx.WXK_ESCAPE:
            self.Close()
        elif event.GetKeyCode() == wx.WXK_RETURN:
            value = self.cb.GetValue()
            if os.path.isfile(value):
                self.Close()
                core.TabMgr.LoadFileLine(value)
            else:
                event.Skip()
        else:
            event.Skip()

    def OnSelectCallback(self, values):
        self.Close()
        core.TabMgr.LoadFileLine(values[1])