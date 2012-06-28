from wx.lib.agw import aui
import wx.lib.agw.customtreectrl as CT
__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from idn_colorschema import ColorSchema
from idn_customstc import CustomSTC

class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 1050))
        ColorSchema.load("dark")

        self.winmgr = aui.AuiManager(self)

        self.explorer = CT.CustomTreeCtrl(self)
        self.winmgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Explorer")
        .MinimizeButton().CloseButton(False).BestSize2(300, 600))

        self.tabmgr = aui.AuiNotebook(self)
        self.winmgr.AddPane1(self.tabmgr, aui.AuiPaneInfo().Center().Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CloseButton(False))

        self.menubar = wx.MenuBar()
        self.fileMenu = wx.Menu()
        fitem = self.fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.menubar.Append(self.fileMenu, '&File')
        self.SetMenuBar(self.menubar)

        self.AddTestTabs(10)
        self.winmgr.Update()

        icon = wx.Icon('icon.gif', wx.BITMAP_TYPE_GIF, 16, 16)
        self.SetIcon(icon)

    def AddTestTabs(self, amount):
        for i in range(amount):
            editor = CustomSTC(self, os.path.join(os.getcwd(), "eide_cache.erl"))
            self.tabmgr.AddPage(editor, editor.FileName())

class App(wx.App):
    def __init__(self):
        wx.App.__init__(self)
        frame = NoiseIDE()
        frame.Show()


if __name__ == '__main__':
    def main():
        app = App()
        app.MainLoop()

    main()