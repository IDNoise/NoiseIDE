__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx.lib.agw import aui
import wx.lib.agw.customtreectrl as CT
import wx.lib.agw.aui.framemanager as framemanager
from idn_colorschema import ColorSchema
from idn_customstc import CustomSTC
from wx.lib.agw.aui import AuiPaneInfo

class Notebook(aui.AuiNotebook):
    def FloatPage(self, pageIndex):
        pageTitle = self.GetPageText(pageIndex)
        aui.AuiNotebook.FloatPage(self, pageIndex)
        rootManager = framemanager.GetManager(self)
        if rootManager and rootManager != self._mgr:
            pane = rootManager.GetPaneByName("__floating__{}".format(pageTitle))
            pane.MaximizeButton(True).MinimizeButton(True).DefaultPane()
            pane.Hide().Show()
            rootManager.Update()

class Manager(aui.AuiManager):
    def OnFloatingPaneActivated(self, paneWindow):
        aui.AuiManager.OnFloatingPaneActivated(self, paneWindow)
        pane = self.GetPaneByWidget(paneWindow)
        print("OnFloatingPaneActivated", pane, pane.name)

    def OnPaneDocked(self, event):
        aui.AuiManager.OnPaneDocked(self, event)
        pane = event.GetPane()
        print("OnPaneDocked", pane, pane.name)


class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 1050))
        ColorSchema.load("dark")

        agwFlags = aui.AUI_MGR_DEFAULT | aui.AUI_MGR_USE_NATIVE_MINIFRAMES | aui.AUI_MGR_AUTONB_NO_CAPTION
        self.winmgr = Manager(self, agwFlags = agwFlags )

        self.explorer = CT.CustomTreeCtrl(self)
        self.winmgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600))
        agwStyle = aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_CLOSE_ON_ALL_TABS | \
                   aui.AUI_NB_SMART_TABS | aui.AUI_NB_TAB_FLOAT | aui.AUI_NB_WINDOWLIST_BUTTON
        self.tabmgr = Notebook(self, agwStyle = agwStyle)
        self.winmgr.AddPane1(self.tabmgr, aui.AuiPaneInfo().Center().Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False))

        self.menubar = wx.MenuBar()
        self.fileMenu = wx.Menu()
        fitem = self.fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.menubar.Append(self.fileMenu, '&File')
        self.SetMenuBar(self.menubar)

        self.AddTestTabs(10)
        self.winmgr.Update()

        #self.winmgr.Bind(aui.EVT_AUI_PANE_FLOATED, self.OnEvent)


        icon = wx.Icon('icon.gif', wx.BITMAP_TYPE_GIF, 16, 16)
        self.SetIcon(icon)

    def OnEvent(self, event):
        print(event)
        event.Skip()

    def AddTestTabs(self, amount):
        for i in range(amount):
            editor = CustomSTC(self, os.path.join(os.getcwd(), "eide_cache.erl"))
            self.tabmgr.AddPage(editor, editor.FileName() + str(i))

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