__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_customstc import CustomSTC
from idn_projectexplorer import ProjectExplorer
from idn_winmanager import Manager

class Notebook(aui.AuiNotebook):
    pass
#    def FloatPage(self, pageIndex):
#        pageTitle = self.GetPageText(pageIndex)
#        aui.AuiNotebook.FloatPage(self, pageIndex)
#        rootManager = framemanager.GetManager(self)
#        if rootManager and rootManager != self._mgr:
#            pane = rootManager.GetPaneByName("__floating__{}".format(pageTitle))
#            pane.MaximizeButton(True).MinimizeButton(True).DefaultPane()
#            print(pane.frame)
#            pane.Hide().Show()
#            rootManager.Update()
#    def OnEvent(self, event):
#        event.Skip()
#        print(event)




class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 1050))

        ColorSchema.load("dark")

        icon = wx.Icon('data/images/icon.png', wx.BITMAP_TYPE_PNG, 16, 16)
        self.SetIcon(icon)

        agwFlags = aui.AUI_MGR_DEFAULT | aui.AUI_MGR_AUTONB_NO_CAPTION
        self.winmgr = Manager(self, agwFlags = agwFlags )

        self.explorer = ProjectExplorer(self)
        self.explorer.SetRoot(os.getcwd()) #test

        self.winmgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600))

        agwStyle = aui.AUI_NB_DEFAULT_STYLE | \
                   aui.AUI_NB_CLOSE_ON_ALL_TABS | \
                   aui.AUI_NB_SMART_TABS | \
                   aui.AUI_NB_TAB_FLOAT | \
                   aui.AUI_NB_WINDOWLIST_BUTTON
        self.tabmgr = Notebook(self, agwStyle = agwStyle)

        self.winmgr.AddPane1(self.tabmgr, aui.AuiPaneInfo().Center().Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False))

        self.SetupMenu()


        self.winmgr.Update()

        self.AddTestTabs(10)

    def SetupMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = wx.Menu()
        fitem = self.fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.Bind(wx.EVT_MENU, self.OnQuit, fitem)
        self.menubar.Append(self.fileMenu, '&File')
        self.SetMenuBar(self.menubar)

    def OnQuit(self, event):
        self.Close()

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