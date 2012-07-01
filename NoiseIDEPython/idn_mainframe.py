from idn_project import loadProject

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_customstc import CustomSTC
from idn_projectexplorer import ProjectExplorer, PythonProjectExplorer
from idn_winmanager import Manager
from idn_notebook import  Notebook
from idn_project import loadProject
from idn_config import Config

class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 1050))

        Config.load()
        ColorSchema.load(Config.GetProp("color_schema"))

        icon = wx.Icon('data/images/icon.png', wx.BITMAP_TYPE_PNG, 16, 16)
        self.SetIcon(icon)

        agwFlags = aui.AUI_MGR_DEFAULT | aui.AUI_MGR_AUTONB_NO_CAPTION
        self.winmgr = Manager(self, agwFlags = agwFlags )
        self.explorer = None

        agwStyle = aui.AUI_NB_DEFAULT_STYLE | \
                   aui.AUI_NB_CLOSE_ON_ALL_TABS | \
                   aui.AUI_NB_SMART_TABS | \
                   aui.AUI_NB_TAB_FLOAT | \
                   aui.AUI_NB_WINDOWLIST_BUTTON
        self.TabMgr = Notebook(self, agwStyle = agwStyle)

        self.winmgr.AddPane1(self.TabMgr, aui.AuiPaneInfo().Center()#.Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False))

        agwStyle = aui.AUI_NB_DEFAULT_STYLE ^ aui.AUI_NB_CLOSE_ON_ACTIVE_TAB
        self.ToolMgr = aui.AuiNotebook(self, agwStyle = agwStyle)
        self.winmgr.AddPane1(self.ToolMgr, aui.AuiPaneInfo().Bottom()#.Caption("Tools")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False).BestSize(400, 300))

        self.SetupMenu()

        self.winmgr.Update()

        self.Bind(wx.EVT_CLOSE, self.OnClose)

        projectPath = "D:\\Projects\\GIJoe\\server\\gijoe.noiseide.project"
        self.project = loadProject(self, projectPath)
        self.SetExplorerForProject(self.project)
        self.winmgr.Update()

        #self.AddTestTabs(10)

    def SetExplorerForProject(self, project):
        self.explorer = project.CreateExplorer()
        self.winmgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600))

    def SetupMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = wx.Menu()
        mOpen = self.fileMenu.Append(wx.NewId(), 'Open File', 'Open File')
        self.fileMenu.AppendSeparator()
        mOpenProject = self.fileMenu.Append(wx.NewId(), 'Open Project', 'Open Project')
        self.fileMenu.AppendSeparator()
        mQuit = self.fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.Bind(wx.EVT_MENU, self.OnOpen, mOpen)
        self.Bind(wx.EVT_MENU, self.OnOpenProject, mOpenProject)
        self.Bind(wx.EVT_MENU, self.OnQuit, mQuit)
        self.menubar.Append(self.fileMenu, '&File')
        self.SetMenuBar(self.menubar)

    def OnOpen(self, event):
        dialog = wx.FileDialog(
            self,
            message = "Select file",
            #wildcard = "*.erl",
            style = wx.FD_MULTIPLE | wx.FD_OPEN | wx.FD_FILE_MUST_EXIST
        )
        if dialog.ShowModal() == wx.ID_OK:
            files = dialog.GetFilenames()
            dirname = dialog.GetDirectory()
            for file in files:
                self.TabMgr.LoadFile(os.path.join(dirname, file))
        dialog.Destroy()

    def OnOpenProject(self, event):
        dialog = wx.FileDialog(
            self,
            message = "Select project",
            wildcard = "*.noiseide.project",
            style = wx.FD_OPEN | wx.FD_FILE_MUST_EXIST
        )
        if dialog.ShowModal() == wx.ID_OK:
            file = dialog.GetPath()
            self.OpenProject(file)
        dialog.Destroy()

    def OpenProject(self, projectFile):
        print "loading project ", projectFile

    def OnClose(self, event):
        if self.explorer:
            self.explorer.StopTrackingProject()
        if self.project:
            self.project.Close()
        event.Skip()

    def OnQuit(self, event):
        self.Close()

    def OnEvent(self, event):
        print(event)
        event.Skip()

    def AddTestTabs(self, amount):
        for i in range(amount):
            self.TabMgr.LoadFile(os.path.join(os.getcwd(), "eide_cache.erl"))

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