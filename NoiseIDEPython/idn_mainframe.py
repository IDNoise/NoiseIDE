from idn_utils import Menu

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_customstc import ConsoleSTC
from idn_winmanager import Manager
from idn_notebook import  Notebook, EditorNotebook
from idn_config import Config
import idn_global
from idn_project import loadProject, ErlangProjectFrom

class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 900), pos = (10, 10))
        self.cwd = os.getcwd()
        idn_global.MainFrame = self
        wx.ToolTip.SetMaxWidth(600)

        self.Maximize()
        Config.load()
        ColorSchema.load(Config.ColorSchema())

        icon = wx.Icon('data/images/icon.png', wx.BITMAP_TYPE_PNG, 16, 16)
        self.SetIcon(icon)

        self.explorer = None
        self.project = None

        agwFlags = aui.AUI_MGR_DEFAULT | aui.AUI_MGR_AUTONB_NO_CAPTION
        self.WinMgr = Manager(self, agwFlags = agwFlags )



        self.TabMgr = EditorNotebook(self)
        self.TabMgr.SetArtProvider(aui.ChromeTabArt())

        self.WinMgr.AddPane1(self.TabMgr, aui.AuiPaneInfo().Center()#.Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CaptionVisible(False)
            .CloseButton(False).Floatable(False))

        self.ToolMgr = Notebook(self)
        self.WinMgr.AddPane1(self.ToolMgr, aui.AuiPaneInfo().Bottom()#.Caption("Tools")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False).BestSize(400, 300))

        self.log = ConsoleSTC(self.ToolMgr)
        self.log.SetReadOnly(True)
        self.ToolMgr.AddPage(self.log, "Log")

        self.SetupMenu()

        self.WinMgr.Update()

        self.Bind(wx.EVT_CLOSE, self.OnClose)

        self.WinMgr.Update()

        wx.CallAfter(self.TryLoadLastProject)
        #self.OpenProject("D:\\Projects\\GIJoe\\server\\gijoe.noiseide.project")
        #self.OpenProject("D:\\Projects\\Joe\\server\\gijoe.noiseide.project")

    def Log(self, text):
        self.log.Append(text)

    def TryLoadLastProject(self):
        lastProject = Config.GetProp("last_project")
        if lastProject and os.path.isfile(lastProject):
            dial = wx.MessageDialog(None,
                'Do you want to open last project {}?'.format(os.path.basename(lastProject)),
                'Last project',
                wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
            if dial.ShowModal() == wx.ID_YES:
                self.OpenProject(lastProject)

    def SetupMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = Menu()
        self.fileMenu.AppendMenuItem('Open File', self, self.OnOpen)
        self.fileMenu.AppendSeparator()
        projectsMenu = Menu()
        projectsMenu.AppendMenuItem('Erlang', self, self.OnNewErlangProject)
        self.fileMenu.AppendMenu(wx.NewId(), "New project", projectsMenu)
        self.fileMenu.AppendMenuItem('Open Project', self, self.OnOpenProject)
        self.mEditProject = self.fileMenu.AppendMenuItem('Edit Project', self, self.OnEditProject)
        self.mEditProject.Enable(False)
        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('Quit', self, self.OnQuit)
        self.menubar.Append(self.fileMenu, '&File')
        helpMenu = Menu()
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def OnHelpAbout(self, event):
        wx.MessageBox("IDE with good functionality for Erlang programming language.\nMade by Yaroslav 'IDNoise' Nikityshev.", "Noise IDE v0.1")

    def MenuBar(self):
        return self.menubar

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
        if self.project:
            self.project.Close()
        Config.SetProp("last_project", projectFile)
        loadProject(self, projectFile)
        self.mEditProject.Enable(True)

    def OnNewErlangProject(self, event):
        ErlangProjectFrom().ShowModal()

    def OnEditProject(self, event):
        if not self.project: return
        form = self.project.GetEditForm()
        form(self.project).ShowModal()

    def OnClose(self, event):
        if self.project:
            self.project.Close()
        Config.save()
        event.Skip()

    def OnQuit(self, event):
        self.Close()

#    def OnEvent(self, event):
#        event.Skip()

class App(wx.App):
    def __init__(self):
        wx.App.__init__(self)
        frame = NoiseIDE()
        frame.Show()

if __name__ == '__main__':
    def main():
        app = App()
        app.MainLoop()

    #import cProfile
    #cProfile.run('main()', 'ideprof')
    #import pstats
    #p = pstats.Stats('ideprof')
    #p.strip_dirs().sort_stats(-1).print_stats()

    main()