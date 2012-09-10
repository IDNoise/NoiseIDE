from idn_utils import Menu

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_customstc import ConsoleSTC, CustomSTC
from idn_winmanager import Manager
from idn_notebook import  Notebook, EditorNotebook, ConsolePanel
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
        self.TabMgr.SetArtProvider(aui.VC71TabArt())
        #self.TabMgr.SetArtProvider(aui.ChromeTabArt())
        #self.TabMgr.SetArtProvider(aui.FF2TabArt())

        self.WinMgr.AddPane1(self.TabMgr, aui.AuiPaneInfo().Center()#.Caption("Code Editor")
            .MaximizeButton().MinimizeButton().CaptionVisible(False)
            .CloseButton(False).Floatable(False).MinSize(100, 100))

        self.ToolMgr = Notebook(self)
        self.WinMgr.AddPane1(self.ToolMgr, aui.AuiPaneInfo().Bottom()#.Caption("Tools")
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False).BestSize(400, 300).MinSize(100, 100))

        self.logPanel = ConsolePanel(self.ToolMgr)
        self.log = self.logPanel.editor
        self.log.SetReadOnly(True)
        self.ToolMgr.AddPage(self.logPanel, "Log")

        self.SetupMenu()

        self.WinMgr.Update()

        self.Bind(wx.EVT_CLOSE, self.OnClose)

        self.WinMgr.Update()

        wx.CallAfter(self.TryLoadLastProject)

        #self.TabMgr.Bind(aui.EVT_AUINOTEBOOK_PAGE_CHANGED, self.OnNotebookPageChanged)
        #self.OpenProject("D:\\Projects\\GIJoe\\server\\gijoe.noiseide.project")
        #self.OpenProject("D:\\Projects\\Joe\\server\\gijoe.noiseide.project")

#    def OnNotebookPageChanged(self, event):
#        for item in self.editorMenu.GetMenuItems():
#            self.editorMenu.Enable(item.GetId(), self.TabMgr.GetSelection() != -1)

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

        if Config.LastProjects():
            lastProjects = Menu()
            self.fileMenu.AppendMenu(wx.NewId(), "Last projects", lastProjects)
            def handler(p):
                print p
                return lambda e: self.OpenProject(p)
            for p in Config.LastProjects():
                if os.path.isfile(p):
                    lastProjects.AppendMenuItem(os.path.basename(p), self, handler(p))

        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('Quit', self, self.OnQuit)
        self.menubar.Append(self.fileMenu, '&File')

        self.editorMenu = Menu()
        self.editorMenu.AppendCheckMenuItem('Show white space', self, self.OnShowWhiteSpace)
        self.editorMenu.AppendCheckMenuItem('Show EOL', self, self.OnShowEOL)
        self.menubar.Append(self.editorMenu, '&Editor')

        helpMenu = Menu()
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def OnShowWhiteSpace(self, event):
        CustomSTC.ShowWhiteSpace = not CustomSTC.ShowWhiteSpace
        for editor in self.TabMgr.Pages():
            editor.UpdateOptions()

    def OnShowEOL(self, event):
        CustomSTC.ShowEOL = not CustomSTC.ShowEOL
        for editor in self.TabMgr.Pages():
            editor.UpdateOptions()

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
                file =os.path.join(dirname, file)
                self.TabMgr.LoadFileLine(file)
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
        projectFile = os.path.normcase(projectFile)
        Config.SetProp("last_project", projectFile)

        projects = Config.LastProjects()
        if projectFile in projects:
            projects.remove(projectFile)
        projects.append(projectFile)
        Config.SetLastProjects(projects)

        loadProject(self, projectFile)
        self.project.mEditProject.Enable(True)

    def OnNewErlangProject(self, event):
        ErlangProjectFrom().ShowModal()

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