__author__ = 'Yaroslav Nikityshev aka IDNoise'

import sys
import urllib2
from wx.lib.dialogs import  MultiMessageDialog
import yaml
from idn_erlang_dialogs import ErlangOptionsDialog
from idn_erlang_project import ErlangProject
from idn_erlang_project_form import ErlangProjectFrom
import idn_installer
from idn_utils import Menu, GetImage, readFile, writeFile, writeBinaryFile
import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_winmanager import Manager
from idn_notebook import  Notebook, EditorNotebook
from idn_config import Config, ConfigEditForm
import idn_global
from idn_project import Project

#lists:flatten(edoc:read("d:/projects/noiseide/noiseidepython/data/erlang/modules/noiseide/src/test_cache_module.erl")).

installNewVersion = False

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

        self.SetupSimpleMenu()
        self.CreateToolBar()

        Project.TYPE_PROJECT_DICT["erlang"] = ErlangProject


        self.TabMgr = EditorNotebook(self)
        self.TabMgr.SetArtProvider(aui.VC71TabArt())
        #self.TabMgr.SetArtProvider(aui.ChromeTabArt())
        #self.TabMgr.SetArtProvider(aui.FF2TabArt())
        self.TabMgrPaneInfo = aui.AuiPaneInfo().Center()\
            .MaximizeButton().MinimizeButton().CaptionVisible(False)\
            .CloseButton(False).Floatable(False).MinSize(100, 100)
        self.WinMgr.AddPane1(self.TabMgr, self.TabMgrPaneInfo )

        self.ToolMgr = Notebook(self)
        self.ToolMgrPaneInfo = aui.AuiPaneInfo().Bottom()\
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False)\
            .BestSize(400, 300).MinSize(100, 100).Name("Tools").Caption("Tools").CaptionVisible(True)\
            .MinimizeMode(aui.AUI_MINIMIZE_POS_LEFT | aui.AUI_MINIMIZE_CAPT_SMART)
        self.WinMgr.AddPane1(self.ToolMgr, self.ToolMgrPaneInfo)

        #self.logPanel = ConsolePanel(self.ToolMgr)
        #self.log = self.logPanel.editor
        #self.log.SetReadOnly(True)
        #self.ToolMgr.AddPage(self.logPanel, "Log")
        #self.ToolMgr.

        #self.WinMgr.MaximizePane()

        self.WinMgr.Update()

        self.Bind(wx.EVT_CLOSE, self.OnClose)

        self.WinMgr.Update()

        args = sys.argv[1:]
        if args:
            path = args[0]
            if os.path.isfile(path) and path.endswith("noiseide"):
                wx.CallAfter(self.OpenProject, path)
            else:
                wx.CallAfter(self.TryLoadLastProject)
        else:
            wx.CallAfter(self.TryLoadLastProject)

        self.logFile = open(os.path.join(self.cwd, "ide.log"), 'w')

        #self.TabMgr.Bind(aui.EVT_AUINOTEBOOK_PAGE_CHANGED, self.OnNotebookPageChanged)
        #self.OpenProject("D:\\Projects\\GIJoe\\server\\gijoe.noiseide.project")
        #self.OpenProject("D:\\Projects\\Joe\\server\\gijoe.noiseide.project")

#    def OnNotebookPageChanged(self, event):
#        for item in self.editorMenu.GetMenuItems():
#            self.editorMenu.Enable(item.GetId(), self.TabMgr.GetSelection() != -1)

    def SetupSimpleMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = Menu()

        projectsMenu = Menu()
        projectsMenu.AppendMenuItem('Erlang', self, self.OnNewErlangProject)

        self.fileMenu.AppendMenu(wx.NewId(), "New project", projectsMenu)
        self.fileMenu.AppendMenuItem('Open Project', self, self.OnOpenProject)

        if Config.LastProjects():
            lastProjects = Menu()
            self.fileMenu.AppendMenu(wx.NewId(), "Recent projects", lastProjects)
            def handler(p):
                return lambda e: self.OpenProject(p)
            for p in Config.LastProjects():
                if os.path.isfile(p):
                    lastProjects.AppendMenuItem(os.path.basename(p), self, handler(p))

        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('User Settings', self, self.OnEditOptions)
        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('Quit', self, self.OnQuit)
        self.menubar.Append(self.fileMenu, '&File')


        languagesMenu = Menu()
        self.menubar.Append(languagesMenu, "&Languages")
        erlangMenu = Menu()
        languagesMenu.AppendMenu(wx.NewId(), 'Erlang', erlangMenu)
        erlangMenu.AppendMenuItem("Options", self, lambda e: self.SetupRuntimes())

        helpMenu = Menu()
        helpMenu.AppendMenuItem("Check for updates", self, self.OnHelpCheckForUpdates)
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def SetupProjectMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = Menu()

        projectsMenu = Menu()
        projectsMenu.AppendMenuItem('Erlang', self, self.OnNewErlangProject)

        self.fileMenu.AppendMenu(wx.NewId(), "New project", projectsMenu)
        self.fileMenu.AppendMenuItem('Open Project', self, self.OnOpenProject)

        if Config.LastProjects():
            lastProjects = Menu()
            self.fileMenu.AppendMenu(wx.NewId(), "Recent projects", lastProjects)
            def handler(p):
                return lambda e: self.OpenProject(p)
            for p in Config.LastProjects():
                if os.path.isfile(p):
                    lastProjects.AppendMenuItem(os.path.basename(p), self, handler(p))

        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('User Settings', self, self.OnEditOptions)
        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('Quit', self, self.OnQuit)
        self.menubar.Append(self.fileMenu, '&File')

        self.editorMenu = Menu()
        self.menubar.Append(self.editorMenu, '&Edit')

        self.projectMenu = Menu()
        self.menubar.Append(self.projectMenu, '&Project')

        languagesMenu = Menu()
        self.menubar.Append(languagesMenu, "&Languages")
        erlangMenu = Menu()
        languagesMenu.AppendMenu(wx.NewId(), 'Erlang', erlangMenu)
        erlangMenu.AppendMenuItem("Options", self, lambda e: self.SetupRuntimes())
        erlangMenu.AppendSeparator()

       # erlangMenu.AppendSeparator()
        self.erlangMenu = erlangMenu

        self.viewMenu = Menu()
        self.viewMenu.AppendCheckMenuItem('Show white space', self, self.OnMenuShowWhiteSpace, Config.GetProp("show_white_space", False))
        self.viewMenu.AppendCheckMenuItem('Show EOL', self, self.OnMenuShowEOL, Config.GetProp("show_eol", False))
        self.viewMenu.AppendSeparator()
        #self.viewMenu.AppendMenuItem("Log", self.window, lambda e: self.ShowLog())

        self.menubar.Append(self.viewMenu, "&View")

        helpMenu = Menu()
        helpMenu.AppendMenuItem("Check for updates", self, self.OnHelpCheckForUpdates)
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def GetCurrentVersion(self):
        revCfg = os.path.join(self.cwd, "rev.cfg")
        version = 0.1
        if os.path.isfile(revCfg):
            data = readFile(revCfg)
            version = float(data.split("\n")[0].split(":")[1].strip())
        return version

    def OnHelpCheckForUpdates(self, event):
        try:
            self.SetCursor(wx.StockCursor(wx.CURSOR_WAIT))
            version = self.GetCurrentVersion()
            revfile = urllib2.urlopen("https://dl.dropbox.com/s/1a36pmlgmdy4rly/rev.cfg")
            newData = revfile.read()
            newVersion = float(newData.split("\n")[0].split(":")[1].strip())
            dir = os.path.join(self.cwd, "installer")
            if not os.path.isdir(dir):
                os.mkdir(dir)
            if newVersion != version:
                dial = MultiMessageDialog(self,
                    'There is new version {} available. Current version is {}. Do you want to update after exit?'.format(newVersion, version),
                    msg2 = 'Changelog:\n\n' + newData,
                    caption = 'New version {} available'.format(newVersion),
                    #icon = wx.ICON_QUESTION,
                    style = wx.YES_NO | wx.YES_DEFAULT | wx.ICON_QUESTION)
                #dial.SetSize((600, 400))
                if dial.ShowModal() == wx.ID_YES:
                    progressDialog = wx.ProgressDialog("Autoupdater", "Downloading installer...", parent = self, style = wx.PD_APP_MODAL | wx.PD_ELAPSED_TIME | wx.PD_AUTO_HIDE)
                    progressDialog.Show()
                    installerFileName = os.path.join(dir, "installer.zip")
                    installerFile = open(installerFileName, 'wb')
                    dataFile = urllib2.urlopen("https://dl.dropbox.com/s/a2qrs1zw20who93/noiseide.zip")
                    meta = dataFile.info()
                    fileSize = int(meta.getheaders("Content-Length")[0])
                    #print "Downloading: %s Bytes: %s" % (os.path.join(self.cwd, "installer.zip"), fileSize)

                    fileSizeDl = 0
                    block_sz = 8192
                    while True:
                        buffer = dataFile.read(block_sz)
                        if not buffer:
                            break

                        fileSizeDl += len(buffer)
                        installerFile.write(buffer)
                        #status = "Downloading installer... {} / {} done".format(round(fileSizeDl / 1000000.0, 3), round(fileSize / 1000000.0, 3))
                        newValue = int(float(fileSizeDl) / float(fileSize) * 100)
                        progressDialog.Update(newValue)
                    #progressDialog.Destroy()
                    installerFile.close()
                    writeBinaryFile(os.path.join(dir, "rev.cfg"), newData)
                    idn_installer.Decompress(installerFileName)
                    global installNewVersion
                    installNewVersion = True
                    self.Enable()
                    self.SetFocus()
            else:
                wx.MessageBox("You have last version", "Check result")
        except Exception, e:
            idn_global.Log("Update error", e)
            wx.MessageBox("Update check error. Check log for info", "Check result")
        self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
    def ShowLog(self):
        pass#if self.ToolMgr.FindPageIndexByWindow(self.)

    def Log(self, text):
        print text
        self.logFile.write(text)
        self.logFile.flush()
        #self.log.Append(text)

#    def ClearLog(self):
#        self.log.Clear()

    def TryLoadLastProject(self):
        lastProject = Config.GetProp("last_project")
        if lastProject and os.path.isfile(lastProject):
            dial = wx.MessageDialog(None,
                'Do you want to open last project {}?'.format(os.path.basename(lastProject)),
                'Last project',
                wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
            if dial.ShowModal() == wx.ID_YES:
                self.OpenProject(lastProject)

    def CreateToolBar(self):
        self.toolbar = wx.Frame.CreateToolBar(self)
        self.navBackT = self.toolbar.AddLabelTool(wx.NewId(), 'Navigate Back', GetImage('navigateBack.png'), shortHelp = 'Navigate Back')
        self.navForwardT = self.toolbar.AddLabelTool(wx.NewId(), 'Navigate Forward', GetImage('navigateForward.png'), shortHelp = 'Navigate Forward')

        self.Bind(wx.EVT_TOOL, lambda e: self.TabMgr.NavigateBack(), self.navBackT)
        self.Bind(wx.EVT_TOOL, lambda e: self.TabMgr.NavigateForward(), self.navForwardT)

        self.toolbar.Realize()

    def OnMenuShowWhiteSpace(self, event):
        newValue = not Config.GetProp("show_white_space", False)
        Config.SetProp("show_white_space", newValue)
        for editor in self.TabMgr.Pages():
            editor.UpdateOptions()

    def OnMenuShowEOL(self, event):
        newValue = not Config.GetProp("show_eol", False)
        Config.SetProp("show_eol", newValue)
        for editor in self.TabMgr.Pages():
            editor.UpdateOptions()

    def OnEditOptions(self, event):
        form = ConfigEditForm()
        form.ShowModal()

    def OnHelpAbout(self, event):
        wx.MessageBox("IDE with good functionality for Erlang programming language.\nMade by Yaroslav 'IDNoise' Nikityshev.", "Noise IDE v {}".format(self.GetCurrentVersion()))

    def MenuBar(self):
        return self.menubar

    def OnOpenProject(self, event):
        dialog = wx.FileDialog(
            self,
            message = "Select project",
            wildcard = "*.noiseide",
            style = wx.FD_OPEN | wx.FD_FILE_MUST_EXIST
        )
        if dialog.ShowModal() == wx.ID_OK:
            file = dialog.GetPath()
            self.OpenProject(file)
        dialog.Destroy()

    def OpenProject(self, projectFile):
        if self.project:
            self.project.Close()
        #self.ClearLog()
        projectFile = os.path.normcase(projectFile)
        Config.SetProp("last_project", projectFile)

        projects = Config.LastProjects()
        if projectFile in projects:
            projects.remove(projectFile)
        projects.append(projectFile)
        Config.SetLastProjects(projects)

        self.SetupProjectMenu()
        self.LoadProject(projectFile)

        #self.project.mEditProject.Enable(True)
        self.SetTitle(self.project.ProjectName() + " - " + "Noise IDE")


    def LoadProject(self, filePath):
        projectData = yaml.load(file(filePath, 'r'))
        type = projectData[Project.CONFIG_PROJECT_TYPE]
        return Project.TYPE_PROJECT_DICT[type](self, filePath, projectData)

    def OnNewErlangProject(self, event):
        self.CheckRuntimes()
        ErlangProjectFrom().ShowModal()

    def OnClose(self, event):
        if self.project:
            self.project.Close()
        Config.save()
        event.Skip()

    def OnQuit(self, event):
        self.Close()

    def CheckRuntimes(self):
        availableRuntimes = {}
        if Config.Runtimes():
            for r in Config.Runtimes():
                if  os.path.isfile(Config.Runtimes()[r]):
                    availableRuntimes[r] = Config.Runtimes()[r]

        Config.SetProp(Config.RUNTIMES, availableRuntimes)

        while not Config.Runtimes():
            wx.MessageBox("Add at least one erlang runtime!", "Error")
            self.SetupRuntimes()

    def SetupRuntimes(self):
        dlg = ErlangOptionsDialog(self)
        dlg.ShowModal()
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
    try:
        main()
        if installNewVersion:
            os.startfile("noiseide_copy.bat")
            #os.spawnlp(os.P_NOWAIT, , "noiseide_copy")
#            installerDir = os.path.join(os.getcwd(), 'installer')
#            os.rmdir(installerDir)
#            import shutil
#            root_src_dir =
#            root_dst_dir = os.getcwd()
#            for src_dir, dirs, files in os.walk(root_src_dir):
#                dst_dir = src_dir.replace(root_src_dir, root_dst_dir)
#                if not os.path.exists(dst_dir):
#                    os.mkdir(dst_dir)
#                for file_ in files:
#                    src_file = os.path.join(src_dir, file_)
#                    dst_file = os.path.join(dst_dir, file_)
#                    if os.path.exists(dst_file):
#                        os.remove(dst_file)
#                    print src_file
#                    shutil.move(src_file, dst_dir)


    except Exception, e:
        with open("ide.log", 'a') as logFile:
            logFile.write("app error" + str(e))
