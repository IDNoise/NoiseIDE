import ide_migrations
from idn_erlang_constats import SINGLE_APP_PROJECT, MULTIPLE_APP_PROJECT, CONFIG_PROJECT_TYPE

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import sys
import urllib2
from wx.lib.dialogs import  MultiMessageDialog
import yaml
from idn_erlang_dialogs import ErlangOptionsDialog
from idn_erlang_project import ErlangProject, LoadProject
from idn_erlang_project_form import ErlangProjectFrom
import idn_installer
from idn_utils import Menu, GetImage, readFile, writeBinaryFile, Timer, CreateButton, CreateLabel
from idn_shortcut_window import ShortcutWindow
import os
import wx
from wx.lib.agw import aui
from idn_colorschema import ColorSchema
from idn_winmanager import Manager
from idn_notebook import  Notebook, EditorNotebook
from idn_config import Config, ConfigEditForm
import core
from idn_project import Project
import traceback
import asyncore
from threading import Thread, Event
#lists:flatten(edoc:read("d:/projects/noiseide/noiseidepython/data/erlang/modules/noiseide/src/test_cache_module.erl")).

installNewVersion = False

import sys
reload(sys)
sys.setdefaultencoding("utf-8")

class AsyncoreThread(Thread):
    def __init__(self):
        Thread.__init__(self)
        self.active = Event()

    def Start(self):
        self.active.set()
        self.start()

    def Stop(self):
        self.active.clear()

    def run(self):
        while self.active.is_set():
            asyncore.poll(0.1)

class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (1680, 900), pos = (10, 10))

        self.asyncthread = AsyncoreThread()
        self.asyncthread.Start()

        self.cwd = os.getcwd()
        core.MainFrame = self
        wx.ToolTip.SetMaxWidth(600)

        self.Maximize()
        Config.load()

        ColorSchema.load(Config.ColorSchema())

        icon = wx.Icon('data/images/icon.png', wx.BITMAP_TYPE_PNG, 16, 16)
        self.SetIcon(icon)

        self.explorer = None
        self.project = None

        agwFlags = aui.AUI_MGR_DEFAULT | aui.AUI_MGR_AUTONB_NO_CAPTION | aui.AUI_MGR_LIVE_RESIZE
        self.WinMgr = Manager(self, agwFlags = agwFlags )

        self.SetupSimpleMenu()
        self.CreateToolBar()

        Project.TYPE_PROJECT_DICT["erlang"] = ErlangProject


        self.TabMgr = EditorNotebook(self)
        self.TabMgr.SetArtProvider(aui.VC71TabArt())
        self.TabMgrPaneInfo = aui.AuiPaneInfo().Center()\
            .MaximizeButton().MinimizeButton().CaptionVisible(False)\
            .CloseButton(False).Floatable(False).MinSize(100, 100)
        self.WinMgr.AddPane1(self.TabMgr, self.TabMgrPaneInfo )

        self.ToolMgr = Notebook(self)
        self.ToolMgrPaneInfo = aui.AuiPaneInfo().Bottom()\
            .MaximizeButton().MinimizeButton().CloseButton(False).Floatable(False)\
            .BestSize(400, 300).MinSize(100, 100).Name("Tools").Caption("Tools").CaptionVisible(True)\
            .MinimizeMode(aui.AUI_MINIMIZE_POS_BOTTOM | aui.AUI_MINIMIZE_CAPT_SMART)
        self.WinMgr.AddPane1(self.ToolMgr, self.ToolMgrPaneInfo)
        core.TabMgr = self.TabMgr
        core.ToolMgr = self.ToolMgr
        core.WinMgr = self.WinMgr

        self.WinMgr.Update()

        self.Bind(wx.EVT_CLOSE, self.OnClose)

        self.WinMgr.Update()

        def newVersionChecker():
            self.OnHelpCheckForUpdates(None, True)

        self.autoCheckTimer = Timer(600, newVersionChecker)
        self.autoCheckTimer.Start()

        args = sys.argv[1:]
        wx.CallAfter(self.OnAfterLoad, args)

    def async(self):
        while True:
            asyncore.loop()

    def OnAfterLoad(self, args):
        newVersion = Config.GetCurrentVersion()
        current = Config.GetProp("current_version", 0)

        if current < newVersion:
            self.apply_migration(current)
        Config.SetProp("current_version", newVersion)

        if args:
            path = args[0]
            if os.path.isfile(path) and path.endswith(".noiseide"):
                self.OpenProject(path)
                return
        self.TryLoadLastProject()

    def apply_migration(self, version):
        for v, fun in ide_migrations.MIGRATIONS.items():
            if v > version:
                fun()

    def SetupSimpleMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = Menu()

        self.fileMenu.AppendMenuItem('New project', self, self.OnNewErlangProject)
        self.fileMenu.AppendMenuItem('Open Project', self, self.OnOpenProject)

        if Config.LastProjects():
            lastProjects = Menu()
            self.fileMenu.AppendMenu(wx.ID_ANY, "Recent projects", lastProjects)
            def handler(p):
                return lambda e: self.OpenProject(p)
            for p in reversed(Config.LastProjects()):
                projectData = yaml.load(file(p, 'r'))
                lastProjects.AppendMenuItem(projectData[Project.CONFIG_PROJECT_NAME], self, handler(p))

        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('User Settings', self, self.OnEditOptions)
        self.fileMenu.AppendSeparator()
        self.fileMenu.AppendMenuItem('Quit', self, self.OnQuit)
        self.menubar.Append(self.fileMenu, '&File')


        languagesMenu = Menu()
        self.menubar.Append(languagesMenu, "&Languages")
        erlangMenu = Menu()
        languagesMenu.AppendMenu(wx.ID_ANY, 'Erlang', erlangMenu)
        erlangMenu.AppendMenuItem("Options", self, lambda e: self.SetupRuntimes())

        helpMenu = Menu()
        helpMenu.AppendMenuItem("Shorcuts", self, self.OnHelpShortcuts)
        helpMenu.AppendMenuItem("Check for updates", self, self.OnHelpCheckForUpdates)
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def SetupProjectMenu(self):
        self.menubar = wx.MenuBar()
        self.fileMenu = Menu()

        projectsMenu = Menu()
        projectsMenu.AppendMenuItem('Erlang', self, self.OnNewErlangProject)

        self.fileMenu.AppendMenu(wx.ID_ANY, "New project", projectsMenu)
        self.fileMenu.AppendMenuItem('Open Project', self, self.OnOpenProject)

        if Config.LastProjects():
            lastProjects = Menu()
            self.fileMenu.AppendMenu(wx.ID_ANY, "Recent projects", lastProjects)
            def handler(p):
                return lambda e: self.OpenProject(p)
            for p in Config.LastProjects():
                projectData = yaml.load(file(p, 'r'))
                lastProjects.AppendMenuItem(projectData[Project.CONFIG_PROJECT_NAME], self, handler(p))

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
        languagesMenu.AppendMenu(wx.ID_ANY, 'Erlang', erlangMenu)
        erlangMenu.AppendMenuItem("Options", self, lambda e: self.SetupRuntimes())
        erlangMenu.AppendSeparator()


        self.erlangMenu = erlangMenu

        self.viewMenu = Menu()
        self.viewMenu.AppendCheckMenuItem('Show white space', self, self.OnMenuShowWhiteSpace, Config.GetProp("show_white_space", False))
        self.viewMenu.AppendCheckMenuItem('Show EOL', self, self.OnMenuShowEOL, Config.GetProp("show_eol", False))
        self.viewMenu.AppendSeparator()

        self.menubar.Append(self.viewMenu, "&View")

        helpMenu = Menu()
        helpMenu.AppendMenuItem("Shorcuts", self, self.OnHelpShortcuts)
        helpMenu.AppendMenuItem("Check for updates", self, self.OnHelpCheckForUpdates)
        helpMenu.AppendMenuItem("About", self, self.OnHelpAbout)
        self.menubar.Append(helpMenu, '&Help')
        self.SetMenuBar(self.menubar)

    def OnHelpShortcuts(self, event):
        ShortcutWindow(self).Show()

    def OnHelpCheckForUpdates(self, event, auto = False):
        try:
            if not auto:
                self.SetCursor(wx.StockCursor(wx.CURSOR_WAIT))
            version = Config.GetCurrentVersion()
            revfile = urllib2.urlopen("https://dl.dropbox.com/s/8hg4b53tugmgbe0/rev.cfg")
            newData = revfile.read()
            newVersion = float(newData.split("\n")[0].split(":")[1].strip())
            self.autoCheckTimer.Stop()
            if newVersion > version:
                dial = MultiMessageDialog(self,
                    'There is new version {} available. Current version is {}. Do you want to install?'.format(newVersion, version),
                    msg2 = 'Changelog:\n\n' + newData,
                    caption = 'New version {} available'.format(newVersion),
                    style = wx.YES_NO | wx.ICON_QUESTION)
                if dial.ShowModal() == wx.ID_YES:
                    progressDialog = wx.ProgressDialog("Autoupdater", "Downloading installer...", parent = self, style = wx.PD_APP_MODAL | wx.PD_ELAPSED_TIME | wx.PD_AUTO_HIDE)
                    progressDialog.Show()
                    installDir = os.path.join(core.TempDir(), "installer")
                    if not os.path.isdir(installDir):
                        os.mkdir(installDir)
                    installerFileName = os.path.join(installDir, "NoiseIDE.msi")
                    installerFile = open(installerFileName, 'wb')
                    dataFile = urllib2.urlopen("https://dl.dropbox.com/s/u1esqq4h68qufcz/NoiseIDE.msi")
                    meta = dataFile.info()
                    fileSize = int(meta.getheaders("Content-Length")[0])

                    fileSizeDl = 0
                    block_sz = 8192
                    while True:
                        buf = dataFile.read(block_sz)
                        if not buf:
                            break

                        fileSizeDl += len(buf)
                        installerFile.write(buf)
                        newValue = int(float(fileSizeDl) / float(fileSize) * 100)
                        progressDialog.Update(newValue)
                    installerFile.close()
                    writeBinaryFile(os.path.join(installDir, "rev.cfg"), newData)
                    #idn_installer.Decompress(installerFileName)
                    global installNewVersion
                    installNewVersion = True
                    self.Close()
            elif not auto:
                wx.MessageBox("You have last version", "Check new version result")
        except Exception, e:
            core.Log("Update error", e)
            if not auto:
                wx.MessageBox("Update check error. Check log for info", "Check new version result")
        if not auto:
            self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))

    def TryLoadLastProject(self):
        dialog = HelloDialog(self)
        dialog.Show()

        #lastProject = Config.GetProp("last_project")
        #if lastProject and os.path.isfile(lastProject):
            #dial = wx.MessageDialog(None,
                #'Do you want to open last project {}?'.format(os.path.basename(lastProject)),
                #'Last project',
                #wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
            #if dial.ShowModal() == wx.ID_YES:
                #self.OpenProject(lastProject)

    def CreateToolBar(self):
        self.toolbar = wx.Frame.CreateToolBar(self)
        self.navBackT = self.toolbar.AddLabelTool(wx.ID_ANY, 'Navigate Back', GetImage('navigateBack.png'), shortHelp = 'Navigate Back')
        self.navForwardT = self.toolbar.AddLabelTool(wx.ID_ANY, 'Navigate Forward', GetImage('navigateForward.png'), shortHelp = 'Navigate Forward')

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
        wx.MessageBox("IDE for Erlang programming language.\nMade by Yaroslav 'IDNoise' Nikityshev.", "Noise IDE v {}".format(Config.GetCurrentVersion()))

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
            filePath = dialog.GetPath()
            self.OpenProject(filePath)
        dialog.Destroy()

    def OpenProject(self, projectFile):
        if self.project:
            self.project.Close()
        projectFile = os.path.normpath(projectFile)
        Config.SetProp("last_project", projectFile)
        projects = Config.LastProjects()
        if projectFile in projects:
            projects.remove(projectFile)
        projects.append(projectFile)
        Config.SetLastProjects(projects)
        self.SetupProjectMenu()
        self.project = LoadProject(projectFile)

        self.SetTitle(self.project.ProjectName() + " - " + "Noise IDE")

    def OnNewErlangProject(self, event):
        self.CheckRuntimes()
        dlg = wx.SingleChoiceDialog(self, "Select project type", "Project type", [SINGLE_APP_PROJECT, MULTIPLE_APP_PROJECT])
        if dlg.ShowModal() == wx.ID_CANCEL:
            return

        ErlangProjectFrom(None, dlg.GetStringSelection()).ShowModal()

    def OnClose(self, event):
        self.asyncthread.Stop()
        if self.project:
            self.project.Close()
        Config.save()
        self.autoCheckTimer.Stop()
        for wnd in wx.GetTopLevelWindows():
            if wnd != self:
                wnd.Close(True)
        event.Skip()

    def OnQuit(self, event):
        self.Close(True)

    def CheckRuntimes(self):
        #Config.SetProp(Config.RUNTIMES, availableRuntimes)

        while not Config.AvailableRuntimes():
            if not Config.Runtimes():
                wx.MessageBox("Add at least one erlang runtime!", "Error")
            else:
                wx.MessageBox("Specify at least one proper path to erl executable.", "Error")
            self.SetupRuntimes(True)

    def SetupRuntimes(self, atLeastOneRequired = False):
        dlg = ErlangOptionsDialog(self, atLeastOneRequired)
        dlg.ShowModal()

class HelloDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, title = "Hello, " + Config.GetProp(Config.USER_NAME),
            style = wx.DEFAULT_DIALOG_STYLE)

        self.recentLB = wx.ListCtrl(self, -1, style = wx.LC_REPORT | wx.LC_NO_HEADER | wx.LC_ALIGN_LEFT)
        self.recentLB.SetMinSize((200, 200))
        self.recentLB.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnClickRecent)

        self.recentLB.InsertColumn(0, "data")
        self.recentLB.SetColumnWidth(0, 200)
        i = 0
        self.navigation = {}
        for p in reversed(Config.LastProjects()):
            projectData = yaml.load(file(p, 'r'))
            self.recentLB.InsertStringItem(i, projectData[Project.CONFIG_PROJECT_NAME])
            self.navigation[i] = p
            i += 1

        self.createNewB = CreateButton(self, "New Project", self.OnCreateNew)
        self.createNewB.MinSize = (100, 30)

        self.openOtherB = CreateButton(self, "Open Project", self.OnOpenOther)
        self.openOtherB.MinSize = (100, 30)

        gSizer = wx.GridBagSizer(2, 2)

        gSizer.Add(CreateLabel(self, "Open recent:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.recentLB, (1, 0), (4, 1), flag = wx.ALL | wx.ALIGN_CENTER | wx.EXPAND, border = 4)

        gSizer.Add(CreateLabel(self, "Create new:"), (0, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.createNewB, (1, 1), flag = wx.ALL | wx.ALIGN_TOP, border = 4)

        gSizer.Add(CreateLabel(self, "Open other:"), (2, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 4)
        gSizer.Add(self.openOtherB, (3, 1), flag = wx.ALL | wx.ALIGN_TOP, border = 4)

        self.SetSizer(gSizer)
        self.Layout()
        gSizer.SetSizeHints(self)

    def OnCreateNew(self, event):
        self.Close()
        self.Parent.OnNewErlangProject(None)

    def OnClickRecent(self, event):
        project = self.navigation[event.GetIndex()]
        self.Parent.OpenProject(project)
        self.Close()

    def OnOpenOther(self, event):
        self.Close()
        self.Parent.OnOpenProject(None)

class App(wx.App):
    def __init__(self):
        wx.App.__init__(self, redirect=False)
        self.SetAppName("NoiseIDE")
        core.App = self
        wx.Log.SetLogLevel(0)
        frame = NoiseIDE()
        frame.Show()

if __name__ == '__main__':
    def excepthook(type, value, tb):
        message = 'Uncaught exception:\n'
        message += ''.join(traceback.format_exception(type, value, tb))
        core.Log(message)

    sys.excepthook = excepthook

    def main():
        app = App()
        app.MainLoop()

    try:
        main()
        if installNewVersion:
            core.Log("start install")

            import pythoncom
            import win32com
            import win32com.client
            command = os.path.join(core.TempDir(), "installer", "NoiseIDE.msi")
            if sys.platform == "win32":
                pythoncom.CoInitialize()
                shell = win32com.client.Dispatch('WScript.Shell')
                shell.Run(command, 1, False)
                pythoncom.CoUninitialize()

    except Exception, e:
        core.Log("app error" + str(e))
