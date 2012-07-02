__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import idn_projectexplorer as exp
from wx.lib.agw import aui
from idn_console import ErlangIDEConsole, ErlangProjectConsole

class Project:
    EXPLORER_TYPE = exp.ProjectExplorer
    def __init__(self, window, filePath, projectData):
        self.window = window
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData

        self.CreateExplorer()
        self.OnLoadProject()

    def ProjectName(self):
        return self.projectData["project_name"]

    def AppsPath(self):
        return os.path.join(self.projectDir, self.projectData["apps_dir"])

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorer = self.EXPLORER_TYPE(self.window)
        self.explorer.SetRoot(self.projectDir)
        self.window.WinMgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Explorer")
        .MinimizeButton().CloseButton(False).BestSize2(300, 600))

    def Close(self):
        pass

class ErlangProject(Project):
    EXPLORER_TYPE = exp.ErlangProjectExplorer

    def OnLoadProject(self):
        #connect.ErlangProcess()
        self.AddConsoles()

        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)


    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr)
        self.shellConsole.shell.SetProp("project_dir", self.AppsPath())
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        cacheDir = os.path.join(os.getcwd(), "cache", "erlang")
        if not os.path.isdir(cacheDir):
            os.makedirs(cacheDir)
        self.shellConsole.shell.SetProp("cache_dir", cacheDir)

        self.window.ToolMgr.AddPage(self.shellConsole, "IDE Console")

        self.consoles = {}
        consoles = self.projectData["consoles"]
        print consoles
        for title in consoles:
            print title
            data = consoles[title]
            params = []
            params.append("-sname " + data["sname"])
            params.append("-cookie " + data["cookie"])
            params.append("-config " + data["config"])

            dirs = ""
            for dir in os.listdir(self.AppsPath()):
                appPath = os.path.join(self.AppsPath(), dir)
                if os.path.isdir(appPath):
                    dirs += ' "{}"'.format(os.path.join(appPath, "ebin"))
            dirs += ' "{}"'.format(os.path.join(os.getcwd(), 'data', 'erlang', 'modules'))

            params.append("-pa " + dirs)
            #print params
            self.consoles[title] = ErlangProjectConsole(self.window.ToolMgr, self.AppsPath(), params)
            self.consoles[title].SetStartCommand(data["command"])
            self.window.ToolMgr.AddPage(self.consoles[title], '<{}> Console'.format(title))

    def Close(self):
        self.shellConsole.Stop()

    def OnProjectFileModified(self, event):
        self.shellConsole.shell.CompileFile(event.File)
        print event.File

def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)