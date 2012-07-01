import time
from idn_console import ErlangIDEConsole

__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import idn_projectexplorer

class Project:
    EXPLORER_TYPE = idn_projectexplorer.ProjectExplorer
    def __init__(self, window, filePath, projectData):
        self.window = window
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData

        self.OnLoadProject()

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        explorer = self.EXPLORER_TYPE(self.window)
        explorer.SetRoot(self.projectDir)
        return explorer

    def Close(self):
        pass

class ErlangProject(Project):
    EXPLORER_TYPE = idn_projectexplorer.ErlangProjectExplorer

    def OnLoadProject(self):
        #connect.ErlangProcess()
        self.AddConsoles()



    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr)
        self.window.ToolMgr.AddPage(self.shellConsole, "IDE Console")

    def Close(self):
        self.shellConsole.Stop()

def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)