import time

__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import idn_projectexplorer
import idn_connect as connect

class Project:
    EXPLORER_TYPE = idn_projectexplorer.ProjectExplorer
    def __init__(self, filePath, projectData):
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData

        self.OnLoadProject()

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self, parent):
        explorer = self.EXPLORER_TYPE(parent)
        explorer.SetRoot(self.projectDir)
        return explorer

    def Close(self):
        pass

class ErlangProject(Project):
    EXPLORER_TYPE = idn_projectexplorer.ErlangProjectExplorer

    def OnLoadProject(self):
        #connect.ErlangProcess()
        self.shell = connect.ErlangSubprocess(os.getcwd(), [])
        self.shell.Start()
        #self.erlangShell.ExecCommand(u'[io:format("~p~n", [V]) || V <- lists:seq(1, 1000)].')
        #self.erlangShell.ExecCommand(u'[io:format("~p~n", [V]) || V <- lists:seq(1000, 3000)].')
        #print "loading erlang project"

    def Close(self):
        self.shell.Stop()

def loadProject(filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](filePath, projectData)