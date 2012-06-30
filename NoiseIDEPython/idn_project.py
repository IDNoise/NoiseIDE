__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import idn_projectexplorer


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

class ErlangProject(Project):
    EXPLORER_TYPE = idn_projectexplorer.ErlangProjectExplorer

    def OnLoadProject(self):
        print "loading erlang project"


def loadProject(filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](filePath, projectData)