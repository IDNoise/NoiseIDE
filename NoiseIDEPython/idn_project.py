__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import yaml



class Project:
    def __init__(self, filePath, projectData):
        self.filePath = filePath
        self.projectData = projectData
        self.OnLoadProject()

    def OnLoadProject(self):
        raise NotImplementedError

class ErlangProject(Project):
    def OnLoadProject(self):
        print "loading erlang project"


def LoadProject(filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](filePath, projectData)