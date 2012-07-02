import time

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
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules')
    EXPLORER_TYPE = exp.ErlangProjectExplorer

    def OnLoadProject(self):
        self.SetupDirs()
        self.AddConsoles()
        self.CompileProject()
        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)

    def SetupDirs(self):
        self.cacheDir = os.path.join(os.getcwd(), "cache", "erlang")
        erlangLibsCacheDir =  os.path.join(self.cacheDir, "erlang")
        otherCacheDir =  os.path.join(self.cacheDir, "other")
        projectCacheDir =  os.path.join(self.cacheDir, self.ProjectName())
        for dir in [self.cacheDir, erlangLibsCacheDir, otherCacheDir, projectCacheDir]:
            if not os.path.isdir(dir):
                os.makedirs(dir)

    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr, self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", self.cacheDir)
        self.shellConsole.shell.SetProp("project_dir", self.AppsPath())
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        #time.sleep(0.1)
        print "setting props"
        self.window.ToolMgr.AddPage(self.shellConsole, "IDE Console")

        self.consoles = {}
        consoles = self.projectData["consoles"]
        #print consoles

        dirs = ""
        for app in self.projectData["apps"]:
            appPath = os.path.join(self.AppsPath(), app)
            if os.path.isdir(appPath):
                ebinDir = os.path.join(appPath, "ebin")
                self.shellConsole.shell.AddPath(ebinDir)
                dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)

        for title in consoles:
            print title
            data = consoles[title]
            params = []
            params.append("-sname " + data["sname"])
            params.append("-cookie " + data["cookie"])
            params.append("-config " + data["config"])

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

    def CompileProject(self):
        print "compile project"
        filesToCompile = set()
        filesToCache = set()
        for app in self.projectData["apps"]:
            srcPath = os.path.join(os.path.join(self.AppsPath(), app), "src")
            includePath = os.path.join(os.path.join(self.AppsPath(), app), "include")
            for path in [srcPath, includePath]:
                for root, _, files in os.walk(path):
                    for file in files:
                        file = os.path.join(root, file)
                        if file.endswith(".erl"):
                            filesToCompile.add((app, file))
                        elif file.endswith(".hrl"):
                            filesToCache.add(file)
        filesToCompile = sorted(list(filesToCompile))
        filesToCache = sorted(list(filesToCache))
        for (app, file) in filesToCompile:
            self.shellConsole.shell.CompileProjectFile(file, app)
        for file in filesToCache:
            self.shellConsole.shell.GenerateFileCache(file)


def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)