from idn_cache import ErlangCache
from idn_directoryinfo import DirectoryChecker

__author__ = 'Yaroslav Nikityshev aka IDNoise'

#from idn_utils import extension
import os
import yaml
import time
import idn_projectexplorer as exp
from wx.lib.agw import aui
from idn_console import ErlangIDEConsole, ErlangProjectConsole

class Project:
    EXPLORER_TYPE = exp.ProjectExplorer
    USER_DATA_FOLDER = os.path.join(os.getcwd(), 'userdata')

    CONFIG_LAST_OPENED_FILES = "last_opened_files"
    CONFIG_HIDDEN_PATHS = "hidden_paths"

    def __init__(self, window, filePath, projectData):
        self.window = window
        self.projectFilePath = filePath
        self.projectDir = os.path.dirname(filePath)
        self.projectData = projectData

        if not os.path.isdir(self.USER_DATA_FOLDER):
            os.makedirs(self.USER_DATA_FOLDER)

        self.userDataFile = os.path.join(self.USER_DATA_FOLDER, "{}.project.user".format(self.ProjectName()))
        if os.path.isfile(self.userDataFile):
            self.userData = yaml.load(open(self.userDataFile))
        else:
            self.userData = {}

        self.CreateExplorer()
        self.OnLoadProject()
        self.OpenLastFiles()

    def ProjectName(self):
        return self.projectData["project_name"]

    def LastOpenedFiles(self):
        if self.CONFIG_LAST_OPENED_FILES in self.userData:
            return self.userData[self.CONFIG_LAST_OPENED_FILES]
        else:
            return []

    def HiddenPathsList(self):
        if self.CONFIG_HIDDEN_PATHS in self.userData:
            return set(self.userData[self.CONFIG_HIDDEN_PATHS])
        else:
            return set()

    def OpenLastFiles(self):
        for file in self.LastOpenedFiles():
            self.window.TabMgr.LoadFile(file)

    def AppsPath(self):
        return os.path.join(self.projectDir, self.projectData["apps_dir"])

    def OnLoadProject(self):
        raise NotImplementedError

    def CreateExplorer(self):
        self.explorer = self.EXPLORER_TYPE(self.window)
        self.explorer.SetRoot(self.projectDir)
        self.explorer.SetHiddenList(self.HiddenPathsList())
        self.window.WinMgr.AddPane1(self.explorer, aui.AuiPaneInfo().Left().Caption("Project Explorer")
            .MinimizeButton().CloseButton(False).BestSize2(300, 600))
        self.window.WinMgr.Update()

    def Close(self):
        self.explorer.StopTrackingProject()
        self.SaveUserData()

    def SaveUserData(self):
        #print "save user data"
        openedFiles = []
        for path in self.window.TabMgr.OpenedFiles():
            if path.startswith(self.projectDir):
                openedFiles.append(path)
        self.userData[self.CONFIG_LAST_OPENED_FILES] = openedFiles
        self.userData[self.CONFIG_HIDDEN_PATHS] = self.explorer.hiddenPaths
        #print(self.userData[self.CONFIG_LAST_OPENED_FILES])
        #print(self.userData[self.CONFIG_HIDDEN_PATHS])
        yaml.dump(self.userData, open(self.userDataFile, 'w'))


class ErlangProject(Project):
    IDE_MODULES_DIR = os.path.join(os.getcwd(), 'data', 'erlang', 'modules')
    EXPLORER_TYPE = exp.ErlangProjectExplorer


    def OnLoadProject(self):
        ErlangCache.Init()
        self.SetupDirs()
        self.AddConsoles()
        #self.GenerateErlangCache() #test

        #self.CompileProject() #test

        self.explorer.Bind(exp.EVT_PROJECT_FILE_MODIFIED, self.OnProjectFileModified)
        ErlangCache.LoadCacheFromDir("erlang")
        ErlangCache.LoadCacheFromDir(self.ProjectName())
        ErlangCache.StartCheckingFolder(self.ProjectName())

    def SetupDirs(self):
        projectCacheDir =  os.path.join(ErlangCache.CACHE_DIR, self.ProjectName())
        if not os.path.isdir(projectCacheDir):
            os.makedirs(projectCacheDir)

    def GenerateErlangCache(self):
        self.shellConsole.shell.GenerateErlangCache()

    def AddConsoles(self):
        self.shellConsole = ErlangIDEConsole(self.window.ToolMgr, self.IDE_MODULES_DIR)
        self.shellConsole.shell.SetProp("cache_dir", ErlangCache.CACHE_DIR)
        self.shellConsole.shell.SetProp("project_dir", self.AppsPath())
        self.shellConsole.shell.SetProp("project_name", self.ProjectName())
        self.window.ToolMgr.AddPage(self.shellConsole, "IDE Console")

        self.consoles = {}
        consoles = self.projectData["consoles"]

        dirs = ""
        for app in self.projectData["apps"]:
            appPath = os.path.join(self.AppsPath(), app)
            if os.path.isdir(appPath):
                ebinDir = os.path.join(appPath, "ebin")
                self.shellConsole.shell.AddPath(ebinDir)
                dirs += ' "{}"'.format(ebinDir)
        dirs += ' "{}"'.format(self.IDE_MODULES_DIR)

        for title in consoles:
            data = consoles[title]
            params = []
            params.append("-sname " + data["sname"])
            params.append("-cookie " + data["cookie"])
            params.append("-config " + data["config"])

            params.append("-pa " + dirs)
            self.consoles[title] = ErlangProjectConsole(self.window.ToolMgr, self.AppsPath(), params)
            self.consoles[title].SetStartCommand(data["command"])
            self.window.ToolMgr.AddPage(self.consoles[title], '<{}> Console'.format(title))

    def Close(self):
        ErlangCache.StopCheckingFolder(self.ProjectName())
        Project.Close(self)
        self.shellConsole.Stop()
        for title, console in self.consoles.items():
            console.Stop()

    def OnProjectFileModified(self, event):
        file = event.File
        if file.endswith(".erl"):
            self.shellConsole.shell.CompileFile(file)
        elif file.endswith(".hrl"):
            self.shellConsole.shell.GenerateFileCache(file)
        #print event.File

    def CompileProject(self):
        #print "compile project"
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
                            filesToCompile.add((file, app))
                        elif file.endswith(".hrl"):
                            filesToCache.add(file)
        filesToCompile = sorted(list(filesToCompile))
        filesToCache = sorted(list(filesToCache))
        self.shellConsole.shell.CompileProjectFiles(filesToCompile)
        self.shellConsole.shell.GenerateFileCaches(filesToCache)
        self.RemoveUnusedBeams()

    def RemoveUnusedBeams(self):
        srcFiles = set()
        for app in self.projectData["apps"]:
            path = os.path.join(self.AppsPath(), app, "src")
            for root, _, files in os.walk(path):
                for fileName in files:
                    (file, ext) = os.path.splitext(fileName)
                    if ext == ".erl":
                        srcFiles.add(file)
            path = os.path.join(self.AppsPath(), app, "ebin")
            for root, _, files in os.walk(path):
                for fileName in files:
                    (file, ext) = os.path.splitext(fileName)
                    if ext == ".beam":
                        if file not in srcFiles:
                            os.remove(os.path.join(root, fileName))


def loadProject(window, filePath):
    TYPE_PROJECT_DICT = {
        "erlang": ErlangProject
    }
    projectData = yaml.load(file(filePath, 'r'))
    type = projectData["project_type"]
    return TYPE_PROJECT_DICT[type](window, filePath, projectData)