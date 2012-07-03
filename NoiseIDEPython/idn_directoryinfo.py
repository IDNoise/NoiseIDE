from idn_utils import Timer

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from stat import ST_MTIME

class DirectoryInfo:
    def __init__(self, root, recursive = True):
        self.recursive = recursive
        self.root = root
        self.files = {}
        self.dirs = {}
        self.GetDirInfo(root)

    def GetDirInfo(self, root):
        files = os.listdir(root)
        for file in files:
            try:
                file = os.path.join(root, file)
                mtime = os.stat(file)[ST_MTIME]
                if os.path.isdir(file):
                    self.dirs[file] = mtime
                    if self.recursive:
                        self.GetDirInfo(file)
                else:
                    self.files[file] = mtime
            except Exception, e:
                print e

class DirectoryInfoDiff:
    def __init__(self, newState, oldState):
        self.createdFiles = []
        self.createdDirs  = []
        self.modifiedFiles = []
        self.modifiedDirs = []
        self.deletedFiles = []
        self.deletedDirs = []

        (self.createdDirs, self.modifiedDirs, self.deletedDirs) =\
        self.GetNewModDel(newState.dirs, oldState.dirs)
        (self.createdFiles, self.modifiedFiles, self.deletedFiles) =\
        self.GetNewModDel(newState.files, oldState.files)



    def GetNewModDel(self, newDict, oldDict):
        new = []
        modified = []
        deleted = []
        for newDir in newDict:
            if newDir in oldDict:
                if newDict[newDir] > oldDict[newDir]:
                    modified.append(newDir)
                del oldDict[newDir]
            else:
                new.append(newDir)
        for oldDir in oldDict:
            deleted.append(oldDir)
        return (new, modified, deleted)

class DirectoryChecker:
    HANDLER_FILE_CREATED, \
    HANDLER_FILE_MODIFIED, \
    HANDLER_FILE_DELETED, \
    HANDLER_DIR_CREATED, \
    HANDLER_DIR_MODIFIED, \
    HANDLER_DIR_DELETED = range(6)
    HANDLER_TYPES = range(6)

    def __init__(self, interval, root, recursive = True):
        self.root = root
        self.recursive = recursive
        self.handlers = {t : [] for t in self.HANDLER_TYPES}
        self.timer = Timer(interval, self.CheckDirectoryChanges)
        if root:
            self.dirSnapshot = DirectoryInfo(self.root, self.recursive)


    def AddHandler(self, type, fun):
        if type not in self.HANDLER_TYPES: raise Exception("Wrong handler type")
        self.handlers[type] += [fun]

    def RemoveHandler(self, fun):
        for type in self.handlers:
            if fun in self.handlers[type]:
                self.handlers[type].remove(fun)
                return

    def SetInterval(self, interval):
        self.Stop()
        self.timer = Timer(interval, self.CheckDirectoryChanges)
        self.Start()

    def SetRoot(self, root):
        self.Stop()
        self.root = root
        self.dirSnapshot = DirectoryInfo(self.root, self.recursive)
        self.Start()

    def Start(self):
        self.timer.Start()

    def Stop(self):
        self.timer.Stop()

    def CheckDirectoryChanges(self):
        dirSnapshot = DirectoryInfo(self.root, self.recursive)
        diff = DirectoryInfoDiff(dirSnapshot, self.dirSnapshot)
        self.SendEvents(self.HANDLER_DIR_CREATED, diff.createdDirs)
        self.SendEvents(self.HANDLER_DIR_DELETED, diff.deletedDirs)
        self.SendEvents(self.HANDLER_DIR_MODIFIED, diff.modifiedDirs)
        self.SendEvents(self.HANDLER_FILE_CREATED, diff.createdFiles)
        self.SendEvents(self.HANDLER_FILE_MODIFIED, diff.modifiedFiles)
        self.SendEvents(self.HANDLER_FILE_DELETED, diff.deletedFiles)
        self.dirSnapshot = dirSnapshot

    def SendEvents(self, type, values):
        if not values or not self.handlers[type]: return
        for value in values:
            for handler in self.handlers[type]:
                handler(value)
