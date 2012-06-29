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
            file = os.path.join(root, file)
            mtime = os.stat(file)[ST_MTIME]
            if os.path.isdir(file):
                self.dirs[file] = mtime
                if self.recursive:
                    self.GetDirInfo(file)
            else:
                self.files[file] = mtime

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