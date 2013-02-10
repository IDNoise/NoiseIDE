from idn_events import Event

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from stat import ST_MTIME
from idn_utils import Timer, extension
import core
import wx

class DirectoryInfo:
    def __init__(self, root, recursive = True, fileMask = [], excludeDirs = [], excludePaths = []):
        self.recursive = recursive
        self.root = root
        self.files = {}
        self.dirs = {}
        self.fileMask = fileMask
        self.excludeDirs = excludeDirs
        self.excludePaths = excludePaths
        self.GatherDirInfo(root)

    def GatherDirInfo(self, root):
        files = os.listdir(root)
        for f in files:
            try:
                f = os.path.normpath(os.path.join(root, f))
                if self.excludePaths and f in self.excludePaths:
                    continue
                mtime = os.stat(f)[ST_MTIME]
                if os.path.isdir(f):
                    if self.excludeDirs and os.path.basename(f) in self.excludeDirs:
                        continue
                    self.dirs[f] = mtime
                    if self.recursive:
                        self.GatherDirInfo(f)
                else:
                    if self.fileMask and extension(f) not in self.fileMask:
                        continue
                    self.files[f] = mtime
            except Exception, e:
                core.Log("Gather dir info error: ", e)


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

    def __init__(self, interval, root, recursive = True, fileMask = [], excludeDirs = [], excludePaths = []):
        self.root = root
        self.recursive = recursive
        self.fileMask = fileMask
        self.excludeDirs = excludeDirs
        self.excludePaths = excludePaths
        self.timer = Timer(interval, self.CheckDirectoryChanges)
        if root:
            self.dirSnapshot = self.GetDirectoryInfo()

        self.FilesCreatedEvent = Event()
        self.FilesModifiedEvent = Event()
        self.FilesDeletedEvent = Event()

        self.DirsCreatedEvent = Event()
        self.DirsModifiedEvent = Event()
        self.DirsDeletedEvent = Event()

    def GetDirectoryInfo(self):
        info = DirectoryInfo(self.root, self.recursive, self.fileMask, self.excludeDirs, self.excludePaths)
        self.files = info.files.keys()
        return info

    def SetInterval(self, interval):
        self.Stop()
        self.timer = Timer(interval, self.CheckDirectoryChanges)
        self.Start()

    def SetRoot(self, root):
        self.Stop()
        self.root = root
        self.Start()

    def SetFileMask(self, fileMask):
        self.Stop()
        self.fileMask = fileMask
        self.Start()

    def SetExcludePaths(self, excludePaths):
        self.Stop()
        self.excludePaths = excludePaths
        self.Start()

    def SetExcludePaths(self, excludeDirs):
        self.Stop()
        self.excludeDirs = excludeDirs
        self.Start()

    def Start(self):
        self.dirSnapshot = self.GetDirectoryInfo()
        self.timer.Start()

    def Stop(self):
        self.timer.Stop()

    def CheckDirectoryChanges(self):
        dirSnapshot = self.GetDirectoryInfo()
        diff = DirectoryInfoDiff(dirSnapshot, self.dirSnapshot)
        self.dirSnapshot = dirSnapshot

        if diff.createdDirs: wx.CallAfter(self.DirsCreatedEvent, diff.createdDirs)
        if diff.modifiedDirs: wx.CallAfter(self.DirsModifiedEvent, diff.modifiedDirs)
        if diff.deletedDirs: wx.CallAfter(self.DirsDeletedEvent, diff.deletedDirs)
        if diff.createdFiles: wx.CallAfter(self.FilesCreatedEvent, diff.createdFiles)
        if diff.modifiedFiles: wx.CallAfter(self.FilesModifiedEvent, diff.modifiedFiles)
        if diff.deletedFiles: wx.CallAfter(self.FilesDeletedEvent, diff.deletedFiles)


