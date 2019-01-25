from idn_events import Event

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from stat import ST_MTIME
from idn_utils import Timer, extension
import core
import wx
import gc

from collections import defaultdict

olditems = None
def output_memory():
    try:
        global olditems
        d = defaultdict(int)
        for o in gc.get_objects():
            name = type(o).__name__
            d[name] += 1
        items = d.items()
        items.sort(key=lambda x:x[1])
        if olditems:
            for k, v in items:
                if k in olditems and v >= olditems[k]:
                    core.Log("Changed ", k, "from", olditems[k], "to", v)
        olditems = items
    finally:
        pass

import time

def GatherInfo(root, recursive = True, fileMask = None, excludeDirs = None, excludePaths = None):
    if not fileMask: fileMask = []
    if not excludeDirs: excludeDirs = []
    if not excludePaths: excludePaths = []
    dirs = {}
    files = {}
    for f in os.listdir(root):
        try:
            f = os.path.normpath(os.path.join(root, f))
            if excludePaths and f in excludePaths:
                continue
            mtime = os.stat(f)[ST_MTIME]
            if os.path.isdir(f):
                if os.path.basename(f) in excludeDirs or f in excludeDirs:
                    continue
                dirs[f] = mtime
                if recursive:
                    time.sleep(0.0005)
                    (dd, fd) = GatherInfo(f, recursive, fileMask, excludeDirs, excludePaths)
                    dirs.update(dd)
                    files.update(fd)
            else:
                if fileMask and extension(f) not in fileMask:
                    continue
                files[f] = mtime
        except Exception, e:
            core.Log("Gather dir info error: ", e)
    return (dirs, files)

class DirectoryInfoDiff:
    def __init__(self, newState, oldState):
        self.createdFiles = []
        self.createdDirs  = []
        self.modifiedFiles = []
        self.modifiedDirs = []
        self.deletedFiles = []
        self.deletedDirs = []

        (self.createdDirs, self.modifiedDirs, self.deletedDirs) =\
        self.GetNewModDel(newState[0], oldState[0])
        (self.createdFiles, self.modifiedFiles, self.deletedFiles) =\
        self.GetNewModDel(newState[1], oldState[1])

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
        self.interval = interval
        self.timer = wx.Timer(core.MainFrame, wx.ID_ANY)
        self.files = []
        core.MainFrame.Bind(wx.EVT_TIMER, self.CheckDirectoryChanges, self.timer)

        self.FilesCreatedEvent = Event()
        self.FilesModifiedEvent = Event()
        self.FilesDeletedEvent = Event()

        self.DirsCreatedEvent = Event()
        self.DirsDeletedEvent = Event()

        self.dirSnapshot = ({}, {})
        self.CheckDirectoryChanges()

    def SetInterval(self, interval):
        self.Stop()
        self.interval = interval
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
        #self.dirSnapshot = self.GetDirectoryInfo()
        if self.interval > 0:
            self.timer.Start(self.interval * 1000, True)

    def Stop(self):
        self.timer.Stop()

    def CheckDirectoryChanges(self, event = None):
        dirSnapshot = GatherInfo(self.root, self.recursive, self.fileMask, self.excludeDirs, self.excludePaths)
        diff = DirectoryInfoDiff(dirSnapshot, self.dirSnapshot)
        self.dirSnapshot = dirSnapshot
        self.files = self.dirSnapshot[1].keys()
        #print diff.createdDirs
        if diff.createdDirs: self.DirsCreatedEvent(diff.createdDirs)
        if diff.deletedDirs: self.DirsDeletedEvent(diff.deletedDirs)
        if diff.createdFiles: self.FilesCreatedEvent(diff.createdFiles)
        if diff.modifiedFiles: self.FilesModifiedEvent(diff.modifiedFiles)
        if diff.deletedFiles: self.FilesDeletedEvent(diff.deletedFiles)
        if self.interval > 0:
            self.timer.Start(self.interval * 1000, True)
        #del diff


