import time

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import wx.lib.agw.customtreectrl as CT
from idn_utils import extension, Timer
from idn_directoryinfo import DirectoryInfo,DirectoryInfoDiff

ICON_SIZE = 16

class ProjectExplorer(CT.CustomTreeCtrl):
    FILE, DIRECTORY_OPEN, DIRECTORY_CLOSED = range(3)

    def __init__(self, parent):
        style = wx.TR_MULTIPLE | \
                wx.DIRCTRL_3D_INTERNAL | \
                wx.TR_HAS_BUTTONS# |\
                #CT.TR_NO_LINES_TO_SINGLE_ITEMS# |\
                #wx.TR_NO_LINES
        CT.CustomTreeCtrl.__init__(self, parent, agwStyle = style)

        self.root = None
        self.mask = []

        self.imageList = wx.ImageList(ICON_SIZE, ICON_SIZE)
        self.iconIndex = {}
        self.AddIconFromArt(self.FILE, wx.ART_NORMAL_FILE)
        self.AddIconFromArt(self.DIRECTORY_OPEN, wx.ART_FILE_OPEN)
        self.AddIconFromArt(self.DIRECTORY_CLOSED, wx.ART_FOLDER)
        self.SetImageList(self.imageList)
        self.timer = Timer(1.5, self.CheckDirectoryChanges)
        self.timer.Start()


    def SetRoot(self, path):
        self.root = path
        self.dirSnapshot = DirectoryInfo(path)
        rootNode = self.AddRoot(path)
        self.SetItemHasChildren(rootNode, True)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
        self.SetPyData(rootNode, path)

        self.Load(rootNode, path)
        self.Expand(rootNode)

    def SetMask(self, mask):
        self.mask = mask
        self.DeleteAllItems()
        self.SetRoot(self.root)

    def AddMask(self, mask):
        self.SetMask(self.mask + mask)

    def AddIcon(self, id, path):
        try:
            if os.path.exists(path):
                key = self.imageList.Add(wx.Bitmap(path, wx.BITMAP_TYPE_PNG))
                self.iconIndex[id] = key
        except Exception, e:
            print e

    def AddIconFromArt(self, id, image):
        icon = wx.ArtProvider_GetBitmap(image, wx.ART_OTHER, (ICON_SIZE, ICON_SIZE))
        key = self.imageList.Add(icon)
        self.iconIndex[id] = key

    def Load(self, node, dir):
        if not os.path.isdir(dir):
            raise Exception("%s is not a valid directory" % dir)

        files = os.listdir(dir)
        for f in files:
            fpath = os.path.join(dir, f)
            if os.path.isdir(fpath):
                child = self.AppendItem(node, f)
                self.SetItemHasChildren(child, True)
                self.SetPyData(child, fpath)
                self.SetItemImage(child, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
                self.SetItemImage(child, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
                self.Load(child, fpath)
            else:
                if self.mask and extension(f) not in self.mask: continue
                child = self.AppendItem(node, f)
                icon = self.GetIconIndex(f)
                self.SetItemImage(child, icon, wx.TreeItemIcon_Normal)
                self.SetPyData(child, fpath)

    def GetIconIndex(self, fileName):
        ext = extension(fileName)
        if ext in self.iconIndex:
            return self.iconIndex[ext]
        else:
            try:
                fileType = wx.TheMimeTypesManager.GetFileTypeFromExtension(ext)
                if hasattr(fileType, 'GetIconInfo'):
                    info = fileType.GetIconInfo()

                    if info is not None:
                        icon = info[0]
                        if icon.Ok():
                            key = self.imageList.AddIcon(icon)
                            self.iconIndex[ext] = key
                            self.SetImageList(self.imageList)
                            return iconkey
            finally:
                return self.iconIndex[self.FILE]

    def StopTrackingProject(self):
        self.timer.Cancel()

    def CheckDirectoryChanges(self):
        if not self.root:
            return
        dirSnapshot = DirectoryInfo(self.root)
        diff = DirectoryInfoDiff(dirSnapshot, self.dirSnapshot)
        #print "mod", diff.modifiedFiles
        #print "new", diff.createdFiles
        #print "del",  diff.deletedFiles
        self.dirSnapshot = dirSnapshot



