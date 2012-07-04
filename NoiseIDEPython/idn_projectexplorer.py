__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import time
import shutil
import wx.lib.agw.customtreectrl as CT
from idn_utils import extension, Menu
from idn_directoryinfo import DirectoryChecker

ICON_SIZE = 16

wxEVT_PROJECT_FILE_CREATED = wx.NewEventType()
wxEVT_PROJECT_DIR_CREATED = wx.NewEventType()
wxEVT_PROJECT_FILE_MODIFIED = wx.NewEventType()
wxEVT_PROJECT_DIR_MODIFIED = wx.NewEventType()
wxEVT_PROJECT_FILE_DELETED = wx.NewEventType()
wxEVT_PROJECT_DIR_DELETED = wx.NewEventType()

EVT_PROJECT_FILE_CREATED = wx.PyEventBinder(wxEVT_PROJECT_FILE_CREATED, 1)
EVT_PROJECT_DIR_CREATED = wx.PyEventBinder(wxEVT_PROJECT_DIR_CREATED, 1)
EVT_PROJECT_FILE_MODIFIED = wx.PyEventBinder(wxEVT_PROJECT_FILE_MODIFIED, 1)
EVT_PROJECT_DIR_MODIFIED = wx.PyEventBinder(wxEVT_PROJECT_DIR_MODIFIED, 1)
EVT_PROJECT_FILE_DELETED = wx.PyEventBinder(wxEVT_PROJECT_FILE_DELETED, 1)
EVT_PROJECT_DIR_DELETED = wx.PyEventBinder(wxEVT_PROJECT_DIR_DELETED, 1)

class ProjectExplorerFileEvent(wx.PyCommandEvent):
    def __init__(self, evtType, evtId, file = None, **kwargs):
        """
        :param integer `evtType`: the event type;
        :param integer `evtId`: the event identifier;
        :param `file`: string path to file;
        """

        wx.PyCommandEvent.__init__(self, evtType, evtId, **kwargs)
        self.File = file

class ProjectExplorer(CT.CustomTreeCtrl):
    FILE, DIRECTORY_OPEN, DIRECTORY_CLOSED = range(3)
    INTERVAL = 1
    def __init__(self, parent):
        style = wx.TR_MULTIPLE | wx.DIRCTRL_3D_INTERNAL | wx.TR_HAS_BUTTONS

        CT.CustomTreeCtrl.__init__(self, parent, agwStyle = style)

        self.root = None
        self.mask = self.DefaultMask()
        self.excludeDirs = self.DefaultExcludeDirs()
        self.excludePaths = self.DefaultExcludePaths()
        self.dirChecker = None

        self.SetupIcons()

        self.Bind(CT.EVT_TREE_ITEM_MENU, self.ShowMenu)
        self.Bind(CT.EVT_TREE_ITEM_ACTIVATED, self.OnActivateItem)

    def SetupIcons(self):
        self.imageList = wx.ImageList(ICON_SIZE, ICON_SIZE)
        self.iconIndex = {}
        self.AddIconFromArt(self.FILE, wx.ART_NORMAL_FILE)
        self.AddIconFromArt(self.DIRECTORY_OPEN, wx.ART_FILE_OPEN)
        self.AddIconFromArt(self.DIRECTORY_CLOSED, wx.ART_FOLDER)
        self.SetImageList(self.imageList)

    def AppendDir(self, parentNode, path):
        if self.excludeDirs and os.path.basename(path) in self.excludeDirs:
            return False
        dir = self.AppendItem(parentNode, os.path.basename(path))
        self.SetItemHasChildren(dir, True)
        self.SetPyData(dir, path)
        self.SetItemImage(dir, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
        self.SetItemImage(dir, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
        self.Load(dir, path)
        return True

    def AppendFile(self, parentNode, path):
        file = os.path.basename(path)
        if self.mask and extension(file) not in self.mask: return False
        file = self.AppendItem(parentNode, file)
        icon = self.GetIconIndex(path)
        self.SetItemImage(file, icon, wx.TreeItemIcon_Normal)
        self.SetPyData(file, path)
        return True

    def SetupChecker(self):
        if self.dirChecker:
            self.dirChecker.Stop()
        self.dirChecker = DirectoryChecker(self.INTERVAL, self.root, True, self.mask, self.excludeDirs, self.excludePaths)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_FILE_CREATED, self.FileCreated)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_FILE_MODIFIED, self.FileModified)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_FILE_DELETED, self.FileDeleted)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_DIR_CREATED, self.DirCreated)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_DIR_MODIFIED, self.DirModified)
        self.dirChecker.AddHandler(DirectoryChecker.HANDLER_DIR_DELETED, self.DirDeleted)
        self.dirChecker.Start()

    def SetRoot(self, root):
        self.root = root
        self.SetupChecker()
        rootNode = self.AddRoot(root)
        self.SetItemHasChildren(rootNode, True)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
        self.SetPyData(rootNode, root)

        self.Load(rootNode, root)
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
            raise Exception("{} is not a valid directory".format(dir))

        files = os.listdir(dir)
        for f in files:
            path = os.path.join(dir, f)
            if self.excludePaths and path in self.excludePaths:
                continue
            if os.path.isdir(path):
                self.AppendDir(node, path)
            else:
                self.AppendFile(node, path)

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
        self.dirChecker.Stop()

    def FileCreated(self, file):
        id = self.FindItemByPath(file)
        if self.AppendFile(id, file):
            e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_CREATED , self.GetId(), file)
            self.GetEventHandler().ProcessEvent(e)

    def FileModified(self, file):
        if self.mask and extension(file) not in self.mask: return
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_MODIFIED , self.GetId(), file)
        self.GetEventHandler().ProcessEvent(e)

    def FileDeleted(self, file):
        if self.mask and extension(file) not in self.mask: return
        parentId = self.FindItemByPath(file)
        id = self.FindItem(parentId, os.path.basename(file))
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_DELETED , self.GetId(), file)
        self.GetEventHandler().ProcessEvent(e)
        self.Delete(id)

    def DirCreated(self, dir):
        id = self.FindItemByPath(os.path.dirname(dir))
        self.AppendDir(id, dir)

        e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_CREATED, self.GetId(), dir)
        self.GetEventHandler().ProcessEvent(e)

    def DirModified(self, dir):
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_MODIFIED, self.GetId(), dir)
        self.GetEventHandler().ProcessEvent(e)

    def DirDeleted(self, dir):
        id = self.FindItemByPath(dir)
        if self.GetPyData(id) == dir:
            e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_DELETED , self.GetId(), dir)
            self.GetEventHandler().ProcessEvent(e)
            self.Delete(id)

    def FindItemByPath(self, path):
        id = self.GetRootItem()
        items = self.SplitOnItemsFromRoot(path)
        for item in items:
            id = self.FindItem(id, item)
        return id

    def SplitOnItemsFromRoot(self, path):
        items = []
        if os.path.isfile(path):
            path = os.path.dirname(path)
        while path != self.root:
            (path, folder) = os.path.split(path)
            items.append(folder)

        return reversed(items)

    def ShowMenu(self, event):
        self.popupItemId = event.GetItem()
        menu = Menu()
        if self.ItemHasChildren(self.popupItemId):
            newMenu = Menu()
            newMenu.AppendMenuItem("New Dir", self, self.OnMenuNewDir)
            newMenu.AppendSeparator()
            self.FillNewSubMenu(newMenu)
            menu.AppendMenu(wx.NewId(), "New", newMenu)
        if self.popupItemId != self.GetRootItem():
            menu.AppendMenuItem("Delete", self, self.OnMenuDelete)

        self.PopupMenu(menu)

    def FillNewSubMenu(self, newMenu):
        pass

    def DefaultMask(self):
        return []

    def DefaultExcludeDirs(self):
        return [".git", ".svn"]

    def DefaultExcludePaths(self):
        return []

    def OnMenuNewDir(self, event):
        #print "on menu new dir", self.GetPyData(self.popupItemId)
        dialog = wx.TextEntryDialog(None, "Enter dir name",
            "New Directory", "new_dir", style=wx.OK | wx.CANCEL)
        if dialog.ShowModal() == wx.ID_OK:
            newDir = os.path.join(self.GetPyData(self.popupItemId), dialog.GetValue())
            if not os.path.isdir(newDir):
                os.mkdir(newDir)
        dialog.Destroy()

    def OnMenuDelete(self, event):
        #print "on menu delete", self.GetPyData(self.popupItemId)
        path = self.GetPyData(self.popupItemId)
        if os.path.isdir(path):
            os.removedirs(path)
        else:
            os.remove(path)

    def OnActivateItem(self, event):
        path = self.GetPyData(event.GetItem())
        if os.path.isfile(path):
            self.Parent.TabMgr.LoadFile(path)
        else:
            event.Skip()


class PythonProjectExplorer(ProjectExplorer):
    def FillNewSubMenu(self, newMenu):
        newMenu.AppendMenuItem("New File", self, self.OnMenuNewFile)

    def DefaultMask(self):
        return [".py", ".yaml"]

    def OnMenuNewFile(self, event):
        print "on menu new file", self.GetPyData(self.popupItemId)

class ErlangProjectExplorer(ProjectExplorer):
    def FillNewSubMenu(self, newMenu):
        newMenu.AppendMenuItem("New Module", self, self.OnMenuNewModule)
        newMenu.AppendMenuItem("New Header", self, self.OnMenuNewHeader)

    def DefaultMask(self):
        return [".erl", ".hrl", ".config", "*.c", "*.cpp"]

    def OnMenuNewModule(self, event):
        print "on menu new module"

    def OnMenuNewHeader(self, event):
        print "on menu new header"

    def DefaultExcludeDirs(self):
        return ProjectExplorer.DefaultExcludeDirs(self) + ["ebin", ".settings"]