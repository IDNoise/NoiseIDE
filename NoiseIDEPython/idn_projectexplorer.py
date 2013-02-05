from idn_events import Event
from idn_window_utils import IDNCustomTreeCtrl

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import shutil
import wx.lib.agw.customtreectrl as CT
from idn_utils import extension, Menu, CreateButton, writeFile
from idn_directoryinfo import DirectoryChecker
import core


ICON_SIZE = 16

class ProjectExplorer(IDNCustomTreeCtrl):
    FILE, DIRECTORY_OPEN, DIRECTORY_CLOSED = range(3)
    INTERVAL = 1

    def __init__(self, parent, project):
        style = wx.TR_MULTIPLE | wx.DIRCTRL_3D_INTERNAL | wx.TR_HAS_BUTTONS
        IDNCustomTreeCtrl.__init__(self, parent, agwStyle = style)

        self.project = project
        self.root = None
        self.mask = project.GetMask()
        if not self.mask:
            self.mask = self.DefaultMask()
        self.excludeDirs = self.DefaultExcludeDirs()
        self.excludePaths = self.DefaultExcludePaths()
        self.hiddenPaths = set()
        self.showHidden = False
        self.dirChecker = None
        self.tempData = []
        self.cut = False
        self.paths = {}

        self.SetupIcons()

        self.Bind(CT.EVT_TREE_ITEM_MENU, self.ShowMenu)
        self.Bind(CT.EVT_TREE_ITEM_ACTIVATED, self.OnActivateItem)
        self.Bind(wx.EVT_KEY_DOWN, self.OnExplorerKeyDown)

        self.ProjectFilesCreatedEvent = Event()
        self.ProjectFilesModifiedEvent = Event()
        self.ProjectFilesDeletedEvent = Event()
        self.ProjectDirsCreatedEvent = Event()
        self.ProjectDirsModifiedEvent = Event()
        self.ProjectDirsDeletedEvent = Event()

    def SetupIcons(self):
        self.imageList = wx.ImageList(ICON_SIZE, ICON_SIZE)
        self.iconIndex = {}
        self.AddIconFromArt(self.FILE, wx.ART_NORMAL_FILE)
        self.AddIconFromArt(self.DIRECTORY_OPEN, wx.ART_FILE_OPEN)
        self.AddIconFromArt(self.DIRECTORY_CLOSED, wx.ART_FOLDER)
        self.SetImageList(self.imageList)

    def AppendDir(self, parentNode, path):
        children = [self.GetPyData(c) for c in self.GetItemChildren(parentNode)]
        if (path in self.excludePaths or
            (not self.showHidden and path in self.hiddenPaths) or
            os.path.basename(path) in self.excludeDirs or
            path in children):
            return False
        dir = self.AppendItem(parentNode, os.path.basename(path))
        self.SetItemHasChildren(dir, True)
        self.SetPyData(dir, path)
        self.SetItemImage(dir, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
        self.SetItemImage(dir, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
        if path in self.hiddenPaths:
            self.SetAttrsForHiddenItem(dir)
        self.Load(dir, path)
        self.SortChildren(dir)
        self.paths[path] = dir
        return True

    def AppendFile(self, parentNode, path):
        file = os.path.basename(path)
        children = [self.GetPyData(c) for c in self.GetItemChildren(parentNode)]
        if (path in self.excludePaths or
            (not self.showHidden and path in self.hiddenPaths) or
            (self.mask and "*" not in self.mask and extension(file) not in self.mask) or
            path in children):
            return False
        icon = self.GetIconIndex(path)
        file = self.AppendItem(parentNode, file)
        self.SetItemImage(file, icon, wx.TreeItemIcon_Normal)
        if path in self.hiddenPaths:
            self.SetAttrsForHiddenItem(file)
        self.SetPyData(file, path)
        self.paths[path] = file
        return True

    def SetupChecker(self):
        if self.dirChecker:
            self.dirChecker.Stop()
        self.dirChecker = DirectoryChecker(self.INTERVAL, self.root, True, self.mask, self.excludeDirs, self.excludePaths)
        self.dirChecker.FilesCreatedEvent += self.OnFilesCreated
        self.dirChecker.FilesModifiedEvent += self.OnFilesModified
        self.dirChecker.FilesDeletedEvent += self.OnFilesDeleted
        self.dirChecker.DirsCreatedEvent += self.OnDirsCreated
        self.dirChecker.DirsModifiedEvent += self.OnDirsModified
        self.dirChecker.DirsDeletedEvent += self.OnDirsDeleted
        self.dirChecker.Start()

    def Destroy(self, *args, **kwargs):
        if self.dirChecker:
            self.dirChecker.FilesCreatedEvent -= self.OnFilesCreated
            self.dirChecker.FilesModifiedEvent -= self.OnFilesModified
            self.dirChecker.FilesDeletedEvent -= self.OnFilesDeleted
            self.dirChecker.DirsCreatedEvent -= self.OnDirsCreated
            self.dirChecker.DirsModifiedEvent -= self.OnDirsModified
            self.dirChecker.DirsDeletedEvent -= self.OnDirsDeleted
        IDNCustomTreeCtrl.Destroy(self, *args, **kwargs)

    def OnFilesCreated(self, files):
        self.ProjectFilesCreatedEvent(files)
        for file in files:
            self.FileCreated(file)

    def OnFilesModified(self, files):
        self.ProjectFilesModifiedEvent(files)
        for file in files:
            self.FileModified(file)

    def OnFilesDeleted(self, files):
        self.ProjectFilesDeletedEvent(files)
        for file in files:
            self.FileDeleted(file)

    def OnDirsCreated(self, dirs):
        self.ProjectDirsCreatedEvent(dirs)
        for dir in dirs:
            self.DirCreated(dir)

    def OnDirsModified(self, dirs):
        self.ProjectDirsModifiedEvent(dirs)
        for dir in dirs:
            self.DirModified(dir)

    def OnDirsDeleted(self, dirs):
        self.ProjectDirsDeletedEvent(dirs)
        for dir in dirs:
            self.DirDeleted(dir)

    def SetRoot(self, root):
        self.root = os.path.normpath(root)
        self.paths = {}
        self.SetupChecker()
        rootNode = self.AddRoot(root)
        self.paths[root] = rootNode
        self.SetItemHasChildren(rootNode, True)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_CLOSED], wx.TreeItemIcon_Normal)
        self.SetItemImage(rootNode, self.iconIndex[self.DIRECTORY_OPEN], wx.TreeItemIcon_Expanded)
        self.SetPyData(rootNode, root)

        self.Load(rootNode, root)
        self.SortChildren(rootNode)
        self.Expand(rootNode)

    def UpdateMask(self):
        self.DeleteAllItems()
        self.SetRoot(self.root)

    def AddMask(self, mask):
        if not mask in self.mask:
            self.mask.append(mask)
            self.UpdateMask()

    def RemoveMask(self, mask):
        if mask in self.mask:
            self.mask.remove(mask)
            self.UpdateMask()

    def GetCustomMask(self):
        return self.mask

    def SetHiddenList(self, list):
        self.hiddenPaths = list
        self.DeleteAllItems()
        self.SetRoot(self.root)

    def AddIcon(self, id, path):
        try:
            if os.path.exists(path):
                key = self.imageList.Add(wx.Bitmap(path, wx.BITMAP_TYPE_PNG))
                self.iconIndex[id] = key
        except Exception, e:
            core.Log("Add icon error: ", e)

    def AddIconFromArt(self, id, image):
        icon = wx.ArtProvider_GetBitmap(image, wx.ART_OTHER, (ICON_SIZE, ICON_SIZE))
        key = self.imageList.Add(icon)
        self.iconIndex[id] = key

    def Load(self, node, dir):
        if not os.path.isdir(dir):
            raise Exception("{} is not a valid directory".format(dir))

        files = os.listdir(dir)
        for f in files:
            path = os.path.normpath(os.path.join(dir, f))
            if os.path.isdir(path):
                self.AppendDir(node, path)
            else:
                self.AppendFile(node, path)

    def GetIconIndex(self, fileName):
        ext = extension(fileName)
        wx.Log.EnableLogging(False)
        noLog = wx.LogNull()
        result = self.iconIndex[self.FILE]
        if ext in self.iconIndex:
            result = self.iconIndex[ext]
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
                            result = iconkey
            except:
                pass
        del noLog
        wx.Log.EnableLogging(True)
        return result

    def StopTrackingProject(self):
        self.dirChecker.Stop()

    def FileCreated(self, file):
        if file in self.paths:
            return
        id = self.FindItemByPath(os.path.dirname(file))
        if id and self.AppendFile(id, file):
            self.SortChildren(id)

    def FileModified(self, file):
        if self.mask and extension(file) not in self.mask: return

    def FileDeleted(self, file):
        if self.mask and extension(file) not in self.mask: return
        id = self.FindItemByPath(file)
        if id:
            del self.paths[self.GetPyData(id)]
            self.Delete(id)

    def DirCreated(self, dir):
        id = self.FindItemByPath(os.path.dirname(dir))
        if id:
            self.AppendDir(id, dir)
            self.SortChildren(id)

    def DirModified(self, dir):
        pass

    def DirDeleted(self, dir):
        id = self.FindItemByPath(dir)
        if id:
            del self.paths[self.GetPyData(id)]
            self.Delete(id)

        for path in self.paths.keys():
            if path.startswith(dir):
                del self.paths[path]

    def FindItemByPath(self, path):
        if path in self.paths:
            return self.paths[path]
        return None

    def CreateMenu(self):
        menu = Menu()

        if self.eventItem == self.GetRootItem():
            menu.AppendMenuItem("Setup masks", self, self.OnMenuSetupMasks)
            menu.AppendCheckMenuItem("Show hidden", self, self.OnMenuShowHide, self.showHidden)
            menu.AppendSeparator()
            newMenu = Menu()
            newMenu.AppendMenuItem("File", self, self.OnMenuNewFile)
            newMenu.AppendMenuItem("Dir", self, self.OnMenuNewDir)
            newMenu.AppendSeparator()
            menu.newMenu = newMenu
            menu.AppendMenu(wx.ID_ANY, "New", newMenu)
        elif self.GetRootItem() in self.selectedItems:
            pass
        else:
            if len(self.selectedItems) == 1:
                if self.ItemHasChildren(self.eventItem):
                    menu.AppendMenuItem("Open in explorer", self, self.OnMenuOpenInExplorer)
                    menu.AppendSeparator()
                    newMenu = Menu()
                    newMenu.AppendMenuItem("File", self, self.OnMenuNewFile)
                    newMenu.AppendMenuItem("Dir", self, self.OnMenuNewDir)
                    newMenu.AppendSeparator()
                    menu.newMenu = newMenu
                    menu.AppendMenu(wx.ID_ANY, "New", newMenu)


                if (self.IsExecutable(self.eventItem) and self.IsEditable(self.eventItem)):
                    menu.AppendSeparator()
                    menu.AppendMenuItem("Edit", self, self.OnMenuEditExecutable)

                menu.AppendSeparator()
                menu.AppendMenuItem("Rename", self, self.OnMenuRename)

            menu.AppendSeparator()
            menu.AppendMenuItem("Cut", self, self.OnMenuCut)
            menu.AppendMenuItem("Copy", self, self.OnMenuCopy)

            if self.tempData:
                menu.AppendMenuItem("Paste", self, self.OnMenuPaste)
            menu.AppendMenuItem("Delete", self, self.OnMenuDelete)
            menu.AppendCheckMenuItem("Hide", self, self.OnMenuHide,
                self.GetPyData(self.selectedItems[0]) in self.hiddenPaths)
        return menu

    def ShowMenu(self, event):
        self.selectedItems = self.GetSelections()
        self.eventItem = event.GetItem()
        self.PopupMenu(self.CreateMenu())

    def IsExecutable(self, item):
        return extension(self.GetPyData(item)) in [".bat", ".exe", ".cmd", ".html", ".xhtml"]

    def IsEditable(self, item):
        return extension(self.GetPyData(item)) in [".bat", ".cmd", ".html", ".xhtml"]

    def DefaultMask(self):
        return []

    def DefaultExcludeDirs(self):
        return [".git", ".svn"]

    def DefaultExcludePaths(self):
        return []

    def OnMenuNewFile(self, event):
        (_, file) = self.RequestName("New File", "Enter file name", "new_file.txt")
        if file and not os.path.isfile(file):
            writeFile(file, "")

    def OnMenuNewDir(self, event):
        (_, dir) = self.RequestName("New Directory", "Enter dir name", "new_dir")
        if dir and not os.path.isdir(dir):
            os.mkdir(dir)
        else:
            wx.MessageBox("Dir {} already exists.".format(dir), "Error")

    def OnMenuCut(self, event):
        self.cut = True
        self.tempData = self.selectedItems

    def OnMenuCopy(self, event):
        self.cut = False
        self.tempData = self.selectedItems

    def OnMenuPaste(self, event):
        if not self.tempData: return
        toPath = self.GetPyData(self.eventItem)

        if os.path.isfile(toPath):
            toPath = os.path.dirname(toPath)

        if self.cut:
            for id in self.tempData:
                what = self.GetPyData(id)
                if toPath.startswith(what):
                    wx.MessageBox("Cant cut and paste dir into subdir.", "Error")
                    return

        for id in self.tempData:
            what = self.GetPyData(id)
            if self.cut and os.path.dirname(what) == toPath:
                continue
            if not os.path.exists(what): continue
            name = os.path.join(toPath, os.path.basename(what))
            newName = self.GetNewIfExists(name)

            if self.cut:
                shutil.move(what, newName)
            else:
                if os.path.isdir(what):
                    shutil.copytree(what, newName)
                else:
                    shutil.copy(what, newName)
            self.AfterPasteMove(what, newName)

        self.tempData = []

    def AfterPasteMove(self, oldName, newName):
        pass

    def SelectPath(self, path):
        item = self.FindItemByPath(path)
        if not item: return
        self.SelectItem(item)

    def OnMenuDelete(self, event):
        for id in self.selectedItems:
            path = self.GetPyData(id)
            if os.path.isdir(path):
                shutil.rmtree(path, True)
            else:
                os.remove(path)

    def GetNewIfExists(self, name):
        base, ext = os.path.splitext(name)
        if os.path.exists(name):
            i = 1
            while True:
                temp = name + "_" + str(i)
                if os.path.isfile(name):
                    temp = base + "_" + str(i) + ext
                if not os.path.exists(temp):
                    name = temp
                    break
                i += 1
        return name

    def OnMenuEditExecutable(self, event):
        path = self.GetPyData(self.eventItem)
        if not self.OpenFile(path):
            event.Skip()

    def OnMenuOpenInExplorer(self, event):
        import subprocess
        import sys
        path = self.GetPyData(self.eventItem)
        if sys.platform == 'darwin':
            subprocess.call(['open', '--', path])
        elif sys.platform == 'linux2':
            subprocess.call(['gnome-open', '--', path])
        elif sys.platform in ['windows', "win32"]:
            subprocess.call(['explorer', path])

    def ExecuteFile(self, path):
        os.chdir(os.path.dirname(path))
        path = path.replace("\\", "/")
        import webbrowser
        webbrowser.open(path)
        os.chdir(core.MainFrame.cwd)

    def OnMenuHide(self, event):
        if self.GetPyData(self.selectedItems[0]) in self.hiddenPaths:
            for id in self.selectedItems:
                path = self.GetPyData(id)
                self.hiddenPaths.remove(path)
                self.ClearHiddenAttrs(id)
        else:
            for id in self.selectedItems:
                path = self.GetPyData(id)
                self.hiddenPaths.add(path)
                self.SetAttrsForHiddenItem(id)
                if not self.showHidden:
                    self.Delete(id)

    def SetAttrsForHiddenItem(self, id):
        self.SetItemTextColour(id, wx.Colour(60, 60, 200))
        self.SetItemItalic(id, True)

    def ClearHiddenAttrs(self, id):
        self.SetItemTextColour(id, wx.NullColour)
        self.SetItemItalic(id, False)

    def OnMenuShowHide(self, event):
        if self.showHidden == True:
            self.showHidden = False
            for path in self.hiddenPaths:
                self.DeleteItemByPath(path)
        else:
            self.showHidden = True
            for path in sorted(self.hiddenPaths):
                if os.path.dirname(path) in self.hiddenPaths: continue
                id = self.FindItemByPath(os.path.dirname(path))
                if id:
                    if os.path.isdir(path):
                        self.AppendDir(id, path)
                    else:
                        self.AppendFile(id, path)
                    self.SortChildren(id)

    def OnMenuSetupMasks(self, event):
        dlg = MaskEditor(self)
        dlg.ShowModal()
        dlg.Destroy()

    def OnMenuRename(self, event):
        path = self.GetPyData(self.eventItem)
        self.Rename(path)

    def Rename(self, path):
        if os.path.isfile(path):
            what = "file"
        else:
            what = "dir"
        title = 'New ' + what + ' name:'
        dlg = wx.TextEntryDialog(self, title + ':', title,
            style = wx.OK | wx.CANCEL)
        dlg.SetValue(os.path.basename(path))
        if dlg.ShowModal() == wx.ID_OK:
            newPath = os.path.join(os.path.dirname(path), dlg.Value)

            def updateEditor(old, new):
                page = core.TabMgr.FindPageIndexByPath(old)
                editor = core.TabMgr[page]
                editor.filePath = new
                editor.UpdateTabTitle()

            if os.path.isfile(path):
                if path in core.TabMgr.OpenedFiles():
                    updateEditor(path, newPath)

            if os.path.isdir(path):
                for oPath in core.TabMgr.OpenedFiles():
                    if oPath.startswith(path):
                        oNewPath = oPath.replace(path, newPath)
                        updateEditor(oPath, oNewPath)

            os.rename(path, newPath)

        dlg.Destroy()


    def DeleteItemByPath(self, path):
        id = self.FindItemByPath(path)
        if id:
            self.Delete(id)
            return True
        return False

    def OnActivateItem(self, event):
        item = event.GetItem()
        path = self.GetPyData(item)
        if self.IsExecutable(item):
            self.ExecuteFile(path)
        else:
            if not self.OpenFile(path):
                event.Skip()

    def OpenFile(self, path):
        if os.path.isfile(path):
            core.TabMgr.LoadFileLine(path)
            return True
        return False

    def GetAllFiles(self, selectAll = False):
        result = []
        for file in self.dirChecker.files:
            if not selectAll and self.mask and extension(file) not in self.mask:
                continue
            result.append(file)
        return result

    def _GetFiles(self, item):
        #self.SelectAll()
        result = []
        if item:
            if item.HasChildren():
                for id in item.GetChildren():
                    result += self._GetFiles(id)
            else:
                path = self.GetPyData(item)
                if os.path.isfile(path):
                    result.append(path)
        return result

    def RequestName(self, title, prompt, default_value):
        path = None
        name = None
        while not name:
            dialog = wx.TextEntryDialog(None, prompt,
                title, default_value, style=wx.OK | wx.CANCEL)
            if dialog.ShowModal() == wx.ID_OK:
                name = dialog.GetValue()
                path = os.path.join(self.GetPyData(self.eventItem), name)
                dialog.Destroy()
            else:
                break

        if path:
            return (name, path)
        else:
            return None

    def OnCompareItems(self, item1, item2):
        path1 = self.GetPyData(item1)
        path2 = self.GetPyData(item2)

        if os.path.isdir(path1) and os.path.isdir(path2):
            result = 1 if path1 > path2 else -1
        elif os.path.isdir(path1) and os.path.isfile(path2):
            result = -1
        elif os.path.isfile(path1) and os.path.isfile(path2):
            result = 1 if path1 > path2 else -1
        elif os.path.isfile(path1) and os.path.isdir(path2):
            result = 1
        else:
            result = 0
        return result

    def OnExplorerKeyDown(self, event):
        self.selectedItems = self.GetSelections()
        self.eventItem = self.selectedItems[0] if self.selectedItems else None

        rootInSelection = self.GetRootItem() in self.selectedItems
        if not self.eventItem or not self.selectedItems or rootInSelection:
            return

        canPaste = self.tempData

        code = event.GetKeyCode()
        CTRL = event.GetModifiers() == wx.MOD_CONTROL

        if code == ord("C") and CTRL:
            self.OnMenuCopy(None)

        elif code == ord("X") and CTRL:
            self.OnMenuCut(None)
        elif canPaste and code == ord("V") and CTRL:
            self.OnMenuPaste(None)
        elif not rootInSelection and (code == wx.WXK_DELETE or (code == ord("D") and CTRL)):
            self.OnMenuDelete(None)
        elif self.IsExecutable(self.eventItem) and code == ord("E") and CTRL:
            self.OnMenuEditExecutable(None)
        else:
            event.Skip()

    def OnPaint(self, event):
        try:
            CT.CustomTreeCtrl.OnPaint(self, event)
        except Exception, e:
            core.Log("tree control pain exception", e)


class PythonProjectExplorer(ProjectExplorer):
    def FillNewSubMenu(self, newMenu):
        newMenu.AppendMenuItem("New File", self, self.OnMenuNewFile)

    def DefaultMask(self):
        return [".py", ".yaml"]

    def OnMenuNewFile(self, event):
        pass


class MaskEditor(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent)
        self.explorer = parent
        self.sizer = wx.BoxSizer()
        buttonSizer = wx.BoxSizer(wx.VERTICAL)
        self.list = wx.ListBox(self, pos = (10, 10), size = (100, 400),
            choices = self.explorer.mask, style = wx.LB_SINGLE)
        buttonAdd = CreateButton(self, "Add", self.OnAddMask)
        buttonRemove = CreateButton(self, "Remove", self.OnRemoveMask)
        buttonSizer.Add(buttonAdd)
        buttonSizer.Add(buttonRemove)
        self.sizer.Add(self.list)
        self.sizer.AddSizer(buttonSizer)
        self.SetSizer(self.sizer)
        self.Layout()

    def OnAddMask(self, event):
        dlg = wx.TextEntryDialog(self, 'Mask:', 'Add Mask',
                    style = wx.OK | wx.CANCEL)
        dlg.SetValue("")
        if dlg.ShowModal() == wx.ID_OK:
            self.explorer.AddMask(dlg.Value)
            self.list.SetItems(self.explorer.mask)
        dlg.Destroy()

    def OnRemoveMask(self, event):
        if self.list.Selection != wx.NOT_FOUND:
            self.explorer.RemoveMask(self.list.GetString(self.list.Selection))
            self.list.SetItems(self.explorer.mask)
