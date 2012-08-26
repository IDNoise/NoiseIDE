__author__ = 'Yaroslav Nikityshev aka IDNoise'

import re
import os
import wx
import time
import subprocess
import shutil
import wx.lib.agw.customtreectrl as CT
from idn_utils import extension, Menu, writeFile, CreateButton
from idn_findreplace import ReplaceInProject, ReplaceInFile
from idn_directoryinfo import DirectoryChecker
from idn_global import GetTabMgr, GetMainFrame, Log
from idn_cache import readFile
from idn_config import Config


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

    def __init__(self, parent, project):
        style = wx.TR_MULTIPLE | wx.DIRCTRL_3D_INTERNAL | wx.TR_HAS_BUTTONS
        CT.CustomTreeCtrl.__init__(self, parent, agwStyle = style)

        self.project = project
        self.root = None
        self.mask = self.DefaultMask() + project.GetMask()
        self.excludeDirs = self.DefaultExcludeDirs()
        self.excludePaths = self.DefaultExcludePaths()
        self.hiddenPaths = set()
        self.showHidden = False
        self.dirChecker = None
        self.tempData = []
        self.cut = False

        self.SetupIcons()

        self.Bind(CT.EVT_TREE_ITEM_MENU, self.ShowMenu)
        self.Bind(CT.EVT_TREE_ITEM_ACTIVATED, self.OnActivateItem)
        self.Bind(wx.EVT_KEY_DOWN, self.OnExplorerKeyDown)

    def SetupIcons(self):
        self.imageList = wx.ImageList(ICON_SIZE, ICON_SIZE)
        self.iconIndex = {}
        self.AddIconFromArt(self.FILE, wx.ART_NORMAL_FILE)
        self.AddIconFromArt(self.DIRECTORY_OPEN, wx.ART_FILE_OPEN)
        self.AddIconFromArt(self.DIRECTORY_CLOSED, wx.ART_FOLDER)
        self.SetImageList(self.imageList)

    def AppendDir(self, parentNode, path):
        if (path in self.excludePaths or
            (not self.showHidden and path in self.hiddenPaths) or
            os.path.basename(path) in self.excludeDirs):
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
        return True

    def AppendFile(self, parentNode, path):
        file = os.path.basename(path)
        children = [self.GetPyData(c) for c in self.GetItemChildren(parentNode)]
        if (path in self.excludePaths or
            (not self.showHidden and path in self.hiddenPaths) or
            self.mask and extension(file) not in self.mask or
            path in children):
            return False
        icon = self.GetIconIndex(path)
        file = self.AppendItem(parentNode, file)
        self.SetItemImage(file, icon, wx.TreeItemIcon_Normal)
        if path in self.hiddenPaths:
            self.SetAttrsForHiddenItem(file)
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
        return [m for m in self.mask if m not in self.DefaultMask()]

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
            Log("Add icon error: ", e)

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
        #print "file created", file
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_CREATED , self.GetId(), file)
        self.GetEventHandler().ProcessEvent(e)
        id = self.FindItemByPath(os.path.dirname(file))
        if id and self.AppendFile(id, file):
            self.SortChildren(id)


    def FileModified(self, file):
        #print "file mod", file
        if self.mask and extension(file) not in self.mask: return
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_MODIFIED , self.GetId(), file)
        self.GetEventHandler().ProcessEvent(e)

    def FileDeleted(self, file):
        #print "file del", file
        if self.mask and extension(file) not in self.mask: return
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_FILE_DELETED , self.GetId(), file)
        self.GetEventHandler().ProcessEvent(e)
        id = self.FindItemByPath(file)
        if id:
            self.Delete(id)

    def DirCreated(self, dir):
        #print "dir created", dir
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_CREATED, self.GetId(), dir)
        self.GetEventHandler().ProcessEvent(e)
        id = self.FindItemByPath(os.path.dirname(dir))
        if id:
            self.AppendDir(id, dir)
            self.SortChildren(id)



    def DirModified(self, dir):
        #print "dir mod", dir
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_MODIFIED, self.GetId(), dir)
        self.GetEventHandler().ProcessEvent(e)

    def DirDeleted(self, dir):
        #print "dir del", dir
        e = ProjectExplorerFileEvent(wxEVT_PROJECT_DIR_DELETED , self.GetId(), dir)
        self.GetEventHandler().ProcessEvent(e)
        id = self.FindItemByPath(dir)
        if id:
            self.Delete(id)

    def FindItemByPath(self, path):
        id = self.GetRootItem()
        items = self.SplitOnItemsFromRoot(path)
        for item in items:
            children = self.GetItemChildren(id)
            for c in children:
                if self.GetPyData(c).endswith(item):
                    id = c
                    break
        if self.GetPyData(id) == path:
            return id
        return None

    def GetItemChildren(self, item):
        child, cookie = self.GetFirstChild(item)
        children = []
        while child:
            children.append(child)
            child, cookie = self.GetNextChild(item, cookie)
        return children

    def SplitOnItemsFromRoot(self, path):
        items = []
        while path != self.root:
            (path, folder) = os.path.split(path)
            items.append(folder)

        return reversed(items)

    def ShowMenu(self, event):
        self.selectedItems = self.GetSelections()#event.GetItem()
        self.eventItem = event.GetItem()
        menu = Menu()

        if self.selectedItems and self.GetRootItem() not in self.selectedItems:
            menu.AppendMenuItem("Cut", self, self.OnMenuCut)
            menu.AppendMenuItem("Copy", self, self.OnMenuCopy)

        if self.tempData and self.ItemHasChildren(self.eventItem):
            menu.AppendMenuItem("Paste", self, self.OnMenuPaste)

        if len(self.selectedItems) > 1 and self.GetRootItem() not in self.selectedItems:
            menu.AppendMenuItem("Delete", self, self.OnMenuDelete)
            menu.AppendCheckMenuItem("Hide", self, self.OnMenuHide,
                self.GetPyData(self.selectedItems[0]) in self.hiddenPaths)
        elif self.eventItem == self.GetRootItem():
            menu.AppendMenuItem("Setup masks", self, self.OnMenuSetupMasks)
            menu.AppendCheckMenuItem("Show hidden", self, self.OnMenuShowHide, self.showHidden)
        else:
            if self.ItemHasChildren(self.eventItem):
                newMenu = Menu()
                newMenu.AppendMenuItem("New Dir", self, self.OnMenuNewDir)
                newMenu.AppendSeparator()
                self.FillNewSubMenu(newMenu)
                menu.AppendMenu(wx.NewId(), "New", newMenu)
                menu.AppendSeparator()

            menu.AppendMenuItem("Rename", self, self.OnMenuRename)
            menu.AppendMenuItem("Delete", self, self.OnMenuDelete)
            menu.AppendCheckMenuItem("Hide", self, self.OnMenuHide,
                self.GetPyData(self.eventItem) in self.hiddenPaths)

            if self.IsExecutable(self.eventItem):
                menu.AppendSeparator()
                menu.AppendMenuItem("Execute", self, self.OnMenuExecute)

        self.PopupMenu(menu)

    def IsExecutable(self, item):
        return extension(self.GetPyData(item)) in [".bat", ".exe", ".cmd"]

    def FillNewSubMenu(self, newMenu):
        pass

    def DefaultMask(self):
        return []

    def DefaultExcludeDirs(self):
        return [".git", ".svn"]

    def DefaultExcludePaths(self):
        return []

    def OnMenuNewDir(self, event):
        (_, dir) = self.RequestName("New Directory", "Enter dir name", "new_dir")
        if dir and not os.path.isdir(dir):
            os.mkdir(dir)

    def OnMenuCut(self, event):
        self.cut = True
        self.tempData = self.selectedItems

    def OnMenuCopy(self, event):
        self.cut = False
        self.tempData = self.selectedItems

    def OnMenuPaste(self, event):
        if not self.tempData: return
        toPath = self.GetPyData(self.eventItem)
        #print self.cut, "to", toPath

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
            newName = self.GetNewIfExists(os.path.join(toPath, os.path.basename(what)))
            #print  what, " -> ", newName

            if self.cut:
                shutil.move(what, newName)
            else:
                if os.path.isdir(what):
                    shutil.copytree(what, newName)
                else:
                    shutil.copy(what, newName)

        self.tempData = []

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

    def OnMenuExecute(self, event):
        path = self.GetPyData(self.eventItem)
        path = path.replace("\\", "/")
        pp = subprocess.Popen(path, shell = True, stdout = subprocess.PIPE)

        Log("=======Executing " + path)
        for out in pp.stdout:
            Log(out)
        Log("=======")

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
        rootId = self.GetRootItem()
        self.SetItemTextColour(id, self.GetItemTextColour(rootId))
        self.SetItemItalic(id, False)

    def OnMenuShowHide(self, event):
        if self.showHidden == True:
            self.showHidden = False
            for path in self.hiddenPaths:
                self.DeleteItemByPath(path)
        else:
            self.showHidden = True
            for path in sorted(self.hiddenPaths):
                #print path
                if os.path.dirname(path) in self.hiddenPaths: continue
                if os.path.isdir(path):
                    id = self.FindItemByPath(os.path.dirname(path))
                    #print "dir"
                    self.AppendDir(id, path)
                else:
                    id = self.FindItemByPath(path)
                    self.AppendFile(id, path)
                    #print "file"
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
                page = GetTabMgr().FindPageIndexByPath(old)
                editor = GetTabMgr()[page]
                editor.filePath = new
                editor.UpdateTabTitle()

            if os.path.isfile(path):
                if path in GetTabMgr().OpenedFiles():
                    if path.endswith(".erl"):
                        (oldModuleName, ext) = os.path.splitext(os.path.basename(path))
                        (newModuleName, ext) = os.path.splitext(os.path.basename(newPath))
                        self.ReplaceOccurencesInProject(path, oldModuleName, newModuleName)

                    updateEditor(path, newPath)

            if os.path.isdir(path):
                for oPath in GetTabMgr().OpenedFiles():
                    if oPath.startswith(path):
                        oNewPath = oPath.replace(path, newPath)
                        updateEditor(oPath, oNewPath)

            os.rename(path, newPath)

        dlg.Destroy()

    def ReplaceOccurencesInProject(self, path, oldModuleName, newModuleName):
        what = "-module\(" + oldModuleName + "\)"
        on = "-module(" + newModuleName + ")"
        ReplaceInFile(path, re.compile(what, re.MULTILINE | re.DOTALL), on)
        what = r"\b" + oldModuleName + ":"
        on = newModuleName + ":"
        ReplaceInProject(re.compile(what, re.MULTILINE | re.DOTALL), on, [".erl", ".hrl"])

    def DeleteItemByPath(self, path):
        id = self.FindItemByPath(path)
        if id:
            self.Delete(id)
            return True
        return False

    def OnActivateItem(self, event):
        path = self.GetPyData(event.GetItem())
        if os.path.isfile(path):
            GetTabMgr().LoadFile(path)
        else:
            event.Skip()

    def GetAllFiles(self):
        result = []
        for file in self.dirChecker.files:
            if self.mask and extension(file) not in self.mask:
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
        dialog = wx.TextEntryDialog(None, prompt,
            title, default_value, style=wx.OK | wx.CANCEL)
        result = None
        value = None
        if dialog.ShowModal() == wx.ID_OK:
            value = dialog.GetValue()
            result = os.path.join(self.GetPyData(self.eventItem), value)
        dialog.Destroy()
        if result:
            return (value, result)
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

        #print event.GetItem()

        rootInSelection = self.GetRootItem() in self.selectedItems
        if not self.eventItem or not self.selectedItems or rootInSelection:
            return

        canPaste = self.tempData and self.ItemHasChildren(self.eventItem)

        code = event.GetKeyCode()
        CTRL = event.ControlDown()

        if code == ord("C") and CTRL:
            self.OnMenuCopy(None)

        elif code == ord("X") and CTRL:
            self.OnMenuCut(None)
        elif canPaste and code == ord("V") and CTRL:
            self.OnMenuPaste(None)
        elif not rootInSelection and (code == wx.WXK_DELETE or (code == ord("D") and CTRL)):
            self.OnMenuDelete(None)
        elif self.IsExecutable(self.eventItem) and code == ord("E") and CTRL:
            self.OnMenuExecute(None)
        else:
            event.Skip()


class PythonProjectExplorer(ProjectExplorer):
    def FillNewSubMenu(self, newMenu):
        newMenu.AppendMenuItem("New File", self, self.OnMenuNewFile)

    def DefaultMask(self):
        return [".py", ".yaml"]

    def OnMenuNewFile(self, event):
        print "on menu new file", self.GetPyData(self.eventItem)

class ErlangProjectExplorer(ProjectExplorer):
    def FillNewSubMenu(self, newMenu):
        newMenu.AppendMenuItem("New Module", self, self.OnMenuNewModule)
        newMenu.AppendMenuItem("New Header", self, self.OnMenuNewHeader)

    def DefaultMask(self):
        return [".erl", ".hrl", ".config", ".c", ".cpp", ".bat", ".igor"]

    def OnMenuNewModule(self, event):
        (module, path) = self.RequestName("New Module", "Enter module name", "new_module")
        path = path + ".erl"
        if path and not os.path.isfile(path):
            data = self._GetTemplate("module")
            data = data.replace("[module_name]", module)
            writeFile(path, data)
            GetTabMgr().LoadFile(path)

    def OnMenuNewHeader(self, event):
        (_, path) = self.RequestName("New Header", "Enter header name", "new_header")
        path = path + ".hrl"
        if path and not os.path.isfile(path):
            writeFile(path, "")
            GetTabMgr().LoadFile(path)

    def DefaultExcludeDirs(self):
        return ProjectExplorer.DefaultExcludeDirs(self) + ["ebin", ".settings"]

    def _GetTemplate(self, template):
        path = os.path.join(GetMainFrame().cwd, "data", "erlang", "templates", template + ".erl")
        data = readFile(path)
        data = data.replace("[username]", Config.UserName())
        data = data.replace("[date]", time.strftime("%d.%m.%Y"))
        return data

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