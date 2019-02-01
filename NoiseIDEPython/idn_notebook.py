import os
from wx.lib.agw.aui.auibook import TabNavigatorWindow
from idn_colorschema import ColorSchema
from idn_config import Config
from idn_erlang_utils import IsModule, IsInclude
from idn_erlangstc import ErlangSTC, ErlangSTCReadOnly, IgorSTC
from idn_marker_panel import MarkerPanel

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
from wx.lib.agw import aui
from idn_findreplace import FindInFilePanel
import core
from idn_utils import extension, Menu, GetImage
from idn_customstc import CustomSTC, YAMLSTC, PythonSTC, ConsoleSTC, CppSTC, HtmlSTC

EXT_STC_TYPE = {
    ".erl":  ErlangSTC,
    ".hrl":  ErlangSTC,
    ".igor":  IgorSTC,
    ".yaml": YAMLSTC,
    ".cpp":  CppSTC,
    ".c":    CppSTC,
    ".h":    CppSTC,
    ".hpp":  CppSTC,
    ".py":   PythonSTC,
    ".html": HtmlSTC,
    ".xhtml":HtmlSTC,
    ".xml":  HtmlSTC
}

def GetSTCTypeByExt(filePath):
    ext = extension(filePath)
    types = EXT_STC_TYPE
    if core.Project:
        types.update(core.Project.GetEditorTypes())
    if ext in types:
        return types[ext]
    else:
        return CustomSTC

class Notebook(aui.AuiNotebook):
    def __init__(self, parent):
        agwStyle = aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_CLOSE_ON_ALL_TABS
        aui.AuiNotebook.__init__(self, parent, agwStyle = agwStyle)

        self.Bind(aui.EVT_AUINOTEBOOK_TAB_DCLICK, self.OnTabDClick)


    def DeletePage(self, page_idx, permanent = False):
        widget = self[page_idx]
        if widget and not permanent and hasattr(widget, "onlyHide"):
            self.RemovePage(page_idx)
            widget.Hide()
        else:
            if hasattr(widget, "CleanUp"):
                widget.CleanUp()
            aui.AuiNotebook.DeletePage(self, page_idx)

    def OnTabDClick(self, event):
        if core.MainFrame.ToolMgrPaneInfo.IsMaximized():
            core.WinMgr.RestorePane(core.MainFrame.ToolMgrPaneInfo)
        else:
            core.WinMgr.MaximizePane(core.MainFrame.ToolMgrPaneInfo)
        core.WinMgr.Update()

    def __getitem__(self, index):
        if index < self.GetPageCount():
            return self.GetPage(index)
        else:
            raise IndexError

    def Pages(self):
        return [self[index] for index in range(self.GetPageCount())]

    def FindPageIndexByTitle(self, title):
        for index in range(self.GetPageCount()):
            if self.GetPageText(index).lower() == title:
                return index
        return -1

    def FindPageIndexByWindow(self, window):
        for index in range(self.GetPageCount()):
            if self[index] == window:
                return index
        return None

    def FindPageByTitle(self, title):
        index = self.FindPageIndexByTitle(title)
        if index >= 0:
            return self[index]
        return None

    def CurrentPage(self):
        currentPage = self.GetSelection()
        if currentPage == -1:
            return None
        return self[currentPage]

    def ClosePage(self, page):
        tabControl = None
        for tabControl in self.GetChildren():
            if isinstance(tabControl, aui.AuiTabCtrl):
                break
        if page:
            event = aui.AuiNotebookEvent(aui.EVT_AUINOTEBOOK_PAGE_CLOSE.typeId, tabControl.Id)
            event.SetOldSelection(-1)
            event.SetSelection(page)
            event.SetEventObject(tabControl)
            event.SetInt(aui.AUI_BUTTON_CLOSE)
            self.OnTabButton(event)

    def FocusOnWidget(self, widget):
        index = self.FindPageIndexByWindow(widget)
        if index:
            self.SetSelection(index)

    def CloseAll(self, event = None):
        while self.GetPageCount() > 0:
            self.DeletePage(0, True)

class EditorNotebook(aui.AuiNotebook):

    def __init__(self, parent):
        agwStyle = aui.AUI_NB_DEFAULT_STYLE |\
                   aui.AUI_NB_CLOSE_ON_ALL_TABS |\
                   aui.AUI_NB_SMART_TABS |\
                   aui.AUI_NB_TAB_FLOAT |\
                   aui.AUI_NB_WINDOWLIST_BUTTON
        aui.AuiNotebook.__init__(self, parent, agwStyle = agwStyle)

        self.navigationHistory = []
        self.navigationHistoryIndex = 0
        self.pageOpenOrder = []
        self.UpdateNavToolbar()

        self.Bind(aui.EVT_AUINOTEBOOK_TAB_RIGHT_UP, self.OnTabRightUp)
        self.Bind(aui.EVT_AUINOTEBOOK_TAB_DCLICK, self.OnTabDClick)
        self.Bind(aui.EVT_AUINOTEBOOK_PAGE_CHANGED, self.OnPageChanged)
        self.Bind(aui.EVT_AUINOTEBOOK_PAGE_CLOSE, self.OnPageClose)


    def OnPageClose(self, event):
        page = event.GetSelection()
        editor = self[page]
        if hasattr(editor, "HasUnsavedChanged") and hasattr(editor, "CanCloseWithUnsavedChanges") \
                and editor.HasUnsavedChanged() and not editor.CanCloseWithUnsavedChanges():
            event.Veto()
            return

        editor.OnClose()
        if editor in self.pageOpenOrder:
            self.pageOpenOrder.remove(editor)
        if self.GetSelection() == page:
            self.NavigateBack(True)

    def OnNavigationKeyNotebook(self, event):
        def OnKeyUp(self, event):
            if event.GetKeyCode() == wx.WXK_CONTROL:
                self.CloseDialog()
            elif event.GetKeyCode() == wx.WXK_TAB:
                selected = self._listBox.GetSelection()
                maxItems = self.Parent.GetPageCount()

                if selected == maxItems - 1:
                    itemToSelect = 0
                else:
                    itemToSelect = selected + 1

                self._listBox.SetSelection(itemToSelect)
        TabNavigatorWindow.OnKeyUp = OnKeyUp
        aui.AuiNotebook.OnNavigationKeyNotebook(self, event)

    def OnPageChanged(self, event):
        editor = self.GetActiveEditor()
        if editor:
            if editor in self.pageOpenOrder:
                self.pageOpenOrder.remove(editor)
            self.pageOpenOrder.append(editor)
            editor.SetupEditorMenu()
        if event.GetOldSelection() >= 0:
            oldEditor = self[event.GetOldSelection()]
            if hasattr(oldEditor, "completer"):
                oldEditor.completer.HideCompleter()

    def OnTabDClick(self, event):
        if core.MainFrame.TabMgrPaneInfo.IsMaximized():
            core.WinMgr.RestorePane(core.MainFrame.TabMgrPaneInfo)
        else:
            core.WinMgr.MaximizePane(core.MainFrame.TabMgrPaneInfo)
        core.WinMgr.Update()

    def OnTabRightUp(self, event):
        page = event.GetSelection()
        editor = self[page]
        def closeCurrent(event):
            if hasattr(editor, "HasUnsavedChanged") and hasattr(editor, "CanCloseWithUnsavedChanges") \
                and editor.HasUnsavedChanged() and not editor.CanCloseWithUnsavedChanges():
                return
            self.ClosePage(page)

        def closeOther(event):
            for pageId in reversed(range(self.GetPageCount())):
                if self[pageId] == editor:
                    continue
                if hasattr(self[pageId], "HasUnsavedChanged") and hasattr(self[pageId], "CanCloseWithUnsavedChanges") \
                        and self[pageId].HasUnsavedChanged() and not self[pageId].CanCloseWithUnsavedChanges():
                    continue;
                self.ClosePage(pageId)


        def renameFile(event):
            core.Project.explorer.Rename(editor.filePath)

        menu = Menu()
        menu.AppendMenuItem("Rename", self, renameFile)
        menu.AppendSeparator()
        menu.AppendMenuItem("Close this tab", self, closeCurrent)
        menu.AppendMenuItem("Close other tabs", self, closeOther)
        menu.AppendMenuItem("Close all tabs", self, self.CloseAll)
        menu.AppendSeparator()
        menu.AppendMenuItem("Show in project explorer", self, lambda e: self.ShowInProjectExplorer(editor.filePath))
        self.PopupMenu(menu)

    def ShowInProjectExplorer(self, path):
        core.Project.explorer.SelectPath(path)

    def __getitem__(self, index):
        if index < self.GetPageCount():
            return self.GetPage(index).editor
        else:
            raise IndexError

    def CloseAll(self, event = None):
        for pageId in reversed(range(self.GetPageCount())):
            if self[pageId].HasUnsavedChanged() and not self[pageId].CanCloseWithUnsavedChanges():
                continue;
            self.ClosePage(pageId)

    def Pages(self):
        return [self[index] for index in range(self.GetPageCount())]

    def OpenedFiles(self):
        return [p.filePath for p in self.Pages()]

    def LoadFile(self, fileName):
        id = self.FindPageIndexByPath(fileName)
        if id >= 0:
            editor = self[id]
            self.SetSelection(id)
            editor.SetFocus()
            self.EnsureVisible(self.FindPageIndexByEditor(editor))
            if editor in self.pageOpenOrder:
                self.pageOpenOrder.remove(editor)
            self.pageOpenOrder.append(editor)
            return editor
        else:
            editorPanel = EditorPanel(self, fileName)
            #bitmap = self.GetBitmapForFile(fileName)
            self.AddPage(editorPanel, editorPanel.editor.FileName(), True)#, bitmap = bitmap)
            if len(self.Pages()) > Config.GetProp("max_tabs", 15):
                while self.FindPageIndexByEditor(self.pageOpenOrder[0]) == None:
                    self.pageOpenOrder.pop(0)
                if len(self.pageOpenOrder) > 0:
                    self.ClosePage(self.FindPageIndexByEditor(self.pageOpenOrder[0]))
            editorPanel.editor.SetFocus()
            self.EnsureVisible(self.FindPageIndexByEditor(editorPanel.editor))
            self.pageOpenOrder.append(editorPanel.editor)
            return editorPanel.editor

    def LoadFileLine(self, fileName, line = 0, addToHistory = True, fromLine = 0):
        prevEditor = self.GetActiveEditor()
        editor = self.LoadFile(fileName)
        editor.GotoLine(line)

        if addToHistory:
            self._AddToHistory(editor, prevEditor, fromLine)
        self.UpdateNavToolbar()
        editor.EnsureVisibleEnforcePolicy(line)
        self.EnsureVisible(self.FindPageIndexByEditor(editor))
        return editor

    def GetBitmapForFile(self, path, changed = False):
        if IsModule(path):
            fileName = "erlangModuleChanged.png" if changed else "erlangModule.png"
            return GetImage(os.path.join("tabs", fileName))
        elif IsInclude(path):
            fileName = "erlangIncludeChanged.png" if changed else "erlangInclude.png"
            return GetImage(os.path.join("tabs", fileName))
        else:
            return wx.NullBitmap

    def PageModified(self, path, changed = False):
        index = self.FindPageIndexByPath(path)
        if index >= 0:
            editor = self[index]
            if changed:
                title = "* " + editor.FileName()
            else:
                title = editor.FileName()
            self.SetPageText(index, title)
            #self.SetPageBitmap(index, core.TabMgr.GetBitmapForFile(path, changed))

    def AddCustomPage(self, page, title):
        self.AddPage(page, title, True)
        self.pageOpenOrder.append(page)

    def _AddToHistory(self, new, prev = None, prevLine = 0):
        self.navigationHistory = self.navigationHistory[:self.navigationHistoryIndex + 1]
        if prev:
            prevFile = prev.filePath
            if not prevLine:
                prevLine = prev.CurrentLine
            if not self.navigationHistory or self.navigationHistory[-1] != (prevFile, prevLine):
                self.navigationHistory.append((prevFile, prevLine))
        filePath = new.filePath
        line = new.CurrentLine
        if not self.navigationHistory or self.navigationHistory[-1] != (filePath, line):
            self.navigationHistory.append((filePath, line))
            self.navigationHistoryIndex = len(self.navigationHistory) - 1

    def FindPageIndexByPath(self, path):
        for index in range(self.GetPageCount()):
            if self[index].filePath.lower() == path.lower():
                return index
        return -1

    def FindPageIndexByEditor(self, editor):
        for index in range(self.GetPageCount()):
            if self[index] == editor:
                return index
        return None

    def FindPageByPath(self, fileName):
        for page in self.Pages():
            if page.filePath.lower() == fileName.lower():
                return page
        return None

    def ClosePage(self, page):
        tabControl = None
        for tabControl in self.GetChildren():
            if isinstance(tabControl, aui.AuiTabCtrl):
                break

        event = aui.AuiNotebookEvent(aui.EVT_AUINOTEBOOK_PAGE_CLOSE.typeId, tabControl.Id)
        event.SetOldSelection(-1)
        event.SetSelection(page)
        event.SetEventObject(tabControl)
        event.SetInt(aui.AUI_BUTTON_CLOSE)
        self.OnTabButton(event)

    def GetActiveEditor(self):
        if self.GetSelection() >= 0:
            return self[self.GetSelection()]
        return None

    def NavigateBack(self, pageMustExist = False):
        if self.navigationHistoryIndex > 0 and self.navigationHistory:
            self.navigationHistoryIndex -= 1
            data = self.navigationHistory[self.navigationHistoryIndex]
            if pageMustExist and data[0] not in self.OpenedFiles(): return
            self.LoadFileLine(data[0], data[1], False)
        self.UpdateNavToolbar()

    def NavigateForward(self):
        if self.navigationHistoryIndex < len(self.navigationHistory) - 1 and self.navigationHistory:
            self.navigationHistoryIndex += 1
            data = self.navigationHistory[self.navigationHistoryIndex]
            self.LoadFileLine(data[0], data[1], False)
        self.UpdateNavToolbar()

    def UpdateNavToolbar(self):
        self.Parent.toolbar.EnableTool(self.Parent.navBackT.GetId(), (self.navigationHistory != [] and self.navigationHistoryIndex > 0))
        self.Parent.toolbar.EnableTool(self.Parent.navForwardT.GetId(),  (self.navigationHistory != [] and self.navigationHistoryIndex < (len(self.navigationHistory) - 1)))

    def HighlightErrorPaths(self, pathErrors):
        for (path, hasErrors) in pathErrors:
            index = self.FindPageIndexByPath(path)
            if index >= 0:
                color = ColorSchema.codeEditor["error_tab_color"] if hasErrors else wx.NullColour
                self.SetPageTextColour(index, color)


class EditorPanel(wx.Panel):
    def __init__(self, parent, fileName):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        stcType = GetSTCTypeByExt(fileName)
        self.markPanel = MarkerPanel(self)
        self.editor = stcType(self, self.markPanel, fileName)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.findPanel = FindInFilePanel(self, self.editor)
        self.sizer.Add(self.findPanel)
        self.HideFind()
        hSizer = wx.BoxSizer(wx.HORIZONTAL)
        hSizer.Add(self.editor, 1, wx.EXPAND)
        hSizer.Add(self.markPanel, 0, wx.EXPAND)
        self.sizer.AddSizer(hSizer, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()
        self.findVisible = False

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == ord('F') and event.GetModifiers() == wx.MOD_CONTROL:
            self.ShowFind()
        elif keyCode == ord('F') and event.GetModifiers() == wx.MOD_ALT:
            self.ShowFind(True)
        elif keyCode == wx.WXK_ESCAPE and self.findVisible:
            self.HideFind()
        elif keyCode == wx.WXK_F3 and self.findVisible:
            self.findPanel.OnFind()
        elif keyCode == ord('W') and event.GetModifiers() == wx.MOD_CONTROL:
            index = self.Parent.FindPageIndexByEditor(self.editor)
            self.Parent.ClosePage(index)
        else:
            event.Skip()

    def ShowFind(self, incremental = False):
        self.sizer.Show(self.findPanel, True)
        self.findPanel.incremental = incremental
        self.Layout()
        if self.editor.SelectedText:
            self.findPanel.findText.Value = self.editor.SelectedText
            self.findPanel.findText.SetInsertionPointEnd()
        self.findPanel.findText.SetFocus()
        self.findVisible = True

    def HideFind(self):
        self.sizer.Show(self.findPanel, False)
        self.Layout()
        self.findVisible = False
        self.editor.SetFocus()

class ConsolePanel(EditorPanel):
    def __init__(self, parent, stcType = ConsoleSTC):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        self.markPanel = MarkerPanel(self)
        self.editor = stcType(self, self.markPanel)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.findPanel = FindInFilePanel(self, self.editor)
        self.sizer.Add(self.findPanel)
        self.HideFind()
        hSizer = wx.BoxSizer(wx.HORIZONTAL)
        hSizer.Add(self.editor, 1, wx.EXPAND)
        hSizer.Add(self.markPanel, 0, wx.EXPAND)
        self.sizer.AddSizer(hSizer, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

class ErlangCompileOptionPanel(EditorPanel):
    def __init__(self, parent, fileName, option, text):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        self.markPanel = MarkerPanel(self)
        self.editor = ErlangSTCReadOnly(self, self.markPanel, fileName, option, text)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.findPanel = FindInFilePanel(self, self.editor)
        self.sizer.Add(self.findPanel)
        self.HideFind()
        hSizer = wx.BoxSizer(wx.HORIZONTAL)
        hSizer.Add(self.editor, 1, wx.EXPAND)
        hSizer.Add(self.markPanel, 0, wx.EXPAND)
        self.sizer.AddSizer(hSizer, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

