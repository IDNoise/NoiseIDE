__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
from wx.lib.agw import aui
from wx.aui import wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_UP
from idn_findreplace import FindInFilePanel
from idn_global import GetProject, GetWinMgr, GetMainFrame
from idn_utils import extension, Menu
from idn_customstc import CustomSTC, ErlangSTC, YAMLSTC, PythonSTC, MarkerPanel, ConsoleSTC

EXT_STC_TYPE = {
    ".erl": ErlangSTC,
    ".hrl": ErlangSTC,
    ".yaml": YAMLSTC,
    ".py": PythonSTC
}

def GetSTCTypeByExt(file):
    ext = extension(file)
    types = EXT_STC_TYPE
    if GetProject():
        types.update(GetProject().GetEditorTypes())
    if ext in types:
        return types[ext]
    else:
        return CustomSTC

class Notebook(aui.AuiNotebook):
    def __init__(self, parent):
        agwStyle = aui.AUI_NB_DEFAULT_STYLE ^ aui.AUI_NB_CLOSE_ON_ACTIVE_TAB
        aui.AuiNotebook.__init__(self, parent, agwStyle = agwStyle)

        self.Bind(aui.EVT_AUINOTEBOOK_TAB_DCLICK, self.OnTabDClick)

    def OnTabDClick(self, event):
        if GetMainFrame().ToolMgrPaneInfo.IsMaximized():
            GetWinMgr().RestorePane(GetMainFrame().ToolMgrPaneInfo)
        else:
            GetWinMgr().MaximizePane(GetMainFrame().ToolMgrPaneInfo)
        GetWinMgr().Update()

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

        event = wx.aui.AuiNotebookEvent(wx.aui.EVT_AUINOTEBOOK_PAGE_CLOSE.typeId, tabControl.Id)
        event.SetOldSelection(-1)
        event.SetSelection(page)
        event.SetEventObject(tabControl)
        event.SetInt(aui.AUI_BUTTON_CLOSE)
        self.OnTabButton(event)

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
        self.UpdateNavToolbar()

        self.Bind(aui.EVT_AUINOTEBOOK_TAB_RIGHT_UP, self.OnTabRightUp)
        self.Bind(aui.EVT_AUINOTEBOOK_TAB_DCLICK, self.OnTabDClick)

    def OnTabDClick(self, event):
        if GetMainFrame().TabMgrPaneInfo.IsMaximized():
            GetWinMgr().RestorePane(GetMainFrame().TabMgrPaneInfo)
        else:
            GetWinMgr().MaximizePane(GetMainFrame().TabMgrPaneInfo)
        GetWinMgr().Update()

    def OnTabRightUp(self, event):
        page = event.GetSelection()
        editor = self[page]
        def closeCurrent(event):
            self.ClosePage(page)

        def closeOther(event):
            i = 0
            while self.GetPageCount() > 1:
                if self[i] == editor:
                    i += 1
                self.ClosePage(i)

        def renameFile(event):
            GetProject().explorer.Rename(editor.filePath)

        menu = Menu()
        menu.AppendMenuItem("Rename", self, renameFile)
        menu.AppendSeparator()
        menu.AppendMenuItem("Close this tab", self, closeCurrent)
        menu.AppendMenuItem("Close other tabs", self, closeOther)
        menu.AppendMenuItem("Close all tabs", self, self.CloseAll)
        self.PopupMenu(menu)


    def __getitem__(self, index):
        if index < self.GetPageCount():
            return self.GetPage(index).editor
        else:
            raise IndexError

    def CloseAll(self, event = None):
        while self.GetPageCount() > 0:
            self.ClosePage(0)

    def Pages(self):
        return [self[index] for index in range(self.GetPageCount())]

    def OpenedFiles(self):
        return [p.filePath for p in self.Pages()]

    def LoadFile(self, file):
        id = self.FindPageIndexByPath(file)
        if id >= 0:
            editor = self[id]
            self.SetSelection(id)
            editor.SetFocus()
            return editor
        else:
            editorPanel = EditorPanel(self, file)
            self.AddPage(editorPanel, editorPanel.editor.FileName(), True)
            editorPanel.editor.SetFocus()
            return  editorPanel.editor

    def LoadFileLine(self, file, line = 0, addToHistory = True):
        editor = self.LoadFile(file)
        editor.GotoLine(line)
        if addToHistory:
            self.navigationHistory = self.navigationHistory[:self.navigationHistoryIndex + 1]
            self.navigationHistory.append((file, line))
            self.navigationHistoryIndex = len(self.navigationHistory) - 1
            #print self.navigationHistoryIndex, "history",  self.navigationHistory
        self.UpdateNavToolbar()
        editor.EnsureVisibleEnforcePolicy(line)
        return editor

    def FindPageIndexByPath(self, path):
        for index in range(self.GetPageCount()):
            if self[index].filePath.lower() == path.lower():
                return index
        return -1

    def FindPageIndexByEditor(self, editor):
        for index in range(self.GetPageCount()):
            if self[index] == editor.Parent:
                return index
        return None

    def FindPageByPath(self, file):
        for page in self.Pages():
            if page.filePath.lower() == file.lower():
                return page
        return None

    def ClosePage(self, page):
        tabControl = None
        for tabControl in self.GetChildren():
            if isinstance(tabControl, aui.AuiTabCtrl):
                break

        event = wx.aui.AuiNotebookEvent(wx.aui.EVT_AUINOTEBOOK_PAGE_CLOSE.typeId, tabControl.Id)
        event.SetOldSelection(-1)
        event.SetSelection(page)
        event.SetEventObject(tabControl)
        event.SetInt(aui.AUI_BUTTON_CLOSE)
        self.OnTabButton(event)

    def NavigateBack(self):
        if self.navigationHistoryIndex > 0 and self.navigationHistory:
            self.navigationHistoryIndex -= 1
            data = self.navigationHistory[self.navigationHistoryIndex]
            self.LoadFileLine(data[0], data[1], False)
        self.UpdateNavToolbar()

    def NavigateForward(self):
        if self.navigationHistoryIndex < len(self.navigationHistory) - 1 and self.navigationHistory:
            self.navigationHistoryIndex += 1
            data = self.navigationHistory[self.navigationHistoryIndex]
            self.LoadFileLine(data[0], data[1], False)
        self.UpdateNavToolbar()

    def UpdateNavToolbar(self):
        #print self.navigationHistoryIndex
        self.Parent.toolbar.EnableTool(self.Parent.navBackT.GetId(), (self.navigationHistory != [] and self.navigationHistoryIndex > 0))
        self.Parent.toolbar.EnableTool(self.Parent.navForwardT.GetId(),  (self.navigationHistory != [] and self.navigationHistoryIndex < (len(self.navigationHistory) - 1)))

class EditorPanel(wx.Panel):
    def __init__(self, parent, file):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        stcType = GetSTCTypeByExt(file)
        self.markPanel = MarkerPanel(self)
        self.editor = stcType(self, self.markPanel, file)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.findPanel = FindInFilePanel(self, self.editor)
        self.sizer.Add(self.findPanel)#, 1, wx.EXPAND)
        self.HideFind()
        hSizer = wx.BoxSizer(wx.HORIZONTAL)
        hSizer.Add(self.editor, 1, wx.EXPAND)
        hSizer.Add(self.markPanel, 0, wx.EXPAND)
        self.sizer.AddSizer(hSizer, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == ord('F') and event.ControlDown() and not event.ShiftDown() and not event.AltDown():
            self.ShowFind()
        elif keyCode == wx.WXK_ESCAPE and self.helpVisible:
            self.HideFind()
            event.Skip()
        elif keyCode == wx.WXK_F3 and self.helpVisible:
            self.findPanel.OnFind()
        else:
            event.Skip()

    def ShowFind(self):
        self.sizer.Show(self.findPanel, True)
        self.Layout()
        self.findPanel.findText.SetFocus()
        self.helpVisible = True

    def HideFind(self):
        self.sizer.Show(self.findPanel, False)
        self.Layout()
        self.helpVisible = False

class ConsolePanel(EditorPanel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        self.markPanel = MarkerPanel(self)
        self.editor = ConsoleSTC(self, self.markPanel)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.findPanel = FindInFilePanel(self, self.editor)
        self.sizer.Add(self.findPanel)#, 1, wx.EXPAND)
        self.HideFind()
        hSizer = wx.BoxSizer(wx.HORIZONTAL)
        hSizer.Add(self.editor, 1, wx.EXPAND)
        hSizer.Add(self.markPanel, 0, wx.EXPAND)
        self.sizer.AddSizer(hSizer, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)
