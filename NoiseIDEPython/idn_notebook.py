from wx._aui import AUI_BUTTON_CLOSE, wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSED
from wx.aui import wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_UP, AuiNotebookEvent, AUI_NB_MIDDLE_CLICK_CLOSE
from idn_colorschema import ColorSchema
from idn_global import GetProject

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from wx.lib.agw import aui
from idn_utils import extension, Menu
from idn_customstc import CustomSTC, ErlangSTC, YAMLSTC, PythonSTC, MarkerPanel
import wx

EXT_STC_TYPE = {
    ".erl": ErlangSTC,
    ".hrl": ErlangSTC,
    ".yaml": YAMLSTC,
    ".py": PythonSTC
}

def GetSTCTypeByExt(file):
    ext = extension(file)
    if ext in EXT_STC_TYPE:
        return EXT_STC_TYPE[ext]
    else:
        return CustomSTC

class Notebook(aui.AuiNotebook):
    def __init__(self, parent):
        agwStyle = aui.AUI_NB_DEFAULT_STYLE ^ aui.AUI_NB_CLOSE_ON_ACTIVE_TAB
        aui.AuiNotebook.__init__(self, parent, agwStyle = agwStyle)

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

class EditorNotebook(aui.AuiNotebook):
    def __init__(self, parent):
        agwStyle = aui.AUI_NB_DEFAULT_STYLE |\
                   aui.AUI_NB_CLOSE_ON_ALL_TABS |\
                   aui.AUI_NB_SMART_TABS |\
                   aui.AUI_NB_TAB_FLOAT |\
                   aui.AUI_NB_WINDOWLIST_BUTTON
        aui.AuiNotebook.__init__(self, parent, agwStyle = agwStyle)
        self.Bind(aui.EVT_AUINOTEBOOK_TAB_RIGHT_UP, self.OnTabRightUp)

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

        def closeAll(event):
            while self.GetPageCount() > 0:
                self.ClosePage(0)

        def renameFile(event):
            GetProject().explorer.Rename(editor.filePath)

        menu = Menu()
        menu.AppendMenuItem("Rename", self, renameFile)
        menu.AppendSeparator()
        menu.AppendMenuItem("Close this tab", self, closeCurrent)
        menu.AppendMenuItem("Close other tabs", self, closeOther)
        menu.AppendMenuItem("Close all tabs", self, closeAll)
        self.PopupMenu(menu)


    def __getitem__(self, index):
        if index < self.GetPageCount():
            return self.GetPage(index).editor
        else:
            raise IndexError

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


class EditorPanel(wx.Panel):
    def __init__(self, parent, file):
        wx.Panel.__init__(self, parent, style = wx.TAB_TRAVERSAL | wx.NO_BORDER)
        stcType = GetSTCTypeByExt(file)
        self.markPanel = MarkerPanel(self)
        self.editor = stcType(self, self.markPanel, file)
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(self.editor, 1, wx.EXPAND)
        self.editor.markPanel = self.markPanel
        self.sizer.Add(self.markPanel, 0, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Fit()
