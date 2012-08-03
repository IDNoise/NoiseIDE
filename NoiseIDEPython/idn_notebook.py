from idn_colorschema import ColorSchema

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from wx.lib.agw import aui
from idn_utils import extension
from idn_customstc import CustomSTC, ErlangSTC, YAMLSTC, PythonSTC, MarkerPanel
import wx

EXT_STC_TYPE = {
    ".erl": ErlangSTC,
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

    def FindPageByPath(self, file):
        for page in self.Pages():
            if page.filePath.lower() == file.lower():
                return page
        return None

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
