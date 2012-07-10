__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from wx.lib.agw import aui
from idn_utils import extension
from idn_customstc import CustomSTC, ErlangSTC, YAMLSTC, PythonSTC

EXT_STC_TYPE = {
    ".erl": ErlangSTC,
    ".yaml": YAMLSTC,
    ".py": PythonSTC
}

class Notebook(aui.AuiNotebook):
    def __getitem__(self, index):
        if index < self.GetPageCount():
            return self.GetPage(index)
        else:
            raise IndexError

    def Pages(self):
        return [self.GetPage(index) for index in range(self.GetPageCount())]

    def OpenedFiles(self):
        return [p.filePath for p in self.Pages()]

    def LoadFile(self, file):
        if file.lower() in map(lambda p: p.lower(), self.OpenedFiles()):
            id = self.FindPageIndexByPath(file)
            editor = self[id]
            self.SetSelection(id)
            editor.SetFocus()
            return editor
        stcType = self.GetSTCTypeByExt(file)
        editor = stcType(self, file)
        self.AddPage(editor, editor.FileName(), True)
        editor.SetFocus()
        return editor

    def GetSTCTypeByExt(self, file):
        ext = extension(file)
        if ext in EXT_STC_TYPE:
            return EXT_STC_TYPE[ext]
        else:
            return CustomSTC

    def FindPageIndexByPath(self, path):
        for index in range(self.GetPageCount()):
            if self.GetPage(index).filePath.lower() == path.lower():
                return index
        return None

