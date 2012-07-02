__author__ = 'Yaroslav Nikityshev aka IDNoise'

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
        if file in self.OpenedFiles(): return
        stcType = self.GetSTCTypeByExt(file)
        editor = stcType(self, file)
        self.AddPage(editor, editor.FileName(), True)

    def GetSTCTypeByExt(self, file):
        ext = extension(file)
        if ext in EXT_STC_TYPE:
            return EXT_STC_TYPE[ext]
        else:
            return CustomSTC

    def FindPageIndexByPath(self, path):
        for index in range(self.GetPageCount()):
            if self.GetPage(index).filePath == path:
                return index
        return None

