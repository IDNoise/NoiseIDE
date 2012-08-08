from idn_global import GetTabMgr
from idn_utils import CreateButton

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx

class FindDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, id = wx.NewId(), title = "Find Replace", size = (500, 300))

        self.sizer = wx.BoxSizer()
        self.tabs = wx.Notebook(self)
        self.FindInFile = wx.Panel(self.tabs)
        self.FindInFile.sizer = wx.GridBagSizer()
        self.FindInFile.findText = wx.TextCtrl(self.FindInFile, size = (300, 25))
        self.FindInFile.findButton = CreateButton(self.FindInFile, "Find", self.OnFindInFile)
        self.FindInFile.findButton.Size = (100, 25)
        self.FindInFile.sizer.Add(self.FindInFile.findText, (0, 0))
        self.FindInFile.sizer.Add(self.FindInFile.findButton, (0, 1))
        self.FindInFile.SetSizer(self.FindInFile.sizer)
        self.FindInFile.Layout()

        self.tabs.AddPage(self.FindInFile, "Find in file")

        self.sizer.Add(self.tabs, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()

    def OnFindInFile(self, event):
        findText = self.FindInFile.findText.Value
        page = self.CurrentPage()
        if findText and page:
            index = 0
            results = []
            fileText = page.GetText()
            while True:
                index = fileText.find(findText, index)
                if index != -1:
                    results.append(index)
                    index += len(findText)
                else:
                    break
            print results
        print "no results"

    def CurrentPage(self):
        currentPage = GetTabMgr().GetSelection()
        if currentPage == -1:
            return None
        return GetTabMgr()[currentPage]
