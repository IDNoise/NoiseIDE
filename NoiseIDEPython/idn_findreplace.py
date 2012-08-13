from idn_global import GetTabMgr
from idn_utils import CreateButton

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
from wx import stc

class FindInFileDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, id = wx.NewId(), title = "Find / Replace in file", size = (520, 195))

        self.sizer = wx.GridBagSizer(2, 2)
        self.findTextLabel = wx.StaticText(self, label = "Find text:")
        self.findText = wx.ComboBox(self, size = (300, 25))
        self.replaceTextLabel = wx.StaticText(self, label = "Replace text:")
        self.replaceText = wx.ComboBox(self, size = (300, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind)
        self.replaceButton = CreateButton(self, "Replace", self.OnReplace)
        self.replaceAllButton = CreateButton(self, "Replace All", self.OnReplaceAll)

        self.wholeWordsCb = wx.CheckBox(self, label = "Whole words")
        self.matchCaseCb = wx.CheckBox(self, label = "Match case")
        self.useRegextCb = wx.CheckBox(self, label = "Regexp")
        self.searchUpCb = wx.CheckBox(self, label = "Search up")
        self.sizer.Add(self.findTextLabel, (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.findText, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.findButton, (0, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceTextLabel, (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceText, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceButton, (1, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceAllButton, (2, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.wholeWordsCb, (3, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.sizer.Add(self.matchCaseCb, (4, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.sizer.Add(self.useRegextCb, (5, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.sizer.Add(self.searchUpCb, (6, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.SetSizer(self.sizer)
        self.Layout()

    def OnFind(self, event):
        self.textToFind = self.findText.Value
        editor = GetTabMgr().CurrentPage()
        self.findSuccessful = False
        if self.textToFind and editor:
            if not self.textToFind in self.findText.Items:
                self.findText.Append(self.textToFind)

            findOptions = 0
            wholeWords = self.wholeWordsCb.Value
            matchCase = self.matchCaseCb.Value
            searchDown = not self.searchUpCb.Value
            useRegexp = self.useRegextCb.Value
            if useRegexp:
                findOptions |= stc.STC_FIND_REGEXP
            else:
                if wholeWords:
                    findOptions |= stc.STC_FIND_WHOLEWORD
                if matchCase:
                    findOptions |= stc.STC_FIND_MATCHCASE

            for attempt in range(2):
                caretPosition = editor.CurrentPos
                editor.SetAnchor(caretPosition)
                editor.SearchAnchor()
                if searchDown:
                    pos = editor.SearchNext(findOptions, self.textToFind)
                else:
                    pos = editor.SearchPrev(findOptions, self.textToFind)
                if pos >= 0:
                    if searchDown:
                        editor.GotoPos(pos + len(self.textToFind))
                        editor.SetAnchor(pos)
                    else:
                        editor.GotoPos(pos)
                        editor.SetAnchor(pos + len(self.textToFind))
                    self.findSuccessful = True
                    break
                else:
                    if attempt == 0:
                        if searchDown:
                            editor.SetCurrentPos(0)
                        else:
                            editor.SetCurrentPos(editor.Length)
                        continue
                    else:
                        editor.SetCurrentPos(caretPosition)
                        break

    def OnReplace(self, event):
        editor = GetTabMgr().CurrentPage()
        replaceText = self.replaceText.Value
        if editor and self.findText.Value == self.textToFind:
            if replaceText and not replaceText in self.replaceText.Items:
                self.replaceText.Append(replaceText)

            editor.ReplaceSelection(self.replaceText.Value)
            self.OnFind(None)

    def OnReplaceAll(self, event):
        if not self.findText.Value:
            return
        self.OnFind(None)
        found = self.findSuccessful
        while found:
            self.OnReplace(None)
            self.OnFind(None)
            found = self.findSuccessful