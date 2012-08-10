from idn_global import GetTabMgr
from idn_utils import CreateButton

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
from wx import stc

class FindInFileDialog(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, id = wx.NewId(), title = "Find / Replace in file", size = (500, 300))

        self.sizer = wx.GridBagSizer()
        self.findText = wx.ComboBox(self, size = (300, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind)
        self.findButton.Size = (100, 25)
        self.sizer.Add(self.findText, (0, 0))
        self.sizer.Add(self.findButton, (0, 1))
        self.SetSizer(self.sizer)
        self.Layout()

    def OnFind(self, event):
        findText = self.findText.Value
        editor = GetTabMgr().CurrentPage()
        self.findSuccessful = False
        if findText and editor:
            if not findText in self.findText.Items:
                self.findText.Append(findText)

            findOptions = 0
            wholeWords = True
            matchCase = True
            searchDown = True
            if wholeWords:
                findOptions |= stc.STC_FIND_WHOLEWORD
            if matchCase:
                findOptions |= stc.STC_FIND_MATCHCASE
            caretPosition = editor.CurrentPos
            editor.SetAnchor(caretPosition)
            for attempt in range(2):
                editor.SearchAnchor()
                if searchDown:
                    pos = editor.SearchNext(findOptions, findText)
                else:
                    pos = editor.SearchPrev(findOptions, findText)
                if pos >= 0:
                    editor.GotoPos(pos + len(findText))
                    editor.SetAnchor(pos)
                    self.findSuccessful = True
                    break
                else:
                    if attempt == 0:
                        editor.SetCurrentPos(0)
                        continue
                    else:
                        editor.SetCurrentPos(caretPosition)
                        break


