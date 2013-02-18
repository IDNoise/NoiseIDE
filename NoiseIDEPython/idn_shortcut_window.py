from idn_utils import CreateLabel

__author__ = 'IDNoise'


import wx

class ShortcutWindow(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, title = "Shortcuts", style = wx.DEFAULT_DIALOG_STYLE)

        self.panel = wx.ScrolledWindow(self)
        psizer = wx.BoxSizer(wx.VERTICAL)

        allEditorsSBSizer = wx.StaticBoxSizer(wx.StaticBox(self.panel, label = "All editors\consoles"))
        erlangSBSizer = wx.StaticBoxSizer(wx.StaticBox(self.panel, label = "Erlang"))
        erlangOptionSBSizer = wx.StaticBoxSizer(wx.StaticBox(self.panel, label = "Erlang with option eidtor"))
        psizer.Add(allEditorsSBSizer, border = 2, flag = wx.EXPAND)
        psizer.Add(erlangSBSizer, border = 2, flag = wx.EXPAND)
        psizer.AddSizer(erlangOptionSBSizer, border = 2, flag = wx.EXPAND)

        allEditorsSizer = wx.GridBagSizer(2, 2)
        allEditorsSBSizer.AddSizer(allEditorsSizer, flag = wx.EXPAND)

        erlangSizer = wx.GridBagSizer(2, 2)
        erlangSBSizer.AddSizer(erlangSizer, flag = wx.EXPAND)

        erlangOptionSizer = wx.GridBagSizer(2, 2)
        erlangOptionSBSizer.AddSizer(erlangOptionSizer, flag = wx.EXPAND)

        self.panel.SetSizer(psizer)
        self.panel.SetScrollbars(1, 1, 1, 1)


        i = {
            allEditorsSizer : 0,
            erlangSizer : 0,
            erlangOptionSizer : 0
         }
        def addShortcutRecord(sizer, what, button):
            text = CreateLabel(self.panel, what)
            text.SetMinSize((300, 20))
            sizer.Add(text, (i[sizer], 0), flag = wx.ALL | wx.ALIGN_LEFT, border = 1)
            sizer.Add(CreateLabel(self.panel, button), (i[sizer], 1), flag = wx.ALL | wx.ALIGN_LEFT, border = 1)
            i[sizer] += 1

        #addShortcutRecord("All editors\consoles:", "")
        addShortcutRecord(allEditorsSizer, "Open project file", "Ctrl-O")
        addShortcutRecord(allEditorsSizer, "Go to line", "Ctrl-G")
        addShortcutRecord(allEditorsSizer, "Find in file", "Ctrl-F")
        addShortcutRecord(allEditorsSizer, "Incremental find in file", "Alt-F")
        addShortcutRecord(allEditorsSizer, "Find/Replace in project", "Ctrl-Shift-F")
        addShortcutRecord(allEditorsSizer, "Close current editor", "Ctrl-W")
        addShortcutRecord(allEditorsSizer, "Go to next word occurrence", "F3")
        addShortcutRecord(allEditorsSizer, "Go to previous word occurrence", "Shift-F3")
        #addShortcutRecord("Erlang:", "")
        addShortcutRecord(erlangSizer, "Add to export:", "Ctrl-E")
        addShortcutRecord(erlangSizer, "Outline:", "Ctrl-H")
        addShortcutRecord(erlangSizer, "Go to export entry(cursor in fun)", "Ctrl-Up")
        addShortcutRecord(erlangSizer, "Go to fun body(cursor on fun name):", "Ctrl-Down")
        #addShortcutRecord("Erlang with compiled option:", "")
        addShortcutRecord(erlangOptionSizer, "Refresh", "Ctrl-R")
        self.Layout()
        psizer.SetSizeHints(self)

