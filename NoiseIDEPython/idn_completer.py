from idn_htmlwin import HtmlWin

__author__ = 'IDNoise'

import wx
from idn_colorschema import ColorSchema
import core

class Completer(wx.Frame):
    SIZE = (760, 270)
    LIST_SIZE = (340, 150)

    def __init__(self, stc):
        style = wx.BORDER_NONE | wx.STAY_ON_TOP | wx.FRAME_NO_TASKBAR
        pre = wx.PreFrame()
        pre.SetBackgroundStyle(wx.BG_STYLE_TRANSPARENT)
        pre.Create(stc, size = self.SIZE, style = style)
        self.PostCreate(pre)

        self.stc = stc
        self.lineHeight = stc.TextHeight(0) + 2
        self.separators = ",;([{<-"
        self.lastText = None
        self.showingHelp = False
        self.prefix = ""

        self.list = wx.ListBox(self, size = self.LIST_SIZE, style = wx.LB_SORT | wx.LB_SINGLE | wx.WANTS_CHARS)
        self.list.SetBackgroundColour(ColorSchema.codeEditor["completer_list_back"])
        self.list.SetForegroundColour(ColorSchema.codeEditor["completer_list_fore"])

        self.helpWindow = HtmlWin(self)

        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(self.list)
        self.sizer.Add(self.helpWindow, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Hide()

        self.list.Bind(wx.EVT_LISTBOX_DCLICK, self.OnItemDoubleClick)
        self.list.Bind(wx.EVT_LISTBOX, self.OnMouseItemSelected)
        self.list.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.stc.Bind(wx.EVT_MOUSE_EVENTS, self.OnStcMouseDown)
        wx.GetApp().Bind(wx.EVT_ACTIVATE_APP, self.OnAppFocusLost)

    def OnAppFocusLost(self, event):
        try:
            self.HideCompleter()
        except:
            pass
        event.Skip()

    def OnStcMouseDown(self, event):
        event.Skip()
        if event.ButtonDown(wx.MOUSE_BTN_LEFT) or event.ButtonDown(wx.MOUSE_BTN_RIGHT):
            self.HideCompleter()

    def UpdateCompleterPosition(self, pos):
        if not self.showingHelp:
            pos = self.stc.ClientToScreen((pos[0], pos[1] + self.lineHeight))
            self.SetPosition(pos)
        else:
            pos = wx.GetMousePosition()
            pos = (pos[0], pos[1] + self.lineHeight)
            self.SetPosition(pos)

    def ValidateCompleter(self):
        if len(self.list.GetStrings()) == 0:
            self.HideCompleter()
            return
        self.list.SetSelection(0)
        self.OnItemSelected(0)

    def Update(self, text, nextChar = None):
        if self.lastText == text: return
        self.lastText = text
        self.prefix = ""
        self.OnUpdate(text, nextChar)
        self.ValidateCompleter()

    def OnMouseItemSelected(self, event):
        id = event.GetSelection()
        self.OnItemSelected(id)

    def OnItemSelected(self, id):
        help = self.list.GetClientData(id)
        if not help:
            self.helpWindow.SetPage("")
            self.SetSize(self.LIST_SIZE)
            self.sizer.Hide(self.helpWindow)
        else:
            text = help
            self.helpWindow.SetPage(text)
            self.sizer.Show(self.helpWindow)
            self.SetSize(self.SIZE)
        self.Layout()
        self.stc.Refresh()

    def OnItemDoubleClick(self, event):
        id = event.GetSelection()
        text = self.list.GetString(id)
        self.AutoComplete(text)

    def AutoComplete(self, text):
        toInsert = text[len(self.prefix):]
        self.stc.AddText(toInsert)
        self.HideCompleter()

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER]:
            self.AutoComplete(self.list.GetString(self.list.GetSelection()))
        elif keyCode == wx.WXK_UP or keyCode == wx.WXK_DOWN:
            current = self.list.GetSelection()
            if keyCode == wx.WXK_UP:
                if current == 0: current = self.list.Count - 1
                else: current -= 1
            else:
                if current == self.list.Count - 1: current = 0
                else: current += 1
            self.list.SetSelection(current)
            self.OnItemSelected(current)
        elif keyCode == wx.WXK_ESCAPE:
            self.HideCompleter()

    def HideCompleter(self):
        wx.Frame.Hide(self)
        self.helpWindow.SetPage("")

    def Show(self, show = True):
        if not self.helpWindow.ToText():
            self.sizer.Hide(self.helpWindow)
        else:
            self.sizer.Show(self.helpWindow)
        self.sizer.Show(self.list)
        self.Layout()
        if len(self.list.GetStrings()) > 0:
            wx.Frame.Show(self, show)
            self.stc.SetFocus()

    def OnUpdate(self, text, nextChar = None):
        pass

