from idn_colorschema import ColorSchema

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
from wx.lib.agw import aui
from idn_utils import extension
from idn_customstc import CustomSTC, ErlangSTC, YAMLSTC, PythonSTC
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

class MarkerPanel(wx.Panel):
    Editor = None
    Height = 4
    def __init__(self, parent):
        wx.Panel.__init__(self, parent, size = (10, 400))
        self.SetMinSize((10, 400))
        self.SetMaxSize((10, 20000))
        self.backColor = ColorSchema.codeEditor["marker_panel_background"]
        self.SetBackgroundColour(self.backColor)

        self.markers = {}
        self.markerColor = {}
        self.areas = []

        self.tooltip = wx.ToolTip("")
        self.tooltip.Enable(False)
        self.tooltip.SetDelay(300)
        self.tooltip.SetMaxWidth(400)
        self.SetToolTip(self.tooltip)

        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)
        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_SET_FOCUS, self.OnFocus)
        self.Bind(wx.EVT_PAINT, self.Paint)

    def Paint(self, event = None):
        dc = wx.ClientDC(self)
        width = self.Size[0]
        height = self.Size[1]

        dc.SetPen(wx.Pen(self.backColor))
        dc.SetBrush(wx.Brush(self.backColor))
        dc.DrawRectangle(0, 0, width, height)

        for type, markers in self.markers.items():
            color = self.markerColor[type]
            for (line, msg) in markers:
                y = height / self.Editor.LineCount * line
                dc.SetPen(wx.Pen(color))
                dc.SetBrush(wx.Brush(color))
                dc.DrawRectangle(0, y, width, self.Height)
                self.areas.append((y, y + self.Height, line))

    def OnFocus(self, event):
        self.Editor.SetFocus()

    def OnMouseMove(self, event):
        event.Skip()
        pos = event.GetPosition()
        y = pos[1]
        mouseLine = None
        for area in self.areas:
            if y > area[0] and y < area[1]:
                mouseLine = area[2]
        result = []
        if mouseLine:
            for type, markers in self.markers.items():
                for (line, msg) in markers:
                    if line == mouseLine:
                        result.append(msg)
            msg = "\n".join(result)
            if msg:
                self.ShowToolTip(msg)
                return

        self.HideToolTip()


    def OnMouseClick(self, event):
        pos = event.GetPosition()
        y = pos[1]
        mouseLine = None
        for area in self.areas:
            if y > area[0] and y < area[1]:
                mouseLine = area[2]
        result = []
        if mouseLine:
            for type, markers in self.markers.items():
                for (line, msg) in markers:
                    if line == mouseLine:
                        result = line
                        break
            if result:
                self.Editor.GotoLine(result)
                return
        event.Skip()


    def ClearAllMarkers(self):
        self.markers = {}
        self.Paint()

    def ClearMarker(self, type):
        self.markers[type] = []
        self.Paint()

    def SetMarkers(self, type, markers):
        self.markers[type] = markers
        self.Paint()

    def SetMarkerColor(self, type, color):
        self.markerColor[type] = color
        self.Paint()

    def ShowToolTip(self, msg):
        self.tooltip.SetTip(msg)
        self.tooltip.Enable(True)

    def HideToolTip(self):
        self.tooltip.Enable(False)