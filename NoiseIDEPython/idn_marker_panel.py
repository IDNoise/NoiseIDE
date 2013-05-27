import wx
from idn_colorschema import ColorSchema

__author__ = 'Yaroslav'

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

        self.tooltip = wx.ToolTip(" " * 500)
        self.ShowToolTip(" " * 500)
        self.ShowToolTip(" ")
        self.HideToolTip()
        self.SetToolTip(self.tooltip)
        self.lastTip = None

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
            if not type in self.markerColor: return
            color = self.markerColor[type]
            for marker in markers:
                y = float(height) / float(self.Editor.GetScrollRange(wx.VERTICAL)) * float(marker.line)
                dc.SetPen(wx.Pen(color))
                dc.SetBrush(wx.Brush(color))
                dc.DrawRectangle(0, y, width, self.Height)
                self.areas.append((y, y + self.Height, marker.line))

    def OnFocus(self, event):
        self.Editor.SetFocus()

    def OnMouseMove(self, event):
        event.Skip()
        pos = event.GetPosition()
        y = pos[1]
        mouseLine = None
        for area in self.areas:
            if y >= area[0] and y <= area[1]:
                mouseLine = area[2]
        result = []
        if mouseLine:
            for type, markers in self.markers.items():
                for marker in markers:
                    if marker.line == mouseLine:
                        result.append(marker.msg)
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
        if not mouseLine:
            mouseLine = y * float(self.Editor.LineCount) / float(self.Size[1])
        result = None
        if mouseLine:
            for type, markers in self.markers.items():
                for marker in markers:
                    if marker.line == mouseLine:
                        result = marker
                        break
            if result:
                self.Editor.GotoLine(result.line)
                if result.indexFrom:
                    self.Editor.SetSelection(result.indexFrom, result.indexTo)
                return
            else:
                self.Editor.GotoLine(mouseLine)
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


class Marker:
    def __init__(self, line, msg, index = None, length = None):
        self.line = line
        self.msg = msg

        self.indexFrom = index
        self.length = length
        if index:
            self.indexTo = index + length
