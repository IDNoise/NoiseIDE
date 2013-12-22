import wx
import wx.html
from idn_colorschema import ColorSchema

__author__ = 'IDNoise'


class HtmlWin(wx.html.HtmlWindow):
    def SetPage(self, text):
        wx.html.HtmlWindow.SetPage(self, '<body bgcolor="' + ColorSchema.codeEditor["completer_help_back"] +
                                         '"><font color="' + ColorSchema.codeEditor["completer_help_fore"] + '">' + text + '</font></body>')