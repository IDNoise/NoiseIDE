__author__ = 'Yaroslav Nikityshev aka IDNoise'


import os
import wx
from wx import stc
from wx.stc import STC_FOLDLEVELHEADERFLAG, StyledTextCtrl
from idn_colorschema import ColorSchema
from idn_highlight import ErlangHighlightType
from idn_lexer import ErlangLexer

class EditorFoldMixin:
    def __init__(self):
        self.Bind(stc.EVT_STC_MARGINCLICK, self.OnMarginClick)
        self.Bind(stc.EVT_STC_CHANGE, self.OnTextChanged)

    def OnMarginClick(self, event):
        lineNum = self.LineFromPosition(event.GetPosition())
        if event.GetMargin() == 2:
            self.ToggleFold(lineNum)
        event.Skip()

    def OnTextChanged(self, event):
        lineNum = self.LineFromPosition(event.GetPosition())
        if (self.GetFoldLevel(lineNum) & STC_FOLDLEVELHEADERFLAG and
            not self.GetFoldExpanded(lineNum)):
            self.ToggleFold(lineNum)
        event.Skip()

class EditorLineMarginMixin:
    def __init__(self):
        self.Bind(stc.EVT_STC_CHANGE, self.OnUpdateLineAreaWidth)
        self.Bind(stc.EVT_STC_ZOOM, self.OnUpdateLineAreaWidth)

    def CalcFontWidth(self):
        dc = wx.WindowDC(self)
        font = self.GetDefaultFont()
        defaultFontSize = font.GetPointSize()
        font.SetPointSize(defaultFontSize + self.GetZoom())
        dc.SetFont(font)
        defaultFontWidth, height = dc.GetTextExtent("9")
        font.SetPointSize(defaultFontSize)
        return defaultFontWidth

    def OnUpdateLineAreaWidth(self, event):
        if self.lineNumbersEnabled:
            self.EnableLineNumbers()
        event.Skip()


    def EnableLineNumbers(self, enable=True):
        self.lineNumbersEnabled = enable
        if enable:
            self.SetMarginWidth(1, 7 + len(str(self.GetLineCount())) * self.CalcFontWidth())
        else:
            self.SetMarginWidth(1, 0)

    def GetDefaultFont(self):
        raise NotImplementedError

class CustomSTC(StyledTextCtrl, EditorFoldMixin, EditorLineMarginMixin):
    def __init__(self, parent, filePath):
        #style = wx.MAXIMIZE_BOX|wx.RESIZE_BORDER|wx.SYSTEM_MENU|wx.CAPTION|wx.CLOSE_BOX
        StyledTextCtrl.__init__(self, parent)#, style = style)
        EditorFoldMixin.__init__(self)
        EditorLineMarginMixin.__init__(self)

        self.SetupLexer()
        self.filePath = filePath

        self.SetCaretWidth(3)
        self.SetCaretLineBackground(ColorSchema.codeEditor["current_line_background"])
        self.SetCaretLineVisible(True)

        #self.SetUseHorizontalScrollBar(True)
        self.SetEndAtLastLine(False)
        #self.SetScrollWidthTracking(True)
        self.SetScrollWidth(500)

        self.SetTabWidth(4)
        self.SetUseTabs(False)
        self.SetTabIndents(True)
        self.SetBackSpaceUnIndents(True)
        self.SetIndent(4)

        self.SetEdgeColumn(140)
        self.SetEdgeMode(stc.STC_EDGE_LINE)

        self.SetMargins(5, 5)
        self.SetProperty("fold", "1")

        self.SetMarginType(1, stc.STC_MARGIN_NUMBER)
        self.SetMarginMask(1, 0)

        self.SetMarginType(2, stc.STC_MARGIN_SYMBOL)
        self.SetMarginMask(2, stc.STC_MASK_FOLDERS)
        self.SetMarginSensitive(2, True)
        self.SetMarginWidth(2, 10)

        foreColor = ColorSchema.codeEditor["fold_area_foreground"]
        backColor = ColorSchema.codeEditor["fold_area_background"]
        self.MarkerDefine(stc.STC_MARKNUM_FOLDEROPEN, stc.STC_MARK_BOXMINUS, foreColor, backColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDER, stc.STC_MARK_BOXPLUS, foreColor, backColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDERSUB, stc.STC_MARK_VLINE, backColor, foreColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDERTAIL, stc.STC_MARK_LCORNER, foreColor, foreColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDEREND, stc.STC_MARK_VLINE, foreColor, foreColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDEROPENMID, stc.STC_MARK_VLINE, foreColor, foreColor)
        self.MarkerDefine(stc.STC_MARKNUM_FOLDERMIDTAIL, stc.STC_MARK_VLINE, foreColor, foreColor)
        self.SetFoldMarginColour(True, backColor)
        self.SetFoldMarginHiColour(True, backColor)

        self.font = wx.Font(ColorSchema.codeEditor["font_size"],
            wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL, False,
            ColorSchema.codeEditor["font_name"])

        self.StyleSetBackground(stc.STC_STYLE_DEFAULT, ColorSchema.codeEditor["background"])
        self.StyleSetForeground(stc.STC_STYLE_DEFAULT, ColorSchema.codeEditor["foreground"])
        self.StyleSetFont(stc.STC_STYLE_DEFAULT, self.font)
        self.StyleClearAll()
        self.StyleSetBackground(stc.STC_STYLE_LINENUMBER, ColorSchema.codeEditor["line_number_area_background"])
        self.SetupLanguageStyles()
        self.StyleSetBackground(stc.STC_STYLE_BRACELIGHT, ColorSchema.codeEditor["brace_background"])
        self.StyleSetForeground(stc.STC_STYLE_BRACELIGHT, ColorSchema.codeEditor["brace_foreground"])
        self.StyleSetBackground(stc.STC_STYLE_BRACEBAD, ColorSchema.codeEditor["brace_bad_background"])
        self.StyleSetForeground(stc.STC_STYLE_BRACEBAD, ColorSchema.codeEditor["brace_bad_foreground"])

        self.Bind(stc.EVT_STC_STYLENEEDED, self.OnStyleNeeded)
        self.Bind(stc.EVT_STC_UPDATEUI, self.HighlightBrackets)

        self.EnableLineNumbers()

        self.LoadFile(self.filePath)
        self.SetSelection(0, 0)

        #print(self.GetScrollWidth())
        #print(self.Size)

    def SetupLexer(self):
        self.lexer = None

    def SetupLanguageStyles(self):
        pass



    def FileName(self):
        return os.path.basename(self.filePath)

    def GetDefaultFont(self):
        return self.font

    def OnStyleNeeded(self, event):
        if self.lexer:
            self.lexer.StyleEvent(event)
        event.Skip()

    def HighlightBrackets(self, event):
        event.Skip()
        pos = self.GetCurrentPos()
        char = chr(self.GetCharAt(pos))
        if not char in "()[]{}<>":
            self.BraceBadLight(-1)
            return
        otherPos = self.BraceMatch(pos)
        if otherPos > 0:
            self.BraceHighlight(pos, otherPos)
        else:
            self.BraceBadLight(pos)

class ErlangSTC(CustomSTC):
    def SetupLexer(self):
        self.lexer = ErlangLexer()
        self.SetLexer(stc.STC_LEX_CONTAINER)

    def SetupLanguageStyles(self):
        self.StyleSetSpec(ErlangHighlightType.DEFAULT, ColorSchema.codeFormats["default"])
        self.StyleSetSpec(ErlangHighlightType.STRING, ColorSchema.codeFormats["string"])
        self.StyleSetSpec(ErlangHighlightType.COMMENT, ColorSchema.codeFormats["comment"])
        self.StyleSetSpec(ErlangHighlightType.ARROW, ColorSchema.codeFormats["arrow"])
        self.StyleSetSpec(ErlangHighlightType.VAR, ColorSchema.codeFormats["variable"])
        self.StyleSetSpec(ErlangHighlightType.MACROS, ColorSchema.codeFormats["macros"])
        self.StyleSetSpec(ErlangHighlightType.ATOM, ColorSchema.codeFormats["atom"])
        self.StyleSetSpec(ErlangHighlightType.MODULE, ColorSchema.codeFormats["module"])
        self.StyleSetSpec(ErlangHighlightType.SPEC, ColorSchema.codeFormats["preproc"])
        self.StyleSetSpec(ErlangHighlightType.FUNCTION, ColorSchema.codeFormats["function"])
        self.StyleSetSpec(ErlangHighlightType.KEYWORD, ColorSchema.codeFormats["keyword"])
        self.StyleSetSpec(ErlangHighlightType.MODULEATTR, ColorSchema.codeFormats["moduleattr"])
        self.StyleSetSpec(ErlangHighlightType.PREPROC, ColorSchema.codeFormats["preproc"])
        self.StyleSetSpec(ErlangHighlightType.RECORD, ColorSchema.codeFormats["record"])
        self.StyleSetSpec(ErlangHighlightType.RECORDDEF, ColorSchema.codeFormats["record"])
        self.StyleSetSpec(ErlangHighlightType.NUMBER, ColorSchema.codeFormats["number"])
        self.StyleSetSpec(ErlangHighlightType.FUNDEC, ColorSchema.codeFormats["fundec"])
        self.StyleSetSpec(ErlangHighlightType.BRACKET, ColorSchema.codeFormats["bracket"])
        self.StyleSetSpec(ErlangHighlightType.BIF, ColorSchema.codeFormats["bif"])
        self.StyleSetSpec(ErlangHighlightType.FULLSTOP, ColorSchema.codeFormats["fullstop"])