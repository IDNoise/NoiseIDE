from idn_outline import ErlangOutline

__author__ = 'Yaroslav Nikityshev aka IDNoise'


import os
import wx
import re
from wx import stc
from wx import html
from wx.stc import STC_FOLDLEVELHEADERFLAG, StyledTextCtrl
from idn_cache import ErlangCache, Function, Record, Macros, readFile, ExportedType
from idn_connect import CompileErrorInfo
from idn_token import ErlangTokenizer, ErlangTokenType
from idn_colorschema import ColorSchema
from idn_highlight import ErlangHighlightType
from idn_lexer import ErlangLexer
from idn_global import GetProject, GetTabMgr, GetMainFrame

class EditorFoldMixin:
    def __init__(self):
        self.FoldWidth = 10
        self.SetMarginType(3, stc.STC_MARGIN_SYMBOL)
        self.SetMarginMask(3, stc.STC_MASK_FOLDERS)
        self.SetMarginSensitive(3, True)
        self.SetMarginWidth(3, self.FoldWidth)
        self.SetProperty("fold", "1")


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

        self.Bind(stc.EVT_STC_MARGINCLICK, self.OnMarginClick)
        self.Bind(stc.EVT_STC_CHANGE, self.OnTextChanged)

    def OnMarginClick(self, event):
        lineNum = self.LineFromPosition(event.GetPosition())
        if event.GetMargin() == 3:
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
        self.SetMarginType(1, stc.STC_MARGIN_NUMBER)
        self.SetMarginMask(1, 0)
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
        self.SetMarginWidth(1, self.LineNumbersWidth())

    def GetDefaultFont(self):
        raise NotImplementedError

    def LineNumbersWidth(self):
        if self.lineNumbersEnabled:
            return 7 + len(str(self.GetLineCount())) * self.CalcFontWidth()
        else:
            return 0


class CustomSTC(StyledTextCtrl, EditorFoldMixin, EditorLineMarginMixin):

    ShowEOL = False
    ShowWhiteSpace = False

    def __init__(self, parent, markerPanel, filePath = None):
        #style = wx.MAXIMIZE_BOX|wx.RESIZE_BORDER|wx.SYSTEM_MENU|wx.CAPTION|wx.CLOSE_BOX
        StyledTextCtrl.__init__(self, parent, style = wx.NO_BORDER)#, style = style)
        EditorFoldMixin.__init__(self)
        EditorLineMarginMixin.__init__(self)

        self.tooltip = wx.ToolTip("a" * 500)
        self.tooltip.Enable(False)
        self.tooltip.SetDelay(300)
        self.SetToolTip(self.tooltip)

        self.markerPanel = markerPanel
        if self.markerPanel:
            self.markerPanel.Editor = self
            self.markerPanel.SetMarkerColor("selected_word",
                ColorSchema.codeEditor["selected_word_marker_color"])

        self.SetupLexer()
        self.filePath = None
        self.lastHighlightedWord = ""
        self.changed = False
        self.saved = True

        self.SetCaretWidth(3)
        self.SetCaretLineBackground(ColorSchema.codeEditor["current_line_background"])
        self.SetCaretLineVisible(True)

        #self.SetUseHorizontalScrollBar(True)
        self.SetEndAtLastLine(False)
        self.SetScrollWidthTracking(True)
        self.SetScrollWidth(140)

        self.SetTabWidth(4)
        self.SetUseTabs(False)
        self.SetTabIndents(True)
        self.SetBackSpaceUnIndents(True)
        self.SetIndent(4)
        self.SetEOLMode(stc.STC_EOL_LF)

        self.SetEdgeColumn(140)
        self.SetEdgeMode(stc.STC_EDGE_LINE)

        self.SetMargins(5, 5)

        self.SetMarginType(2, stc.STC_MARGIN_SYMBOL)
        self.SetMarginSensitive(2, True)
        self.SetMarginWidth(2, 10)

        self.IndicatorSetStyle(0, stc.STC_INDIC_ROUNDBOX)
        self.IndicatorSetForeground(0, ColorSchema.codeEditor["highlighted_word"])

        self.SetVisiblePolicy(stc.STC_VISIBLE_STRICT, 0)
        #self.SetYCaretPolicy(stc.STC_CARET_STRICT | stc.STC_CARET_EVEN, 0)


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

        if hasattr(self, "lexer"):
            self.Bind(stc.EVT_STC_STYLENEEDED, self.OnStyleNeeded)
        self.Bind(stc.EVT_STC_UPDATEUI, self.HighlightBrackets)
        self.Bind(stc.EVT_STC_CHARADDED, self.OnCharAdded)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)

        self.highlightTimer = wx.Timer(self, wx.NewId())
        self.Bind(wx.EVT_TIMER, self.OnHighlightTimer, self.highlightTimer)
        self.highlightTimer.Start(400)


        self.EnableLineNumbers()

        self.Bind(stc.EVT_STC_CHANGE , self.OnDocumentChanged)
        self.SetModEventMask(stc.STC_MOD_INSERTTEXT | stc.STC_MOD_DELETETEXT |
                             stc.STC_PERFORMED_USER | stc.STC_PERFORMED_UNDO |
                             stc.STC_PERFORMED_REDO)

        self.UpdateOptions()

        if filePath:
            self.LoadFile(filePath)
            self.Bind(stc.EVT_STC_SAVEPOINTLEFT, self.OnSavePointLeft)
            self.Bind(stc.EVT_STC_SAVEPOINTREACHED, self.OnSavePointReached)
        self.OnInit()

    def UpdateOptions(self):
        self.SetViewWhiteSpace(stc.STC_WS_VISIBLEALWAYS if CustomSTC.ShowWhiteSpace else  stc.STC_WS_INVISIBLE)
        self.SetViewEOL(CustomSTC.ShowEOL)

    def GetLastVisibleLine(self):
        """
        return the last visible line on the screen,
        taking into consideration the folded lines
        """
        return self.LineFromPosition(
            self.PositionFromPoint(
                wx.Point(self.GetPosition()[0],
                    self.GetPosition()[1] + self.GetSize()[1]))
        )

    def LoadFile(self, filePath):
        self.filePath = os.path.normcase(filePath)
        #print filePath
        self.ClearAll()
        StyledTextCtrl.LoadFile(self, self.filePath)
        self.savedText = self.GetText()
        self.changed = False
        self.saved = True
        self.lastHighlightedWord = ""
        self.SetSelection(0, 0)

    def OnInit(self):
        pass

    def SetupLexer(self):
        self.lexer = None

    def SetupLanguageStyles(self):
        pass

    def OnHighlightTimer(self, event):
        self.HighlightSelectedWord()

    def OnDocumentChanged(self, event):
        #if self.GetText() != self.
        self.Changed()
        event.Skip()

    def OnCharAdded(self, event):
        char = event.GetKey()
        if char == ord('\n'):
            self.DoIndent()
        event.Skip()

    def OnKeyDown(self, event):
        if self.HandleKeyDownEvent(event):
            return
        else:
            event.Skip()

    def HandleKeyDownEvent(self, event):
        keyCode = event.GetKeyCode()
        if keyCode in [wx.WXK_DOWN, wx.WXK_UP] and event.ControlDown() and event.ShiftDown():
            offset = -1 if keyCode == wx.WXK_UP else 1
            startLine = self.LineFromPosition(self.GetSelectionStart())
            endLine = self.LineFromPosition(self.GetSelectionEnd())
            self.SetSelectionStart(self.PositionFromLine(startLine))
            self.SetSelectionEnd(self.GetLineEndPosition(endLine))
            text = self.GetSelectedText()  + '\n'
            self.GotoLine(startLine)
            for i in range(endLine - startLine + 1):
                self.LineDelete()
            targetLine = startLine + offset
            self.GotoPos(self.PositionFromLine(targetLine))
            self.InsertText(-1, text)
            self.SetSelectionStart(self.PositionFromLine(startLine + offset))
            self.SetSelectionEnd(self.GetLineEndPosition(endLine + offset))
        elif keyCode == ord('S') and event.ControlDown():
            self.Save()
        elif keyCode == wx.WXK_SPACE and event.ControlDown():
            self.OnAutoComplete()
        elif keyCode == ord('G') and event.ControlDown():
            dlg = wx.TextEntryDialog(self, 'Line:', 'Goto Line', style = wx.OK | wx.CANCEL)
            dlg.SetValue("")
            if dlg.ShowModal() == wx.ID_OK:
                self.GotoLine(int(dlg.Value) - 1)
            dlg.Destroy()
        else:
            return False
        return True

    def Save(self):
        self.savedText = self.GetText()
        self.SaveFile(self.filePath)
        self.Changed(False)
        self.OnFileSaved()

    def OnAutoComplete(self):
        pass

    def OnFileSaved(self):
        pass

    def Changed(self, changed = True):
        self.changed = changed

    def OnSavePointLeft(self, event):
        self.saved = False
        self.UpdateTabTitle()

    def OnSavePointReached(self, event):
        self.saved = True
        self.UpdateTabTitle()

    def DoIndent(self):
        indent = self.GetLineIndentation(self.CurrentLine - 1)
        self.InsertText(self.CurrentPos, " " * indent)
        pos = self.PositionFromLine(self.CurrentLine)
        self.GotoPos(pos + indent)

    def UpdateTabTitle(self):
        index = GetTabMgr().FindPageIndexByPath(self.filePath)
        if index >= 0:
            if self.saved:
                title = self.FileName()
            else:
                title = "* " + self.FileName()
            GetTabMgr().SetPageText(index, title)

    def FileName(self):
        return os.path.basename(self.filePath)

    def GetDefaultFont(self):
        return self.font

    def OnStyleNeeded(self, event):
        if self.lexer:
            self.lexer.StyleEvent(event)
        else:
            event.Skip()

    def HighlightBrackets(self, event):
        event.Skip()
        pos = self.GetCurrentPos()
        char = self.GetCharAt(pos)
        self.BraceBadLight(-1)
        if not char in "()[]{}<>":
            self.BraceBadLight(-1) #clear
            return
        otherPos = self.BraceMatch(pos)
        if otherPos > 0:
            self.BraceHighlight(pos, otherPos)
        else:
            self.BraceBadLight(pos)

    def GetCharAt(self, pos):
        return chr(StyledTextCtrl.GetCharAt(self, pos))

    def ClearIndicator(self, incidc):
        self.SetIndicatorCurrent(incidc)
        self.IndicatorClearRange(incidc, self.Length)

    def HighlightSelectedWord(self):
        text = self.GetSelectedText()
        if self.lastHighlightedWord != text:
            #print "clear selected word"
            self.ClearIndicator(0)
            self.lastHighlightedWord = text
        else:
            return
        markers = []
        #print text
        if (text and True not in [c in text for c in [" ", "\n", "\r", ",", ".", ":"]]):
            self.SetIndicatorCurrent(0)
            self.SetSearchFlags(stc.STC_FIND_MATCHCASE | stc.STC_FIND_WHOLEWORD)
            self.SetTargetStart(0)
            self.SetTargetEnd(self.Length)
            index = self.SearchInTarget(text)

            while (index != -1 and index < self.Length):
                line = self.LineFromPosition(index)
                self.IndicatorFillRange(index, len(text))
                self.SetTargetStart(index + len(text))
                self.SetTargetEnd(self.Length)
                marker = Marker(line, self.GetLine(line), index, len(text))
                markers.append(marker)
                index = self.SearchInTarget(text)
        self.Refresh()
        if self.markerPanel:
            self.markerPanel.SetMarkers("selected_word", markers)

    def ShowToolTip(self, msg):
        self.tooltip.SetTip(msg)
        self.tooltip.Enable(True)

    def HideToolTip(self):
        self.tooltip.Enable(False)

class PythonSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_PYTHON)

class YAMLSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_YAML)

#class ErrorMarkersPanel(wx.)

class ErlangHighlightedSTCBase(CustomSTC):
    def SetupLexer(self):
        self.lexer = ErlangLexer(self)
        self.SetLexer(stc.STC_LEX_CONTAINER)

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("erlang")
        self.StyleSetSpec(ErlangHighlightType.DEFAULT, formats["default"])
        self.StyleSetSpec(ErlangHighlightType.STRING, formats["string"])
        self.StyleSetSpec(ErlangHighlightType.COMMENT, formats["comment"])
        self.StyleSetSpec(ErlangHighlightType.ARROW, formats["arrow"])
        self.StyleSetSpec(ErlangHighlightType.VAR, formats["variable"])
        self.StyleSetSpec(ErlangHighlightType.MACROS, formats["macros"])
        self.StyleSetSpec(ErlangHighlightType.ATOM, formats["atom"])
        self.StyleSetSpec(ErlangHighlightType.MODULE, formats["module"])
        self.StyleSetSpec(ErlangHighlightType.SPEC, formats["preproc"])
        self.StyleSetSpec(ErlangHighlightType.FUNCTION, formats["function"])
        self.StyleSetSpec(ErlangHighlightType.KEYWORD, formats["keyword"])
        self.StyleSetSpec(ErlangHighlightType.MODULEATTR, formats["moduleattr"])
        self.StyleSetSpec(ErlangHighlightType.PREPROC, formats["preproc"])
        self.StyleSetSpec(ErlangHighlightType.RECORD, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.RECORDDEF, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.NUMBER, formats["number"])
        self.StyleSetSpec(ErlangHighlightType.FUNDEC, formats["fundec"])
        self.StyleSetSpec(ErlangHighlightType.BRACKET, formats["bracket"])
        self.StyleSetSpec(ErlangHighlightType.BIF, formats["bif"])
        self.StyleSetSpec(ErlangHighlightType.FULLSTOP, formats["fullstop"])

class ErlangSTC(ErlangHighlightedSTCBase):
    TYPE_MODULE, TYPE_HRL, TYPE_UNKNOWN = range(3)

    MARKER_ERROR, MARKER_WARNING = (20, 21)

    def OnInit(self):
        self.completer = ErlangCompleter(self)

        self.overlay = wx.Overlay()

        self.navigateTo = None

        if self.ModuleType() == self.TYPE_MODULE:
            self.flyTimer = wx.Timer(self, wx.NewId())
            self.Bind(wx.EVT_TIMER, self.OnFlyTimer, self.flyTimer)
            self.flyTimer.Start(500)
            self.flyCompileHash = None

        self.lastErrors = []
        self.errorsLines = []
        self.markerPanel.SetMarkerColor("warning", ColorSchema.codeEditor["warning_line_color"])
        self.markerPanel.SetMarkerColor("error", ColorSchema.codeEditor["error_line_color"])

        self.HighlightErrors(GetProject().GetErrors(self.filePath))

        self.Bind(stc.EVT_STC_UPDATEUI, self.OnUpdateCompleter)
        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)
        self.Bind(wx.EVT_MIDDLE_DOWN, self.OnMiddleMouseClick)

    def SetupLanguageStyles(self):
        ErlangHighlightedSTCBase.SetupLanguageStyles(self)

        self.IndicatorSetStyle(1, stc.STC_INDIC_PLAIN)
        self.MarkerDefine(self.MARKER_ERROR, stc.STC_MARK_BACKGROUND,
            foreground = ColorSchema.codeEditor["error_line_color"],
            background = ColorSchema.codeEditor["error_line_color"])
        self.MarkerDefine(self.MARKER_WARNING, stc.STC_MARK_BACKGROUND,
            foreground = ColorSchema.codeEditor["warning_line_color"],
            background = ColorSchema.codeEditor["warning_line_color"])

    def ModuleName(self):
        name = self.FileName()
        if name.endswith(".erl"):
            return name[:-4]
        else:
            return name

    def ModuleType(self):
        name = self.ModuleName()
        if name.endswith('.hrl'): return self.TYPE_HRL
        elif name.find('.') < 0: return self.TYPE_MODULE
        return self.TYPE_UNKNOWN

    def OnAutoComplete(self):
        self.UpdateCompleter()
        self.completer.Show()

    def OnUpdateCompleter(self, event):
        event.Skip()
        if not self.completer.IsShown(): return
        self.UpdateCompleter()

    def UpdateCompleter(self, event = None):
        caretPos = self.GetCurrentPos()
        (isRecField, record, prefix) = self.lexer.RecordFieldUnderCursor()
        if isRecField:
            self.completer.UpdateRecordField(record, prefix)
        else:
            line = self.GetCurrentLine()
            prefix = self.GetTextRange(self.PositionFromLine(line), caretPos)
           # self.completer.Update(prefix, self.GetCharAt(caretPos + 1))
            self.completer.Update(prefix)
        self.completer.UpdateCompleterPosition(self.PointFromPosition(caretPos))

    def HandleKeyDownEvent(self, event):
        keyCode = event.GetKeyCode()
        #print keyCode
        result = CustomSTC.HandleKeyDownEvent(self, event)
        if result: return result
        if (self.completer.IsShown() and
            keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER,
                        wx.WXK_DOWN, wx.WXK_UP, wx.WXK_ESCAPE]):
            self.completer.OnKeyDown(event)
            return True
        elif keyCode == ord('H') and event.ControlDown():
            dlg = ErlangOutline(self, self.ModuleName())
            dlg.ShowModal()
            return True
        elif keyCode == ord('/') and event.ControlDown():
            self.CommentLines()
            return True
        elif keyCode == ord('E') and event.ControlDown():
            self.AddToExport()
            return True
        else:
            return False

    def CommentLines(self):
        start = self.GetSelectionStart()
        end = self.GetSelectionEnd()
        startLine = self.LineFromPosition(start)
        endLine = self.LineFromPosition(end)

        lines = range(startLine, endLine + 1)
        allComments = True
        for line in lines:
            text = self.GetLine(line).strip()
            if text and not text.startswith('%'):
                allComments = False
                break
        for line in lines:
            text = self.GetLine(line).rstrip()
            if not text: continue
            if allComments:
                text = text.replace("%", "", 1)
            else:
                text = "%" + text
            self.SetTargetStart(self.PositionFromLine(line))
            self.SetTargetEnd(self.GetLineEndPosition(line))
            self.ReplaceTarget(text)

    def OnMouseMove(self, event):
        event.Skip()
        self.ClearIndicator(1)
        self.navigateTo = None
        pos = self.PositionFromPoint(event.GetPosition())

        if event.GetPosition()[0] < self.LineNumbersWidth() + self.FoldWidth + 10:
            self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
            self.HideToolTip()
            self.completer.HideHelp()
            return

        if event.ControlDown() and self.HasFocus():
            if not self.CheckHelp(pos):
                self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
                self.completer.HideHelp()
        elif self.HasFocus():
            self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
            self.completer.HideHelp()
            line = self.LineFromPosition(pos)
            errs = list(filter(lambda e: e.line == line, self.lastErrors))
            if errs:
                msg = reduce(lambda msg, e: msg + e.msg + "\n", errs, "")
                self.ShowToolTip(msg)
            else:
                self.HideToolTip()


    def CheckHelp(self, pos):
        style = self.GetStyleAt(pos)
        if style not in [ErlangHighlightType.ATOM,
                         ErlangHighlightType.FUNCTION,
                         #ErlangHighlightType.FUNDEC,
                         ErlangHighlightType.MACROS,
                         ErlangHighlightType.MODULE,
                         ErlangHighlightType.RECORD]:
            #print "wrong style", style
            return False
        start = self.WordStartPosition(pos, True)
        end = self.WordEndPosition(pos, True)
        if start == end:
            #print "same pos"
            return False


        line = self.LineFromPosition(pos)
        lineStart = self.PositionFromLine(line)
        prefix = self.GetTextRange(lineStart, start)
        value = self.GetTextRange(start, end)
        #print value, prefix
        if style == ErlangHighlightType.FUNCTION:
            self.navigateTo = self.completer.ShowFunctionHelp(value, prefix, end)
        if style == ErlangHighlightType.RECORD:
            self.navigateTo = self.completer.ShowRecordHelp(value)
        if style == ErlangHighlightType.MACROS:
            self.navigateTo = self.completer.ShowMacrosHelp(value)
        if style in [ErlangHighlightType.ATOM, ErlangHighlightType.MODULE]:
            if value in ErlangCache.AllModules():
                self.navigateTo = (ErlangCache.moduleData[value].file, 0)

        if self.navigateTo:
            self.SetCursor(wx.StockCursor(wx.CURSOR_HAND))
            self.SetIndicatorCurrent(1)
            self.IndicatorFillRange(start, end - start)
        return True

    def OnMouseClick(self, event):
        if event.ControlDown():
            if self.navigateTo:
                editor = GetTabMgr().LoadFile(self.navigateTo[0])
                line = self.navigateTo[1] - 1
                editor.GotoLine(line)
                editor.EnsureVisibleEnforcePolicy(line)
                self.completer.HideHelp()
                return
        event.Skip()

    def OnMiddleMouseClick(self, event):
        if event.ControlDown():
            if self.navigateTo:
                self.completer.helpWindow.SetFocus()
                return
        event.Skip()

    def OnFlyTimer(self, event):
        if GetProject().IsFlyCompileEnabled() and self.changed:
            currentHash = hash(self.GetText())
            if currentHash == self.flyCompileHash: return
            self.flyCompileHash = currentHash
            self.CompileFly()

    def CompileFly(self):
        GetProject().CompileFileFly(os.path.basename(self.filePath), self.filePath, self.GetText())

    def HighlightErrors(self, errors):
        self.MarkerDeleteAll(20)
        self.MarkerDeleteAll(21)
        self.lastErrors = errors
        self.errorsLines = map(lambda x: x.line, errors)
        wMarkers = []
        eMarkers = []
        errors = sorted(errors, key = lambda e: e.type)
        for e in errors:
            if e.type == CompileErrorInfo.WARNING:
                wMarkers.append(Marker(e.line, e.msg))
                indic = self.MARKER_WARNING
            else:
                eMarkers.append(Marker(e.line, e.msg))
                indic = self.MARKER_ERROR
                self.MarkerDelete(e.line, self.MARKER_WARNING)
            self.MarkerAdd(e.line, indic)
        self.markerPanel.SetMarkers("warning", wMarkers)
        self.markerPanel.SetMarkers("error", eMarkers)

    def OnFileSaved(self):
        GetProject().FileSaved(self.filePath)

    def DoIndent(self):
        text = self.GetLine(self.CurrentLine - 1).strip()
        indent = self.GetLineIndentation(self.CurrentLine - 1)
        if (text.endswith("{") or
            text.endswith("[") or
            text.endswith("[") or
            text.endswith("||") or
            text.endswith("=") or
            text.endswith("begin") or
            #text.endswith("andalso") or
            #text.endswith("orelse") or
            text.endswith("when") or
            text.endswith("of") or
            text.endswith("->") or
            text.endswith("(")):
            indent += 4
        elif (text.endswith(".")):
            indent -= 4
        else:
            for (op, cl) in [("(", ")"), ("{", "}"), ("[", "]")]:
                if text.count(op) > text.count(cl) and text.endswith(","):
                    indent += 4

        indent = 0 if indent < 0 else indent
        self.InsertText(self.CurrentPos, " " * indent)
        pos = self.PositionFromLine(self.CurrentLine)
        self.GotoPos(pos + indent)

    def AddToExport(self):
        line = self.GetCurrentLine()
        pos = self.PositionFromLine(line)
        text = self.GetLine(line)
        tokens = self.lexer.highlighter.GetHighlightingTokens(text)
        if not tokens[0].type == ErlangHighlightType.FUNDEC:
            return

        fun = self.GetTextRange(pos + tokens[0].start, pos + tokens[0].end)
        arity = self.completer.GetFunArity(pos + tokens[0].end)
        fun = ",\n    {}/{}".format(fun, arity)
        (exports, insertPos) = self.lexer.GetAllExports()
        if fun in exports:
            return
        if insertPos:
            self.InsertText(insertPos, fun)

class ErlangCompleter(wx.Frame):
    SIZE = (740, 270)
    LIST_SIZE = (320, 150)

    def __init__(self, stc):
        style = wx.BORDER_NONE | wx.STAY_ON_TOP | wx.FRAME_NO_TASKBAR
        pre = wx.PreFrame()
        pre.SetBackgroundStyle(wx.BG_STYLE_TRANSPARENT)
        pre.Create(stc, size = self.SIZE, style = style)
        self.PostCreate(pre)
        self.tokenizer = ErlangTokenizer()

        self.stc = stc
        self.lineHeight = stc.TextHeight(0) + 2
        self.module = self.stc.ModuleName()
        self.moduleType = self.stc.ModuleType()
        self.separators = ",;([{<-"
        self.lastText = None
        self.showingHelp = False

        self.list = wx.ListBox(self, size = self.LIST_SIZE,
            style = wx.LB_SORT | wx.LB_SINGLE | wx.WANTS_CHARS)
        self.helpWindow = wx.html.HtmlWindow(self)

        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(self.list)
        self.sizer.Add(self.helpWindow, 1, wx.EXPAND)
        self.SetSizer(self.sizer)
        self.Layout()
        self.Hide()

        self.list.Bind(wx.EVT_LISTBOX_DCLICK, self.OnItemDoubleClick)
        self.list.Bind(wx.EVT_LISTBOX, self.OnMouseItemSelected)
        self.list.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.stc.Bind(wx.EVT_MOUSE_EVENTS, self.OnSTCMouseDown)

    def OnSTCMouseDown(self, event):
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

    def UpdateRecordField(self, record, prefix):
        self.prefix = prefix.strip()
        fields = ErlangCache.RecordFields(self.module, record)
        self._PrepareData(fields)

    def ValidateCompleter(self):
        if len(self.list.GetStrings()) == 0:
            self.HideCompleter()
            return
        self.list.SetSelection(0)
        self.OnItemSelected(0)

    def Update(self, text, nextChar = None):
        if self.lastText == text: return
        #print "update", text
        self.lastText = text
        tokens = self.tokenizer.GetTokens(text)
        tokens.reverse()
        data = []
        self.prefix = ""
        if not tokens:
            data = self.GetVars()
        else:
            fToken = tokens[0]
            fType = fToken.type
            fValue = fToken.value
            fIsAtom = fType == ErlangTokenType.ATOM

            if (fType == ErlangTokenType.SPACE or
                (len(tokens) == 1 and fIsAtom) or
                (fIsAtom and tokens[1].type == ErlangTokenType.SPACE) or
                (fIsAtom and tokens[1].value in self.separators) or
                #(fIsAtom and nextChar == "/") or
                fValue in self.separators):
                if fValue in self.separators or fType == ErlangTokenType.SPACE:
                    self.prefix = ""
                else:
                    self.prefix = fValue.strip()
                if self.moduleType == ErlangSTC.TYPE_MODULE:
                    data += ErlangCache.ModuleFunctions(self.module, False)
                    if True:
                        data += ErlangCache.Bifs()
                        data += ErlangCache.AllModules()
            elif (len(tokens) > 1 and
                  ((fIsAtom and tokens[1].value == ":") or fValue == ":")):
                i = 1 if fValue == ":" else 2
                moduleName = tokens[i].value
                self.prefix = "" if fValue == ":" else fValue
                data = ErlangCache.ModuleFunctions(moduleName)
            elif (fValue == "?" or fType == ErlangTokenType.MACROS):
                self.prefix = "" if fValue == "?" else fValue[1:]
                data = ErlangCache.Macroses(self.module)
            elif (len(tokens) > 2 and fIsAtom and tokens[1].value == "."
                  and tokens[2].type == ErlangTokenType.RECORD):
                self.prefix = fValue
                record = tokens[2].value[1:]
                data = ErlangCache.RecordFields(self.module, record)
            elif (len(tokens) > 1 and fValue == "." and tokens[1].type == ErlangTokenType.RECORD):
                self.prefix = ""
                record = tokens[1].value[1:]
                data = ErlangCache.RecordFields(self.module, record)
            elif fType == ErlangTokenType.RECORD or fValue == "#":
                self.prefix = "" if fValue == "#" else fValue[1:]
                data = ErlangCache.ModuleRecords(self.module)
            elif fType == ErlangTokenType.VAR:
                self.prefix = fValue
                data = self.GetVars()
        self._PrepareData(data)

    def _PrepareData(self, data):
        self.list.Clear()
        for d in data:
            help = None
            if isinstance(d, Function):
                if True:
                    text = "{}({})".format(d.name, ", ".join(d.params))
                else:
                    text = "{}/{}".format(d.name, d.arity)
                help = self._FunctionHelp(d)
            elif isinstance(d, Record):
                text = d.name
                help = self._RecordHelp(d)
            elif isinstance(d, ExportedType):
                text = d.name + "()"
                help = self._ExportedTypeHelp(d)
            elif isinstance(d, Macros):
                text = d.name
                help = self._MacrosHelp(d)
            elif isinstance(d, tuple):
                (text, help) = d
            else:
                text = d
            if text.startswith(self.prefix):
                self.list.Append(text, help)
        self.ValidateCompleter()

    def _RecordHelp(self, record):
        fields = record.FieldsData()
        fields = [f[0] + "&nbsp;&nbsp;::&nbsp;&nbsp;" + f[1]  for f in fields]
        return "#{} [<br/>&nbsp;&nbsp;&nbsp;{}<br/>]<br/><br/>{}:{}".format(record.name, ",<br/>&nbsp;&nbsp;&nbsp;".join(fields),
            record.module, record.line)

    def _ExportedTypeHelp(self, expType):
        return "Types:{}<br/><br/>{}:{}".format(expType.types, expType.module, expType.line)

    def _MacrosHelp(self, macros):
        return "?{} -> {}<br/><br/>{}:{}".format(macros.name, macros.value, macros.module, macros.line)

    def _FunctionHelp(self, fun):
        if not fun.docref:
            p = fun.params[:]
            t = p[:]
            if fun.types and not fun.docref:
                for i in range(len(p)):
                    t[i] = p[i] + " :: " + fun.types[i]
            res = [fun.result, ""]
            if " :: " in fun.result:
                res = fun.result.split(" :: ")
                t.append(fun.result)
            comment = fun.comment.replace("\n", "<br/>")
            if comment:
                comment += "<br/>"
            help = "{}{}({}) -> {}. <br/>Types:<br/>&nbsp;&nbsp;{}".format(comment, fun.name, ", ".join(p), res[0], ",<br/>&nbsp;&nbsp;".join(t))
        else:
            path = os.path.join(ErlangCache.ERLANG_LIBS_CACHE_DIR, fun.docref)
            help = readFile(path)
        return help

    def GetVars(self):
        funData = self.stc.lexer.GetCurrentFunction()
        if funData:
            text = funData[3][:self.stc.GetCurrentPos()]
            tokens = self.tokenizer.GetTokens(text)
            return list({token.value for token in tokens
                         if token.type == ErlangTokenType.VAR and token.value != self.prefix})
        return []

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
            if isinstance(help, tuple):
                path = os.path.join(ErlangCache.ERLANG_LIBS_CACHE_DIR, help[1])
                text = readFile(path)
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

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER]:
            self.AutoComplete(self.list.GetString(self.list.GetSelection()))
        elif keyCode == wx.WXK_UP:
            current = self.list.GetSelection()
            if current == 0:
                current = self.list.Count - 1
            else:
                current -= 1
            self.list.SetSelection(current)
            self.OnItemSelected(current)
        elif keyCode == wx.WXK_DOWN:
            current = self.list.GetSelection()
            if current == self.list.Count - 1:
                current = 0
            else:
                current += 1
            self.list.SetSelection(current)
            self.OnItemSelected(current)
        elif keyCode == wx.WXK_ESCAPE:
            self.HideCompleter()


    def AutoComplete(self, text):
        toInsert = text[len(self.prefix):]
        self.stc.AddText(toInsert)
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

    def ShowFunctionHelp(self, fun, prefix, pos):
        arity = self.GetFunArity(pos)
        module = ""
        if prefix and prefix[-1] == ":" and prefix[-2].isalpha():
            i = -2
            while prefix[i].isalpha() or prefix[i] == "_":
                module += prefix[i]
                i -=1
            module = module[::-1]
            if not module.islower():
                module = self.module
        else:
            module = self.module
        #print "show fun help", module, fun, arity
        data = ErlangCache.ModuleFunction(module, fun, arity)
        if not data:
            data = ErlangCache.ModuleExportedData(module, fun)
            if not data:
                data = ErlangCache.ModuleExportedData(module + ".hrl", fun)
                if not data:
                    return
            help = self._ExportedTypeHelp(data)
        else:
            help = self._FunctionHelp(data)
        self.ShowHelp(help)
        return (data.moduleData.file, data.line)

    def GetFunArity(self, pos):
        open = ['[', '(', '{']
        close = [']', ')', '}']
        startPos = pos
        lvl = 0
        if self.stc.GetCharAt(pos) == "/":
            pos += 1
            arity = ""
            while self.stc.GetCharAt(pos).isdigit():
                arity += self.stc.GetCharAt(pos)
                pos += 1
            return int(arity)

        arity = 0
        if self.stc.GetCharAt(pos) != "(": return 0
        sF = pos + 1 if pos + 1 < self.stc.GetLength() else self.stc.GetLength() - 1
        sT = pos + 6 if pos + 6 < self.stc.GetLength() else self.stc.GetLength() - 1
        #print pos, self.stc.GetLength(), sF, sT
        postfix = self.stc.GetText()[sF : sT].strip()
        if self.stc.GetCharAt(pos) == "(" and postfix and postfix[0] == ")":
            return 0
        else:
            arity = 1
        while True:
            if pos > self.stc.GetLength() or\
               abs(pos - startPos) > 2000: break
            c = self.stc.GetCharAt(pos)
            if c in open:
                lvl += 1
            elif c == "," and lvl == 1:
                arity += 1
            elif c in close:
                lvl = lvl - 1
            if lvl == 0:
                break
            pos = pos + 1
        return arity

    def ShowRecordHelp(self, record):
        recordData = ErlangCache.RecordData(self.module, record)
        if not recordData: return
        help = self._RecordHelp(recordData)
        self.ShowHelp(help)
        return (recordData.moduleData.file, recordData.line)

    def ShowMacrosHelp(self, macros):
        macrosData = ErlangCache.MacrosData(self.module, macros)
        if not macrosData: return
        help = self._MacrosHelp(macrosData)
        self.ShowHelp(help)
        return (macrosData.moduleData.file, macrosData.line)

    def ShowHelp(self, help):
        if help:
            #print help
            self.showingHelp = True
            self.UpdateCompleterPosition(None)
            self.helpWindow.SetPage(help)
            self.SetSize((self.SIZE[0] - self.LIST_SIZE[0], self.SIZE[1]))
            self.sizer.Hide(self.list)
            self.sizer.Show(self.helpWindow)
            self.Layout()
            wx.Frame.Show(self)
            self.stc.SetFocus()

        else:
            self.showingHelp = False
            self.Hide()

    def HideHelp(self):
        if self.showingHelp:
            self.helpWindow.SetPage("")
            self.showingHelp = False
            self.Hide()
        #self.Refresh()
        #self.stc.Refresh()

class ConsoleSTC(CustomSTC):
    def __init__(self, parent, markerPanel = None):
        CustomSTC.__init__(self, parent, markerPanel)
        self.EnableLineNumbers(False)
        self.SetMarginWidth(3, 0)

        self.SetCaretWidth(1)
        self.SetCaretLineBackground(ColorSchema.codeEditor["current_line_background"])
        self.SetCaretLineVisible(True)

        self.SetScrollWidth(500)
        self.SetReadOnly(True)

        self.SetMarginWidth(2, 0)

        self.SetEdgeMode(stc.STC_EDGE_NONE)

        self.SetEndAtLastLine(True)

        self.SetYCaretPolicy(stc.STC_CARET_SLOP, 10)

        self.SetToolTip(None)

    def Changed(self, changed = True):
        pass

    def Append(self, text):
        text += "\n"
        wx.CallAfter(self._AppendText, text)

    def _AppendText(self, text):
        try:
            linesCount = self.GetLineCount()
            self.SetReadOnly(False)
            self.AppendText(text)
            self.SetReadOnly(True)
            if self.GetLastVisibleLine() >= linesCount:
                self.ScrollToLine(self.GetLineCount())
        except Exception, e:
            print "append text error", e


class Marker:
    def __init__(self, line, msg, index = None, length = None):
        self.line = line
        self.msg = msg

        self.indexFrom = index
        self.length = length
        if index:
            self.indexTo = index + length

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
            color = self.markerColor[type]
            for marker in markers:
                y = float(height) / float(self.Editor.LineCount) * float(marker.line)
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


