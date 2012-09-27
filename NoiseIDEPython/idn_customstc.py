from idn_config import Config
from idn_marker_panel import Marker

__author__ = 'Yaroslav Nikityshev aka IDNoise'


import os
import wx
from wx import stc
from wx import html
from wx.stc import STC_FOLDLEVELHEADERFLAG, StyledTextCtrl
from idn_colorschema import ColorSchema
from idn_global import  GetTabMgr, GetMainFrame

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

        self.tooltip = wx.ToolTip(" " * 500)
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

        self.SetCaretWidth(2)
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
        #self.SetMarginMask(2, stc.STC_MAS)
        self.SetMarginSensitive(2, True)
        self.SetMarginWidth(2, 10)

        self.IndicatorSetStyle(0, stc.STC_INDIC_ROUNDBOX)
        self.IndicatorSetForeground(0, ColorSchema.codeEditor["highlighted_word"])

        self.SetVisiblePolicy(stc.STC_CARET_STRICT | stc.STC_CARET_EVEN, 0)
        #self.SetYCaretPolicy(stc.STC_CARET_STRICT | stc.STC_CARET_EVEN, 0)
        self.SetYCaretPolicy(stc.STC_CARET_JUMPS | stc.STC_CARET_EVEN, 0)


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

        self.editorMenu = GetMainFrame().editorMenu
        self.SetupEditorMenu()

        self.Bind(wx.EVT_RIGHT_UP, self.CreatePopupMenu)

    def CreatePopupMenu(self, event):
        pass

    def OnClose(self):
        if self.saved == False and os.path.exists(self.filePath):
            dial = wx.MessageDialog(None,
                'You have unsaved changes in this document. Do you want to save it?'.format(file),
                'File has unsaved changed',
                wx.YES_NO | wx.NO_DEFAULT | wx.ICON_QUESTION)
            if dial.ShowModal() == wx.ID_YES:
                self.Save()
            else:
                self.OnFileSaved()

    def SetupEditorMenu(self):
        for item in self.editorMenu.GetMenuItems():
            self.editorMenu.RemoveItem(item)

        self.editorMenu.AppendMenuItem('Find in file', GetMainFrame(), lambda e: self.Parent.ShowFind(), "Ctrl-F")
        self.editorMenu.AppendMenuItem('Incremental find in file', GetMainFrame(), lambda e: self.Parent.ShowFind(True), "Alt-F")
        self.editorMenu.AppendMenuItem('Go to line', GetMainFrame(), lambda e: self.ShowGoToLineDialog(), "Ctrl-G")
        self.editorMenu.AppendSeparator()
        self.editorMenu.AppendCheckMenuItem('Show white space', GetMainFrame(), self.OnMenuShowWhiteSpace, Config.GetProp("show_white_space", False))
        self.editorMenu.AppendCheckMenuItem('Show EOL', GetMainFrame(), self.OnMenuShowEOL, Config.GetProp("show_eol", False))
       # self.editorMenu.AppendSeparator()

    def OnMenuShowWhiteSpace(self, event):
        newValue = not Config.GetProp("show_white_space", False)
        Config.SetProp("show_white_space", newValue)
        CustomSTC.ShowWhiteSpace = newValue
        for editor in GetTabMgr().Pages():
            editor.UpdateOptions()

    def OnMenuShowEOL(self, event):
        newValue = not Config.GetProp("show_eol", False)
        Config.SetProp("show_eol", newValue)
        CustomSTC.ShowEOL = newValue
        for editor in GetTabMgr().Pages():
            editor.UpdateOptions()



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
        #print keyCode
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
        elif ((keyCode == ord('K') and event.ControlDown() and event.ShiftDown()) or
            (keyCode == ord(',') and event.ControlDown())):
            self.GoToPrevOccurence()
        elif ((keyCode == ord('K') and event.ControlDown()) or
              (keyCode == ord('.') and event.ControlDown())):
            self.GoToNextOccurence()
        #elif keyCode == ord('G') and event.ControlDown():
        #    self.ShowGoToLineDialog()
        else:
            return False
        return True

    def GetWordUnderCursor(self):
        start = self.WordStartPosition(self.CurrentPos, True)
        end = self.WordEndPosition(self.CurrentPos, True)
        if start == end:
            return None
        return self.GetTextRange(start, end)

    def GoToPrevOccurence(self):
        self.GoToOccurence(False)

    def GoToNextOccurence(self):
        self.GoToOccurence()

    def GoToOccurence(self, down = True):
        word = self.GetWordUnderCursor()
        fun = self.SearchNext if down else self.SearchPrev
        if not word: return
        startPos = self.CurrentPos
        for i in range(2):
            self.SetAnchor(self.CurrentPos)
            self.SearchAnchor()
            pos = fun(0, word)
            if pos >= 0:
                self.GotoPos(pos + int(len(word) / 2))
#                if down:
#                    self.GotoPos(pos + int(len(word) / 2))
#                    self.SetAnchor(pos)
#                else:
#                    self.GotoPos(pos + int(len(word) / 2))
#                    self.SetAnchor(pos + int(len(word) / 2))
                return
            else:
                if i == 0:
                    if down:  self.SetCurrentPos(0)
                    else: self.SetCurrentPos(self.Length)
                else:
                    self.SetCurrentPos(startPos)

    def ShowGoToLineDialog(self):
        dlg = wx.TextEntryDialog(self, 'Line:', 'Goto Line', style = wx.OK | wx.CANCEL)
        dlg.SetValue("")
        if dlg.ShowModal() == wx.ID_OK:
            self.GotoLine(int(dlg.Value) - 1)
        dlg.Destroy()

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
        if (text and True not in [c in text for c in [" ", "\n", "\r", ","]]):
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

class HtmlWin(wx.html.HtmlWindow):
    def SetPage(self, text):
        wx.html.HtmlWindow.SetPage(self, '<body bgcolor="' + ColorSchema.codeEditor["completer_help_back"] +
                                         '"><font color="' + ColorSchema.codeEditor["completer_help_fore"] + '">' + text + '</font></body>')

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

    def Save(self):
        pass

    def Changed(self, changed = True):
        pass

    def Append(self, text):
        text = text.rstrip()
        text += "\n"
        self._AppendText(text)

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


