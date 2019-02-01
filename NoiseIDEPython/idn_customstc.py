__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from wx import stc
from wx import html
from wx.stc import STC_FOLDLEVELHEADERFLAG, StyledTextCtrl
from idn_colorschema import ColorSchema
import core
from idn_utils import Menu, readFile, writeFile
from idn_config import Config
from idn_marker_panel import Marker
from idn_snippet_completer import SnippetCompleter
import re
import collections
from stat import ST_MTIME
from idn_htmlwin import HtmlWin


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
    def __init__(self, parent, markerPanel, filePath = None):
        StyledTextCtrl.__init__(self, parent, style = wx.NO_BORDER)
        EditorFoldMixin.__init__(self)
        EditorLineMarginMixin.__init__(self)

        self.SetCodePage(wx.stc.STC_CP_UTF8)

        self.snippetCompleter = SnippetCompleter(self)
        self.snippetVarRegExp = re.compile(r"""\$[a-zA-Z0-9]*?\$""", re.VERBOSE | re.MULTILINE)
        self.snippetEditing = False

        self.findPanel = None
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
        self.closed = False

        self.SetCaretWidth(ColorSchema.codeEditor["caret_size"])
        self.SetCaretForeground(ColorSchema.codeEditor["caret_color"])
        self.SetCaretLineBackground(ColorSchema.codeEditor["current_line_background"])
        self.SetCaretLineVisible(True)

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

        self.SetVisiblePolicy(stc.STC_CARET_STRICT | stc.STC_CARET_EVEN, 0)
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
        self.SetSelBackground(True, ColorSchema.codeEditor["selection_background"])

        if hasattr(self, "lexer"):
            self.Bind(stc.EVT_STC_STYLENEEDED, self.OnStyleNeeded)
        self.Bind(stc.EVT_STC_UPDATEUI, self.HighlightBrackets)
        self.Bind(stc.EVT_STC_CHARADDED, self.OnCharAdded)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.Bind(stc.EVT_STC_UPDATEUI, self.OnUpdateSnippetCompleter)

        self.EnableLineNumbers()

        self.Bind(stc.EVT_STC_CHANGE , self.OnDocumentChanged)
        self.SetModEventMask(stc.STC_MOD_INSERTTEXT | stc.STC_MOD_DELETETEXT |
                             stc.STC_PERFORMED_USER | stc.STC_PERFORMED_UNDO |
                             stc.STC_PERFORMED_REDO)

        self.UpdateOptions()
        self.Bind(wx.EVT_WINDOW_DESTROY, self.OnDestroy)
        
        self.modifyCheckTimer = None
        if filePath:
            self.LoadFile(filePath)
            self.Bind(stc.EVT_STC_SAVEPOINTLEFT, self.OnSavePointLeft)
            self.Bind(stc.EVT_STC_SAVEPOINTREACHED, self.OnSavePointReached)
            self.StartModifyCheck()

        self.highlightTimer = wx.Timer(self, wx.ID_ANY)
        self.Bind(wx.EVT_TIMER, self.OnHighlightTimer, self.highlightTimer)
        self.highlightTimer.Start(400)
        
        self.editorMenu = core.MainFrame.editorMenu
        self.SetupEditorMenu()

        self.Bind(wx.EVT_RIGHT_UP, self.ShowPopupMenu)

        self.customTooltip = STCContextToolTip(self, self.OnRequestTooltipText)

        wx.CallAfter(self.OnInit)

    def OnDestroy(self, event):
        if not self.closed:
            self.OnClose()
        event.Skip()
        
    def StartModifyCheck(self):
        self.modifyCheckTimer = wx.Timer(self, wx.ID_ANY)
        self.Bind(wx.EVT_TIMER, self.OnModifyCheckTimer, self.modifyCheckTimer)
        self.modifyCheckTimer.Start(1000)
        self.modifyTime = os.stat(self.filePath)[ST_MTIME]

    def OnModifyCheckTimer(self, event):
        if Config.RefreshInterval() != 0: return
        modifyTime = os.stat(self.filePath)[ST_MTIME] if os.path.exists(self.filePath) else self.modifyTime
        if modifyTime != self.modifyTime:
            self.modifyTime = modifyTime
            core.Project.OnProjectFilesModified([self.filePath])

    def OnRequestTooltipText(self):
        return None

    def ShowPopupMenu(self, event):
        self.popupPos = self.PositionFromPoint(self.ScreenToClient(wx.GetMousePosition()))
        menu = self.CreatePopupMenu()
        if menu:
            self.PopupMenu(menu)

    def CreatePopupMenu(self):
        menu = Menu()
        editMenu = Menu()
        menu.AppendMenu(wx.ID_ANY, "Edit", editMenu)
        editMenu.AppendMenuItem("Cut", self, lambda e: self.Cut())
        editMenu.AppendMenuItem("Copy", self, lambda e: self.Copy())
        editMenu.AppendMenuItem("Paste", self, lambda e: self.Paste())
        return menu

    def HasUnsavedChanged(self):
        return self.saved == False and os.path.exists(self.filePath)

    def CanCloseWithUnsavedChanges(self):
        dial = wx.MessageDialog(None, 'You have unsaved changes in ' + self.filePath + '. Do you want to save it?'.format(
                                    self.filePath),
                                'File has unsaved changed',
                                wx.YES_NO | wx.CANCEL | wx.NO_DEFAULT | wx.ICON_QUESTION)
        result = dial.ShowModal()
        if result == wx.ID_CANCEL:
            return False
        elif result == wx.ID_YES:
            self.Save()
            return True
        elif result == wx.ID_NO:
            self.SetValue(self.savedText)
            self.Changed(False)
            self.saved = True
            return True

    def OnClose(self):
        self.closed = True
        if self.highlightTimer:
            self.highlightTimer.Stop()
        if self.modifyCheckTimer:
            self.modifyCheckTimer.Stop()

    def SetupEditorMenu(self):
        for item in self.editorMenu.GetMenuItems():
            self.editorMenu.RemoveItem(item)

        self.editorMenu.AppendMenuItem('Find in file', core.MainFrame, self.ShowFindInFile)
        self.editorMenu.AppendMenuItem('Incremental find in file', core.MainFrame, self.ShowIncrementalFindInFile)
        self.editorMenu.AppendMenuItem('Go to line', core.MainFrame, lambda e: self.ShowGoToLineDialog())
        self.editorMenu.AppendSeparator()
        self.editorMenu.AppendCheckMenuItem('Close brackets/quotes', core.MainFrame, self.OnMenuCloseBracketsQuotes, Config.GetProp("close_brackets_quotes", False))
        self.editorMenu.AppendCheckMenuItem('Put brackets/quotes around selected text', core.MainFrame, self.OnMenuPutBracketsQuotesAround, Config.GetProp("put_brackets_quotes_around", False))

    def OnMenuCloseBracketsQuotes(self, event):
        newValue = not Config.GetProp("close_brackets_quotes", False)
        Config.SetProp("close_brackets_quotes", newValue)

    def OnMenuPutBracketsQuotesAround(self, event):
        newValue = not Config.GetProp("put_brackets_quotes_around", False)
        Config.SetProp("put_brackets_quotes_around", newValue)

    def ShowFindInFile(self, event):
        if not self.HasFocus():
            event.Skip()
            return
        self.Parent.ShowFind()

    def ShowIncrementalFindInFile(self, event):
        if not self.HasFocus():
            event.Skip()
            return
        self.Parent.ShowFind(True)

    def UpdateOptions(self):
        self.SetViewWhiteSpace(stc.STC_WS_VISIBLEALWAYS if Config.GetProp("show_white_space", False) else  stc.STC_WS_INVISIBLE)
        self.SetViewEOL(Config.GetProp("show_eol", False))

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
        if 'nt' == os.name:
            filePath = filePath[0].upper() + filePath[1:]
        self.filePath = os.path.normpath(filePath)
        self.ClearAll()
        text = readFile(self.filePath)
        #self.SetText(text)
        self.SetValue(text)
        #self.SetTextUTF8(text)
        #StyledTextCtrl.LoadFile(self, self.filePath)
        self.modifyTime = os.stat(self.filePath)[ST_MTIME]
        self.savedText = text#self.GetText()
        self.changed = False
        self.saved = True
        self.lastHighlightedWord = ""
        self.SetSelection(0, 0)
        self.EmptyUndoBuffer()
        self.UpdateTabTitle()

    def OnInit(self):
        pass

    def SetupLexer(self):
        self.lexer = None

    def SetupLanguageStyles(self):
        pass

    def OnHighlightTimer(self, event):
        self.HighlightSelectedWord()

    def OnDocumentChanged(self, event):
        self.Changed()
        event.Skip()

    brackets = {'"': '"', "'": "'", '(': ')', '<': '>', '{' : '}', '[': ']'}
    def OnCharAdded(self, event):
        try:
            char = chr(event.GetKey())
        except:
            char = None
        if char == '\n':
            self.DoIndent()
        elif Config.GetProp("close_brackets_quotes", False) and char in self.brackets:
            self.AddTextUTF8(self.brackets[char])
        event.Skip()

    def OnKeyDown(self, event):
        if self.HandleKeyDownEvent(event):
            return
        else:
            event.Skip()

    bracketQuoteKeyMap = {
        (57, wx.MOD_SHIFT) : '(',
        (48, wx.MOD_SHIFT) : ')',
        (44, wx.MOD_SHIFT) : '<',
        (46, wx.MOD_SHIFT) : '>',
        (91, wx.MOD_NONE) : '[',
        (93, wx.MOD_NONE) : ']',
        (91, wx.MOD_SHIFT) : '{',
        (93, wx.MOD_SHIFT) : '}',
        (39, wx.MOD_NONE) : "'",
        (39, wx.MOD_SHIFT) : '"',
    }

    def HandleKeyDownEvent(self, event):
        keyCode = event.GetKeyCode()
        brKey = (keyCode, event.GetModifiers())
        if brKey in self.bracketQuoteKeyMap:
            bracketQuote = self.bracketQuoteKeyMap[brKey]
        else:
            bracketQuote = None

        if self.snippetEditing:
            if keyCode == wx.WXK_ESCAPE or keyCode == wx.WXK_RETURN:
                self.StopMacroEditing()
                return True
            elif keyCode == wx.WXK_TAB:
                self.SnippetEditingCycleVar()
                return True

        if (self.snippetCompleter.IsShown() and
            keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER,
                        wx.WXK_DOWN, wx.WXK_UP, wx.WXK_ESCAPE]):
            self.snippetCompleter.OnKeyDown(event)
        elif keyCode in [wx.WXK_DOWN, wx.WXK_UP] and event.GetModifiers() == wx.MOD_CONTROL | wx.MOD_SHIFT:
            offset = -1 if keyCode == wx.WXK_UP else 1
            startLine = self.LineFromPosition(self.GetSelectionStart())
            endLine = self.LineFromPosition(self.GetSelectionEnd())
            self.SetSelectionStart(self.PositionFromLine(startLine))
            self.SetSelectionEnd(self.GetLineEndPosition(endLine))
            text = self.GetSelectedTextUTF8()  + '\n'
            self.GotoLine(startLine)
            for i in range(endLine - startLine + 1):
                self.LineDelete()
            targetLine = startLine + offset
            self.GotoPos(self.PositionFromLine(targetLine))
            self.InsertTextUTF8(-1, text)
            self.SetSelectionStart(self.PositionFromLine(startLine + offset))
            self.SetSelectionEnd(self.GetLineEndPosition(endLine + offset))
        elif keyCode == ord('S') and event.GetModifiers() == wx.MOD_CONTROL:
            self.Save()
        elif keyCode == wx.WXK_SPACE and event.GetModifiers() == wx.MOD_CONTROL:
            self.OnAutoComplete()
        elif keyCode == wx.WXK_SPACE and event.GetModifiers() == wx.MOD_CONTROL | wx.MOD_SHIFT:
            self.OnSnippetRequest()
        elif keyCode == ord('G') and event.GetModifiers() == wx.MOD_CONTROL:
            self.ShowGoToLineDialog()
        elif (Config.GetProp("put_brackets_quotes_around", False) and self.SelectedText and
              (bracketQuote in self.brackets.keys() or bracketQuote in self.brackets.values())):
            if bracketQuote in self.brackets:
                open = bracketQuote
                close = self.brackets[bracketQuote]
            else:
                close = bracketQuote
                open = None
                for o, c in self.brackets.items():
                    if c == close:
                        open = o
                        break
                if not open: return
            self.ReplaceSelection(open + self.SelectedText + close)
        elif keyCode == wx.WXK_F3 and (event.GetModifiers() == wx.MOD_SHIFT or event.GetModifiers() == wx.MOD_NONE):
            searchDown = not event.ShiftDown()
            word = self.GetWordUnderCursor()
            if word:
                self.findPanel.FindText(word, searchDown = searchDown, select = False)
        else:
            return False
        return True

    def GetWordUnderCursor(self):
        start = self.WordStartPosition(self.CurrentPos, True)
        end = self.WordEndPosition(self.CurrentPos, True)
        if start == end:
            return None
        return self.GetTextRangeUTF8(start, end)

    def ShowGoToLineDialog(self):
        dlg = wx.TextEntryDialog(self, 'Line:', 'Goto Line', style = wx.OK | wx.CANCEL)
        dlg.SetValue("")
        if dlg.ShowModal() == wx.ID_OK:
            self.GotoLine(int(dlg.Value) - 1)
        dlg.Destroy()

    def Save(self):
        if not self.changed:
            return
        text = self.GetValue()
        self.savedText = text
        writeFile(self.filePath, text)
        #self.SaveFile(self.filePath)
        self.modifyTime = os.stat(self.filePath)[ST_MTIME]
        self.Changed(False)
        self.SetSavePoint()
        self.OnFileSaved()

    def OnAutoComplete(self):
        pass

    def OnUpdateSnippetCompleter(self, event):
        event.Skip()
        if not self.snippetCompleter.IsShown(): return
        self.UpdateSnippetCompleter()

    def UpdateSnippetCompleter(self, event = None):
        caretPos = self.GetCurrentPos()
        line = self.GetCurrentLine()
        prefix = self.GetTextRangeUTF8(self.PositionFromLine(line), caretPos)
        self.snippetCompleter.Update(prefix)
        self.snippetCompleter.UpdateCompleterPosition(self.PointFromPosition(caretPos))

    def OnSnippetRequest(self):
        self.UpdateSnippetCompleter()
        # if len(self.macrosCompleter.list.Items) == 1:
        #     self.macrosCompleter.AutoComplete(self.macrosCompleter.list.Items[0])
        # else:
        self.snippetCompleter.Show()

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
        self.InsertTextUTF8(self.CurrentPos, " " * indent)
        pos = self.PositionFromLine(self.CurrentLine)
        self.GotoPos(pos + indent)

    def UpdateTabTitle(self):
        core.TabMgr.PageModified(self.filePath, not self.saved)

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
            self.BraceBadLight(-1)
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

    def HighlightSelectedWord(self, text = None, start = 0):
        goToFoundText = False
        gotToFoundText = False
        if not text:
            text = self.GetSelectedText()
        else:
            goToFoundText = True
        if self.lastHighlightedWord != text:
            self.ClearIndicator(0)
            self.lastHighlightedWord = text
        else:
            return
        markers = []
        if (text and True not in [c in text for c in [" ", "\n", "\r", ","]]):
            self.SetIndicatorCurrent(0)
            self.SetSearchFlags(stc.STC_FIND_MATCHCASE | stc.STC_FIND_WHOLEWORD)
            self.SetTargetStart(start)
            self.SetTargetEnd(self.Length)
            index = self.SearchInTarget(text)

            while (index != -1 and index < self.Length):
                line = self.LineFromPosition(index)
                self.IndicatorFillRange(index, len(text))
                if goToFoundText and not gotToFoundText:
                    self.GotoPos(index)
                    self.SetAnchor(index + len(text))
                    gotToFoundText = True
                self.SetTargetStart(index + len(text))
                self.SetTargetEnd(self.Length)
                marker = Marker(line, self.GetLine(line), index, len(text))
                markers.append(marker)
                index = self.SearchInTarget(text)
        self.Refresh()
        if self.markerPanel:
            self.markerPanel.SetMarkers("selected_word", markers)

    def StartSnippetEditing(self, startPos, snippet):
        self.snippetVars = self.GetVars(snippet)
        if len(self.snippetVars) == 0: return
        self.snippetEditing = True
        self.SetMultipleSelection(True)
        self.SetAdditionalSelectionTyping(True)
        self.SetAdditionalCaretsVisible(True)

        self.currentSnippetVarIndex = 0
        self.snippetStartPos = startPos
        self.SelectSnippetVars()

    def SelectSnippetVars(self):
        self.currentSnippetVar = self.snippetVars[self.snippetVars.keys()[self.currentSnippetVarIndex]]
        needMain = True
        for (s, e) in self.currentSnippetVar:
            if needMain:
                self.SetSelection(self.snippetStartPos + s, self.snippetStartPos + e)
                self.SetSelectionNCaret(0, self.snippetStartPos + s)
                self.SetSelectionNAnchor(0, self.snippetStartPos + e)
                needMain = False
            else:
                self.AddSelection(self.snippetStartPos + s, self.snippetStartPos + e)

    def StopMacroEditing(self):
        self.snippetEditing = False
        self.SetMultipleSelection(False)
        self.SetAdditionalSelectionTyping(False)
        self.SetAdditionalCaretsVisible(False)
        self.snippetVars = None
        self.currentSnippetVar = None
        self.ClearSelections()
        self.SetCurrentPos(self.snippetStartPos)
        self.SetAnchor(self.snippetStartPos)

    def SnippetEditingCycleVar(self):
        selections = self.GetSelections()
        (os, oe) = self.currentSnippetVar[0]
        for i in range(selections):
            s = self.GetSelectionNCaret(i) - self.snippetStartPos
            e = self.GetSelectionNAnchor(i) - self.snippetStartPos
            self.currentSnippetVar[i] = (s, e)
        (s, e) = self.currentSnippetVar[0]
        self.UpdateOtherSnippetVarPositions(s, e - oe)

        self.ClearSelections()
        nextIndex = self.currentSnippetVarIndex + 1
        if nextIndex == len(self.snippetVars):
            nextIndex = 0
        self.currentSnippetVarIndex = nextIndex
        self.SelectSnippetVars()

    def UpdateOtherSnippetVarPositions(self, s, diff):
        for k in self.snippetVars.keys():
            if self.snippetVars.keys()[self.currentSnippetVarIndex] == k: continue
            snippetVarPositions = self.snippetVars[k]
            for i in range(len(snippetVarPositions)):
                (ps, pe) = snippetVarPositions[i]
                if s < ps:
                    ps += diff
                    pe += diff
                    diff += diff
                snippetVarPositions[i] = (ps, pe)

    def GetVars(self, snippet):
        snippetVars = collections.OrderedDict()
        pos = 0
        while True:
            m = self.snippetVarRegExp.search(snippet, pos)
            if not m: break
            pos = m.end()
            var = m.group(0)
            if var in snippetVars:
                snippetVars[var].append((m.start(), m.end()))
            else:
                snippetVars[var] = [(m.start(), m.end())]
        return snippetVars

class PythonSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_PYTHON)

class YAMLSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_YAML)

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("yaml")
        self.StyleSetSpec(stc.STC_YAML_DEFAULT, formats["default"])
        self.StyleSetSpec(stc.STC_YAML_COMMENT, formats["comment"])
        self.StyleSetSpec(stc.STC_YAML_IDENTIFIER, formats["identifier"])
        self.StyleSetSpec(stc.STC_YAML_NUMBER, formats["number"])
        self.StyleSetSpec(stc.STC_YAML_KEYWORD, formats["word"])
        self.StyleSetSpec(stc.STC_YAML_TEXT, formats["string"])

class HtmlSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_HTML)

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("html")
        self.StyleSetSpec(stc.STC_H_DEFAULT, formats["default"])
        self.StyleSetSpec(stc.STC_H_OTHER, formats["default"])
        self.StyleSetSpec(stc.STC_H_ATTRIBUTE, formats["attribute"])
        self.StyleSetSpec(stc.STC_H_ATTRIBUTEUNKNOWN, formats["attribute"])
        self.StyleSetSpec(stc.STC_H_SINGLESTRING, formats["string"])
        self.StyleSetSpec(stc.STC_H_DOUBLESTRING, formats["string"])
        self.StyleSetSpec(stc.STC_H_TAG, formats["tag"])
        self.StyleSetSpec(stc.STC_H_TAGEND, formats["tag"])
        self.StyleSetSpec(stc.STC_H_NUMBER, formats["number"])
        self.StyleSetSpec(stc.STC_H_COMMENT, formats["comment"])

        formats = ColorSchema.LanguageFormats("css")
        self.StyleSetSpec(stc.STC_CSS_OPERATOR, formats["operator"])
        self.StyleSetSpec(stc.STC_CSS_DOUBLESTRING, formats["string"])
        self.StyleSetSpec(stc.STC_CSS_SINGLESTRING, formats["string"])
        self.StyleSetSpec(stc.STC_CSS_CLASS, formats["class"])
        self.StyleSetSpec(stc.STC_CSS_ID, formats["id"])
        self.StyleSetSpec(stc.STC_CSS_VALUE, formats["value"])

class CppSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_CPP)

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("cpp")
        self.StyleSetSpec(stc.STC_C_DEFAULT, formats["default"])
        self.StyleSetSpec(stc.STC_C_COMMENTLINE, formats["comment"])
        self.StyleSetSpec(stc.STC_C_NUMBER, formats["number"])
        self.StyleSetSpec(stc.STC_C_STRING, formats["string"])
        self.StyleSetSpec(stc.STC_C_CHARACTER, formats["char"])
        self.StyleSetSpec(stc.STC_C_WORD, formats["word"])
        self.StyleSetSpec(stc.STC_C_WORD2, formats["word"])
        self.StyleSetSpec(stc.STC_C_IDENTIFIER, formats["identifier"])
        self.StyleSetSpec(stc.STC_C_PREPROCESSOR, formats["preproc"])

        keywords = [
            'asm', 'delete', 'goto', 'return', 'typedef', 'auto', 'do', 'if', 'short',
            'typeid', 'bad_cast', 'double', 'inline', 'signed', 'typename',
            'bad_typeid', 'dynamic_cast', 'int', 'sizeof', 'union', 'bool',
            'else', 'long', 'static', 'unsigned', 'break', 'enum', 'mutable',
            'static_cast', 'using', 'case', 'except', 'namespace', 'struct', 'virtual',
            'catch', 'explicit', 'new', 'switch', 'void', 'char', 'extern', 'operator',
            'template', 'volatile', 'class', 'false', 'private', 'this', 'while',
            'const', 'finally', 'protected', 'throw', 'const_cast', 'float', 'public',
            'true', 'continue', 'for', 'register', 'try', 'default', 'friend',
            'reinterpret_cast', 'type_info']

        self.SetKeyWords(0, ' '.join(keywords))
        self.SetKeyWords(1, ' '.join(keywords))
        self.SetKeyWords(2, ' '.join(keywords))

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

    def Clear(self):
        self.SetReadOnly(False)
        self.ClearAll()
        self.SetReadOnly(True)

    def Save(self):
        pass

    def Changed(self, changed = True):
        pass

    def Append(self, text):
        text = text.rstrip()
        text += "\n"
        self._AppendText(text)
        if self.TextLength > 20000:
            self.Text = self.Text[-20000:]

    def _AppendText(self, text):
        try:
            linesCount = self.GetLineCount()
            self.SetReadOnly(False)
            self.AppendText(text)
            self.SetReadOnly(True)
            if self.GetLastVisibleLine() >= linesCount:
                self.ScrollToLine(self.GetLineCount())
        except Exception, e:
            core.Log("append text error", e)

class STCContextToolTip:
    def __init__(self, stc, handler):
        self.stc = stc
        self.handler = handler
        self.tooltipWin = None
        self.showPos = (0, 0)
        self.lastPos = (0, 0)
        self.tooltipWin = STCTooltip(self.stc)
        self.tooltipWin.Hide()
        self.showtime = None
        self.counter = 0

        self.stc.Bind(wx.EVT_ENTER_WINDOW, self.OnEnter)
        self.stc.Bind(wx.EVT_LEAVE_WINDOW, self.OnLeave)
        self.stc.Bind(wx.EVT_MOTION, self.OnMotion)
        self.stc.Bind(wx.EVT_WINDOW_DESTROY, self.OnDestroy)
        self.stc.Bind(wx.EVT_KEY_DOWN, self.OnKey)
        self.stc.Bind(wx.EVT_KEY_UP, self.OnKey)
        self.stc.Bind(wx.EVT_CHAR, self.OnKey)
        wx.GetApp().Bind(wx.EVT_ACTIVATE_APP, self.OnAppLostFocus)

    def OnKey(self, event):
        self.HideToolTip()
        self.counter = 0
        event.Skip()

    def OnAppLostFocus(self, event):
        try:
            self.HideToolTip()
        except:
            pass
        event.Skip()

    def OnEnter(self, event):
        if not self.showtime:
            self.showtime = wx.PyTimer(self.ShowTimer)
        if not self.showtime.IsRunning():
            self.showtime.Start(50)
        event.Skip()

    def OnLeave(self, event):
        if self.tooltipWin:
            pos = wx.GetMousePosition()
            realPos = self.tooltipWin.ScreenToClient(pos)
            rect = self.tooltipWin.GetClientRect()

            if rect.Contains(realPos):
                event.Skip()
                return

        self.tooltipWin.Hide()
        self.showtime.Stop()
        event.Skip()

    def OnDestroy(self, event):
        if self.showtime:
            self.showtime.Stop()
        if self.tooltipWin:
            self.tooltipWin.Destroy()
        event.Skip()

    def OnMotion(self, event):
        event.Skip()
        if not self.tooltipWin.IsShown():
            return
        currentPos = wx.GetMousePosition()
        if self.tooltipWin.Shown:

            pos = wx.GetMousePosition()
            realPos = self.tooltipWin.ScreenToClient(pos)
            rect = self.tooltipWin.GetClientRect()
            rect.Top -=10
            rect.Bottom += 10
            rect.Left -=10
            rect.Right += 10
            if rect.Contains(realPos):
                return
            if (abs(self.showPos[0] - currentPos[0]) < 15 and
                abs(self.showPos[1] - currentPos[1]) < 15):
                return
            self.HideToolTip()


    def ShowTimer(self):
        if self.tooltipWin.isFadingOut: return
        if self.tooltipWin.isFadingIn: return
        if not self.stc.HasFocus(): return
        if (core.TabMgr.GetActiveEditor() != self.stc or
            not wx.GetApp().IsActive()):
            if self.tooltipWin.Shown:
                self.HideToolTip()
            return
        if self.tooltipWin.Shown: return
        current = wx.GetMousePosition()
        if current != self.lastPos:
            self.counter = 0
            self.lastPos = current
        else:
            self.counter += 50
            if self.counter == Config.TooltipDelay():
                text = self.handler()
                if text:
                    self.showPos = wx.GetMousePosition()
                    self.tooltipWin.SetPosition((self.showPos[0] - 3, self.showPos[1] + 10))
                    self.tooltipWin.SetText(text)
                    self.tooltipWin.FadeIn()
                    self.stc.SetFocus()
                else:
                    self.HideToolTip()


    def HideToolTip(self):
        self.tooltipWin.FadeOut()

class STCTooltip(wx.Frame):
    def __init__(self, parent):
        wx.Frame.__init__(self, parent, style = wx.BORDER_NONE | wx.STAY_ON_TOP | wx.FRAME_NO_TASKBAR)

        self.SetSize((550, 200))

        self.helpWindow = HtmlWin(self)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.sizer.Add(self.helpWindow, 1, wx.EXPAND, 10)
        self.SetSizer(self.sizer)
        self.Layout()

        self.transp = 255

        self.isFadingOut = False
        self.isFadingIn = False

    def FadeOut(self):
        def handler():
            self.transp -= 25
            self.transp = max(self.transp, 0)
            self.SetTransparent(self.transp)
            if self.transp == 0:
                self.fadeTimer.Stop()
                self.Hide()
                self.isFadingOut = False
            self.Parent.SetFocus()
        self.fadeTimer = wx.PyTimer(handler)
        self.fadeTimer.Start(5)
        self.isFadingOut = True
        self.isFadingIn = False

    def FadeIn(self):
        def handler():
            self.Show()
            self.Parent.SetFocus()
            self.transp += 25
            self.transp = min(self.transp, 255)
            self.SetTransparent(self.transp)
            if self.transp == 255:
                self.fadeTimer.Stop()
                self.isFadingIn = False
        self.fadeTimer = wx.PyTimer(handler)
        self.fadeTimer.Start(5)
        self.isFadingIn = True
        self.isFadingOut = False

    def SetText(self, text):
        self.helpWindow.SetPage(text)
        cell = self.helpWindow.GetInternalRepresentation()
        height = 300
        if cell:
            cell.SetWidthFloat(540, wx.html.HTML_UNITS_PIXELS)
            height = cell.GetHeight()
            height += self.helpWindow.GetCharHeight() / 2
            height = min(height, 400)
        self.SetSize((550, height))
