from idn_cache import ErlangCache, Function, Record, Macros, readFile
from idn_token import ErlangTokenizer, ErlangTokenType

__author__ = 'Yaroslav Nikityshev aka IDNoise'


import os
import wx
from wx import stc
from wx.stc import STC_FOLDLEVELHEADERFLAG, StyledTextCtrl
from idn_colorschema import ColorSchema
from idn_highlight import ErlangHighlightType
from idn_lexer import ErlangLexer
from wx import html

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
    def __init__(self, parent, filePath = None):
        #style = wx.MAXIMIZE_BOX|wx.RESIZE_BORDER|wx.SYSTEM_MENU|wx.CAPTION|wx.CLOSE_BOX
        StyledTextCtrl.__init__(self, parent)#, style = style)
        EditorFoldMixin.__init__(self)
        EditorLineMarginMixin.__init__(self)

        self.SetupLexer()
        self.filePath = filePath
        self.hash = None

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

        if hasattr(self, "lexer"):
            self.Bind(stc.EVT_STC_STYLENEEDED, self.OnStyleNeeded)
        self.Bind(stc.EVT_STC_UPDATEUI, self.HighlightBrackets)
        self.Bind(stc.EVT_STC_CHANGE , self.OnDocumentChanged)
        self.Bind(stc.EVT_STC_CHARADDED, self.OnCharAdded)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)

        self.EnableLineNumbers()

        self.OnInit()

        if self.filePath:
            self.LoadFile(self.filePath)
            self.SetSelection(0, 0)


        #print(self.GetScrollWidth())
        #print(self.Size)

    def OnInit(self):
        pass

    def SetupLexer(self):
        self.lexer = None

    def SetupLanguageStyles(self):
        pass

    def OnDocumentChanged(self, event):
        self.Changed()
        event.Skip()

    def OnCharAdded(self, event):
        keyCode = event.GetKey()
        if keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER]:
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
            self.hash = hash(self.GetText())
            self.SaveFile(self.filePath)
            self.Changed(False)
        elif keyCode == wx.WXK_SPACE and event.ControlDown():
            self.OnAutoComplete()
        else:
            return False
        return True

    def OnAutoComplete(self):
        pass

    def Changed(self, changed = True):
        self.changed = changed
        if self.changed:
            self.changed = self.hash != hash(self.GetText())
        prefix = "* " if self.changed else ""
        index = self.Parent.FindPageIndexByPath(self.filePath)
        if index is not None:
            self.Parent.SetPageText(index, prefix + self.FileName())

    def LoadFile(self, path):
        self.filePath = path
        StyledTextCtrl.LoadFile(self, path)
        self.hash = hash(self.GetText())
        self.Changed(False)

    def DoIndent(self):
        prevIndent = self.GetLineIndentation(self.CurrentLine - 1)
        self.InsertText(self.CurrentPos, " " * prevIndent)
        self.GotoPos(self.GetLineEndPosition(self.CurrentLine))

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
        char = chr(self.GetCharAt(pos))
        if not char in "()[]{}<>":
            self.BraceBadLight(-1) #clear
            return
        otherPos = self.BraceMatch(pos)
        if otherPos > 0:
            self.BraceHighlight(pos, otherPos)
        else:
            self.BraceBadLight(pos)

class PythonSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_PYTHON)

class YAMLSTC(CustomSTC):
    def SetupLexer(self):
        self.SetLexer(stc.STC_LEX_YAML)

class ErlangSTC(CustomSTC):
    TYPE_MODULE, TYPE_HRL, TYPE_UNKNOWN = range(3)

    def OnInit(self):
        self.completer = ErlangCompleter(self)
        self.Bind(stc.EVT_STC_UPDATEUI, self.OnUpdateCompleter)

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
        windowPosition = self.PointFromPosition(caretPos)
        self.completer.UpdateCompleterPosition(windowPosition)

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
        else:
            return False

class ErlangCompleter(wx.Window):
    SIZE = (800, 400)
    LIST_SIZE = (300, 100)

    def __init__(self, stc):
        wx.Window.__init__(self, stc, size = self.SIZE)
        self.tokenizer = ErlangTokenizer()
        #self.SetBackgroundColour(wx.Colour(255, 255, 255, alpha = wx.ALPHA_TRANSPARENT))
        self.SetBackgroundStyle(wx.BG_STYLE_TRANSPARENT)

        self.stc = stc
        self.lineHeight = stc.TextHeight(0) + 2
        self.module = self.stc.ModuleName()
        self.moduleType = self.stc.ModuleType()


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
        self.Bind(wx.EVT_PAINT, self.OnPaint)

        self.separators = ",;([{<-"
        self.lastText = None

    def OnPaint(self, event):
        dc = wx.PaintDC(self)
        dc.SetBackgroundMode(wx.TRANSPARENT)
        event.Skip()

    def UpdateCompleterPosition(self, pos):
        shown = self.IsShown()
        self.Hide()
        pos = (pos[0], pos[1] + self.lineHeight)
        self.SetPosition(pos)
        self.stc.Update()
        if shown: self.Show()

    def UpdateRecordField(self, record, prefix):
        #print "update rec field"
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

                if not d.docref:
                    p = d.params[:]
                    if d.types and not d.docref:
                        for i in range(len(p)):
                            p[i] = p[i] + " :: " + d.types[i]
                    help = "{}({}) -> {}".format(d.name, ", ".join(p), d.result)
                else:
                    help = ("docref", d.docref)
            elif isinstance(d, Record):
                text = d.name
                help = "{} [{}]\nModule:{}".format(d.name, ", ".join(d.fields), d.module)
            elif isinstance(d, Macros):
                text = d.name
                help = "{} -> {}\nModule:{}".format(d.name, d.value, d.module)
            else:
                text = d
            if text.startswith(self.prefix):
                self.list.Append(text, help)
        self.ValidateCompleter()

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
            self.sizer.Hide(self.helpWindow)
        else:
            if isinstance(help, tuple):
                path = os.path.join(ErlangCache.ERLANG_LIBS_CACHE_DIR, help[1])
                text = readFile(path)
            else:
                text = help
            self.helpWindow.SetPage(text)
            self.sizer.Show(self.helpWindow)
        self.Layout()

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
        #print "AutoComplete: ", text
        toInsert = text[len(self.prefix):]
        self.stc.AddText(toInsert)
        self.HideCompleter()

    def HideCompleter(self):
        wx.Window.Hide(self)
        self.helpWindow.SetPage("")

    def Show(self, show = True):
        if not self.helpWindow.ToText():
            self.sizer.Hide(self.helpWindow)
        else:
            self.sizer.Show(self.helpWindow)
        self.Layout()
        if len(self.list.GetStrings()) > 0:
            wx.Window.Show(self, show)

class ConsoleSTC(CustomSTC):
    def __init__(self, parent):
        CustomSTC.__init__(self, parent)
        self.EnableLineNumbers(False)
        self.SetCaretWidth(1)
        self.SetCaretLineBackground(ColorSchema.codeEditor["current_line_background"])
        self.SetCaretLineVisible(True)

        self.SetScrollWidth(500)
        self.SetReadOnly(True)

        self.SetMarginWidth(2, 0)

        self.SetEdgeMode(stc.STC_EDGE_NONE)

        self.SetEndAtLastLine(True)

        self.SetYCaretPolicy(stc.STC_CARET_SLOP, 10)

    def Changed(self, changed = True):
        pass