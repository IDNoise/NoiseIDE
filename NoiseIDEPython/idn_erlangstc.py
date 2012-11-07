from idn_config import Config

__author__ = 'Yaroslav'


import os
import wx
from wx import stc
from idn_cache import ErlangCache
from idn_colorschema import ColorSchema
from idn_connect import CompileErrorInfo
from idn_customstc import CustomSTC
from idn_erlang_completer import ErlangCompleter
from idn_erlang_constats import TYPE_MODULE, TYPE_UNKNOWN, TYPE_HRL
from idn_erlang_lexer import ErlangLexer
from idn_global import GetProject, GetMainFrame, GetToolMgr, GetTabMgr, Log
from idn_highlight import ErlangHighlightType
from idn_marker_panel import Marker
from idn_outline import ErlangOutline
from idn_utils import Menu
import idn_projectexplorer as exp


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
        self.StyleSetSpec(ErlangHighlightType.FUNCTION, formats["function"])
        self.StyleSetSpec(ErlangHighlightType.KEYWORD, formats["keyword"])
        self.StyleSetSpec(ErlangHighlightType.MODULEATTR, formats["moduleattr"])
        self.StyleSetSpec(ErlangHighlightType.RECORD, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.RECORDDEF, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.NUMBER, formats["number"])
        self.StyleSetSpec(ErlangHighlightType.FUNDEC, formats["fundec"])
        self.StyleSetSpec(ErlangHighlightType.BRACKET, formats["bracket"])
        self.StyleSetSpec(ErlangHighlightType.BIF, formats["bif"])
        self.StyleSetSpec(ErlangHighlightType.FULLSTOP, formats["fullstop"])


class ErlangSTC(ErlangHighlightedSTCBase):
    MARKER_ERROR_CIRCLE, MARKER_WARNING_CIRCLE, MARKER_ERROR, MARKER_WARNING = (18, 19, 20, 21)

    def OnInit(self):
        self.completer = ErlangCompleter(self)

        self.overlay = wx.Overlay()

        self.navigateTo = None

        if self.ModuleType() == TYPE_MODULE:
            self.flyTimer = wx.Timer(self, wx.NewId())
            self.Bind(wx.EVT_TIMER, self.OnFlyTimer, self.flyTimer)
            self.flyTimer.Start(500)
            self.flyCompileHash = None

        self.lastErrors = []
        self.errorsLines = []
        self.markerPanel.SetMarkerColor("warning", ColorSchema.codeEditor["warning_marker_color"])
        self.markerPanel.SetMarkerColor("error", ColorSchema.codeEditor["error_marker_color"])

        self.HighlightErrors(GetProject().GetErrors(self.filePath))

        self.Bind(stc.EVT_STC_UPDATEUI, self.OnUpdateCompleter)
        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)
        self.Bind(wx.EVT_MIDDLE_DOWN, self.OnMiddleMouseClick)

    def SetupEditorMenu(self):
        ErlangHighlightedSTCBase.SetupEditorMenu(self)
        self.editorMenu.AppendSeparator()
        self.editorMenu.AppendMenuItem('Outline', GetMainFrame(), lambda e: self.ShowOutline(), "Ctrl-H")
        self.editorMenu.AppendMenuItem('Comment lines', GetMainFrame(), lambda e: self.CommentLines(), "Ctrl-/")
        if self.ModuleType() == TYPE_MODULE:
            self.editorMenu.AppendMenuItem('Add to export', GetMainFrame(), lambda e: self.AddToExport(), "Ctrl-E")

    def SetupLanguageStyles(self):
        ErlangHighlightedSTCBase.SetupLanguageStyles(self)

        self.IndicatorSetStyle(1, stc.STC_INDIC_PLAIN)
        self.MarkerDefine(self.MARKER_ERROR, stc.STC_MARK_BACKGROUND,
            foreground = ColorSchema.codeEditor["error_line_color"],
            background = ColorSchema.codeEditor["error_line_color"])
        self.MarkerDefine(self.MARKER_WARNING, stc.STC_MARK_BACKGROUND,
            foreground = ColorSchema.codeEditor["warning_line_color"],
            background = ColorSchema.codeEditor["warning_line_color"])

        self.MarkerDefine(self.MARKER_ERROR_CIRCLE, stc.STC_MARK_CIRCLE,
            foreground = ColorSchema.codeEditor["error_marker_color"],
            background = ColorSchema.codeEditor["error_marker_color"])

        self.MarkerDefine(self.MARKER_WARNING_CIRCLE, stc.STC_MARK_CIRCLE,
            foreground = ColorSchema.codeEditor["warning_marker_color"],
            background = ColorSchema.codeEditor["warning_marker_color"])

        self.SetMarginMask(2, ~stc.STC_MASK_FOLDERS)#self.MARKER_ERROR_CIRCLE | self.MARKER_WARNING_CIRCLE)

    def CreatePopupMenu(self, event):
        menu = Menu()
        if self.ModuleType() == TYPE_MODULE:
            compileOptionMenu = Menu()
            menu.AppendMenu(wx.NewId(), "Compile Option", compileOptionMenu)
            compileOptionMenu.AppendCheckMenuItem("With 'P' flag", self, lambda e: GetProject().CompileOption(self.filePath, "P"))
            compileOptionMenu.AppendCheckMenuItem("With 'E' flag", self, lambda e: GetProject().CompileOption(self.filePath, "E"))
            compileOptionMenu.AppendCheckMenuItem("With 'S' flag", self, lambda e: GetProject().CompileOption(self.filePath, "S"))

        self.PopupMenu(menu)

    def ModuleName(self):
        name = self.FileName()
        if name.endswith(".erl"):
            return name[:-4]
        else:
            return name

    def ModuleType(self):
        name = self.ModuleName()
        if name.endswith('.hrl'): return TYPE_HRL
        elif name.find('.') < 0: return TYPE_MODULE
        return TYPE_UNKNOWN

    def OnAutoComplete(self):
        self.UpdateCompleter()
        #print len(self.completer.list.Items) == 1
        #print isinstance(self.completer.lastData[0], unicode)
        #print self.completer.lastData[0], type(self.completer.lastData[0])
        if len(self.completer.list.Items) == 1 and isinstance(self.completer.lastData[0], unicode):
            self.completer.AutoComplete(self.completer.list.Items[0])
        else:
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

        if keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER] and event.GetModifiers() == wx.MOD_ALT:
            activeToolPage = GetToolMgr().GetSelection()
            activeTool = GetToolMgr()[activeToolPage]
            consoles = GetProject().consoles
            if activeTool not in consoles.values():
                activeTool = consoles.values()[0]
            GetToolMgr().SetSelection(GetToolMgr().FindPageIndexByWindow(activeTool))
            activeTool.commandText.SetFocus()
            return True
        elif event.GetModifiers() == wx.MOD_CONTROL and keyCode == wx.WXK_UP:
            self.GoToExport()
            return True
        elif event.GetModifiers() == wx.MOD_CONTROL and keyCode == wx.WXK_DOWN:
            self.GoToFun()
            return True
        else:
            return False

    def ShowOutline(self):
        dlg = ErlangOutline(self, self.ModuleName())
        dlg.ShowModal()

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

#        def showErrorTooltip():
#            line = self.LineFromPosition(pos)
#            errs = list(filter(lambda e: e.line == line, self.lastErrors))
#            if errs:
#                msg = reduce(lambda msg, e: msg + e.msg + "\n", errs, "")
#                self.ShowToolTip(msg)
#            else:
#                self.HideToolTip()

        if event.GetPosition()[0] < self.LineNumbersWidth() + self.FoldWidth + 10:
            self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
#            self.HideToolTip()
#            if (event.GetPosition()[0] > self.LineNumbersWidth() and
#                event.GetPosition()[0] < self.LineNumbersWidth() + 10):
#                showErrorTooltip()
            return

        #print event.GetModifiers() == wx.MOD_CONTROL,  self.HasFocus()
        if event.GetModifiers() == wx.MOD_CONTROL and self.HasFocus():
            if not self.CheckNavigation(pos):
                self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
        elif self.HasFocus():
            self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
#            showErrorTooltip()

    def OnRequestTooltipText(self):
        if wx.GetKeyState(wx.WXK_CONTROL):
            return None
        data = self.GetContextData()
        if not data:
            pos = self.PositionFromPoint(self.ScreenToClient(wx.GetMousePosition()))
            line = self.LineFromPosition(pos)
            errs = list(filter(lambda e: e.line == line, self.lastErrors))
            if errs:
                data = reduce(lambda msg, e: msg + e.msg + "\n", errs, "")
        return data


    def GetContextData(self):
        pos = self.PositionFromPoint(self.ScreenToClient(wx.GetMousePosition()))
        style = self.GetStyleAt(pos)
        if style not in [ErlangHighlightType.FUNCTION,
                         ErlangHighlightType.MACROS,
                         ErlangHighlightType.RECORD]:
            return None
        start = self.WordStartPosition(pos, True)
        end = self.WordEndPosition(pos, True)
        if start == end:
            return None

        line = self.LineFromPosition(pos)
        lineStart = self.PositionFromLine(line)
        prefix = self.GetTextRange(lineStart, start)
        value = self.GetTextRange(start, end)
        data = None
        if style == ErlangHighlightType.FUNCTION:
            data = self.completer.GetFunctionNavAndHelp(value, prefix, end)
        elif style == ErlangHighlightType.RECORD:
            data = self.completer.GetRecordNavAndHelp(value)
        elif style == ErlangHighlightType.MACROS:
            data = self.completer.GetMacrosNavAndHelp(value)

        if data:
            return data[1]

        return None

    def CheckNavigation(self, pos):
        style = self.GetStyleAt(pos)
        if style not in [ErlangHighlightType.ATOM,
                         ErlangHighlightType.FUNCTION,
                         #ErlangHighlightType.FUNDEC,
                         ErlangHighlightType.MACROS,
                         ErlangHighlightType.MODULE,
                         ErlangHighlightType.RECORD,
                         ErlangHighlightType.MODULEATTR,
                         ErlangHighlightType.STRING]:
            #print "wrong style", style
            return False
        start = self.WordStartPosition(pos, True)
        end = self.WordEndPosition(pos, True)
        if start == end:
            #print "same pos"
            return False


        line = self.LineFromPosition(pos)
        lineText = self.GetLine(line).strip()
        lineStart = self.PositionFromLine(line)
        lineEnd = self.GetLineEndPosition(line)
        prefix = self.GetTextRange(lineStart, start)
        value = self.GetTextRange(start, end)
        postfix = self.GetTextRange(end, lineEnd)
        #print value, prefix
        data = None
        if style == ErlangHighlightType.FUNCTION:
            data = self.completer.GetFunctionNavAndHelp(value, prefix, end)
        elif style == ErlangHighlightType.RECORD:
            data = self.completer.GetRecordNavAndHelp(value)
        elif style == ErlangHighlightType.MACROS:
            data = self.completer.GetMacrosNavAndHelp(value)
        elif style in [ErlangHighlightType.ATOM, ErlangHighlightType.MODULE]:
            if value in ErlangCache.AllModules():
                self.navigateTo = (ErlangCache.moduleData[value].file, 0)

        elif style == ErlangHighlightType.MODULEATTR or ErlangHighlightType.STRING:
            path = None
            if (style == ErlangHighlightType.MODULEATTR and value in ["include", "include_lib"]):
                path = postfix[2:len(postfix)-3]
            elif style == ErlangHighlightType.STRING and lineText.startswith("-include"):
                s = 14 if lineText.startswith("-include_lib") else 10
                path = lineText[s:len(lineText)-3]
            if path:
                include = os.path.basename(path)
                if include in ErlangCache.moduleData:
                    self.navigateTo = (ErlangCache.moduleData[include].file, 0)
                    start = lineStart
                    end = lineEnd
        #print self.navigateTo
        if style in [ErlangHighlightType.FUNCTION, ErlangHighlightType.RECORD, ErlangHighlightType.MACROS]:
            if data:
                self.navigateTo = data[0]
            else:
                return False
        if self.navigateTo:
            line = self.LineFromPosition(pos)
            self.navigateTo += (line, )
            if self.navigateTo[0]:
                self.SetCursor(wx.StockCursor(wx.CURSOR_HAND))
                self.SetIndicatorCurrent(1)
                self.IndicatorFillRange(start, end - start)
            return True
        return False

    def OnMouseClick(self, event):
        if event.GetModifiers() == wx.MOD_CONTROL:
            if self.navigateTo:
                #editor =
                GetTabMgr().LoadFileLine(self.navigateTo[0], self.navigateTo[1] - 1, True, self.navigateTo[2])
                return
        event.Skip()

    def OnMiddleMouseClick(self, event):
#        if event.GetModifiers() == wx.MOD_CONTROL:
#            if self.navigateTo:
#                #self.completer.helpWindow.SetFocus()
#                return
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
        self.MarkerDeleteAll(self.MARKER_WARNING)
        self.MarkerDeleteAll(self.MARKER_ERROR)
        self.MarkerDeleteAll(self.MARKER_WARNING_CIRCLE)
        self.MarkerDeleteAll(self.MARKER_ERROR_CIRCLE)
        self.lastErrors = errors
        self.errorsLines = map(lambda x: x.line, errors)
        wMarkers = []
        eMarkers = []
        errors = sorted(errors, key = lambda e: e.type)
        highlightLine = Config.GetProp("highlight_error_background", False)
        for e in errors:
            if e.type == CompileErrorInfo.WARNING:
                wMarkers.append(Marker(e.line, e.msg))
                indic_line = self.MARKER_WARNING
                indic_margin = self.MARKER_WARNING_CIRCLE
            else:
                eMarkers.append(Marker(e.line, e.msg))
                indic_line = self.MARKER_ERROR
                indic_margin = self.MARKER_ERROR_CIRCLE
                self.MarkerDelete(e.line, self.MARKER_WARNING)
                self.MarkerDelete(e.line, self.MARKER_WARNING_CIRCLE)
            #print highlightLine, e.line, indic_line, indic_margin
            if highlightLine:
                self.MarkerAdd(e.line, indic_line)
            self.MarkerAdd(e.line, indic_margin)
        self.markerPanel.SetMarkers("warning", wMarkers)
        self.markerPanel.SetMarkers("error", eMarkers)
        self.Refresh()

    def OnFileSaved(self):
        #Log("saved stc", self.filePath)
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
        funData = self.lexer.GetCurrentFunction()
        if not funData: return

        fun = funData[0]
        arity = self.completer.GetFunArity(funData[1] + len(fun))

        funStr = "{}/{}".format(fun, arity)
        (exports, startPos, insertPos) = self.lexer.GetAllExports()
        if funStr in exports:
            return
        if exports:
            funStr = ",\n    {}/{}".format(fun, arity)

        if insertPos:
            self.InsertText(insertPos, funStr)

    def GoToExport(self):
        funData = self.lexer.GetCurrentFunction()
        #print funData
        if not funData: return

        fun = funData[0]
        arity = self.completer.GetFunArity(funData[1] + len(fun))
        funStr = "{}/{}".format(fun, arity)
        (exports, startPos, insertPos) = self.lexer.GetAllExports()
        #print funStr
        if funStr not in exports: return
        self.SetTargetStart(startPos)
        self.SetTargetEnd(insertPos)
        pos = self.SearchInTarget(funStr)
        if pos >= 0:
            self.SetAnchor(pos)
            self.GotoPos(pos + len(funStr))

    def GoToFun(self):
        pos = self.CurrentPos
        style = self.GetStyleAt(pos)
        if style != ErlangHighlightType.FUNCTION: return

        start = self.WordStartPosition(pos, True)
        end = self.WordEndPosition(pos, True)
        if start == end: return

        line = self.LineFromPosition(pos)
        lineStart = self.PositionFromLine(line)
        prefix = self.GetTextRange(lineStart, start)
        value = self.GetTextRange(start, end)
        navigateTo = self.completer.GetFunctionNavAndHelp(value, prefix, end)[0]
        self.GotoLine(navigateTo[1] - 1)

class ErlangSTCReadOnly(ErlangSTC):
    def __init__(self, parent, panel, filePath, option, text):
        ErlangSTC.__init__(self, parent, panel)

        self.AppendText(text)
        self.SetReadOnly(True)
        self.SetToolTip(None)
        self.filePath = filePath
        self.option = option

        GetProject().explorer.ProjectFilesModifiedEvent += self.OnProjectFilesModified

        self.completer = ErlangCompleter(self)

        self.overlay = wx.Overlay()
        self.lastErrors = []
        self.errorsLines = []
        self.navigateTo = None

        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)
        self.Bind(wx.EVT_MIDDLE_DOWN, self.OnMiddleMouseClick)

    def OnInit(self):
        pass

    def SetupEditorMenu(self):
        ErlangHighlightedSTCBase.SetupEditorMenu(self)

    def CreatePopupMenu(self, event):
        pass

    def Changed(self, changed = True):
        pass

    def Save(self):
        pass

    def OnProjectFilesModified(self, files):
        for file in files:
            if file == self.filePath:
                GetProject().CompileOption(self.filePath, self.option)

    def SetNewText(self, text):
        self.SetReadOnly(False)
        self.ClearAll()
        self.AppendText(text)
        self.SetReadOnly(True)

    def HandleKeyDownEvent(self, event):
        result = ErlangSTC.HandleKeyDownEvent(self, event)
        if result:
            return result
        keyCode = event.GetKeyCode()

        if keyCode == ord('R') and event.GetModifiers() == wx.MOD_CONTROL:
            GetProject().CompileOption(self.filePath, self.option)
            return True
        else:
            return False