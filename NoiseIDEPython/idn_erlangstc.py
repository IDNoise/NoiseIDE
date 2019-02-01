import re
from idn_findreplace import FindInProjectDialog, Find
from idn_token import ErlangTokenType

__author__ = 'Yaroslav'

import os
import wx
from wx import stc
from idn_cache import ErlangCache, IgorCache
from idn_colorschema import ColorSchema
from idn_connect import CompileErrorInfo
from idn_customstc import CustomSTC, ConsoleSTC
from idn_erlang_completer import ErlangCompleter
from idn_erlang_constats import TYPE_MODULE, TYPE_UNKNOWN, TYPE_HRL
from idn_erlang_lexer import ErlangLexer, IgorLexer
import core
from idn_highlight import ErlangHighlightType, IgorHighlightType
from idn_marker_panel import Marker
from idn_outline import ErlangOutline
from idn_utils import Menu, camelToLowerUnderscore, readFile, underscoreToCamelcase
from idn_config import Config
from idn_erlang_utils import IsModule
from idn_findreplace import ReplaceInProject, ReplaceInFile


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
        self.StyleSetSpec(ErlangHighlightType.SPECIAL, formats["special"])
        self.StyleSetSpec(ErlangHighlightType.MODULEATTR, formats["moduleattr"])
        self.StyleSetSpec(ErlangHighlightType.RECORD, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.RECORDDEF, formats["record"])
        self.StyleSetSpec(ErlangHighlightType.NUMBER, formats["number"])
        self.StyleSetSpec(ErlangHighlightType.FUNDEC, formats["fundec"])
        self.StyleSetSpec(ErlangHighlightType.BRACKET, formats["bracket"])
        self.StyleSetSpec(ErlangHighlightType.BIF, formats["bif"])
        self.StyleSetSpec(ErlangHighlightType.FULLSTOP, formats["fullstop"])

    def OnFileSaved(self):
        core.Project.FileSaved(self.filePath)


class ErlangSTC(ErlangHighlightedSTCBase):
    MARKER_ERROR_CIRCLE, MARKER_WARNING_CIRCLE, MARKER_ERROR, MARKER_WARNING = (18, 19, 20, 21)

    def OnInit(self):
        ErlangHighlightedSTCBase.OnInit(self)
        self.completer = ErlangCompleter(self)

        self.overlay = wx.Overlay()

        self.navigateTo = None
        self.flyTimer = None
        if self.ModuleType() == TYPE_MODULE:
            self.flyTimer = wx.Timer(self, wx.ID_ANY)
            self.Bind(wx.EVT_TIMER, self.OnFlyTimer, self.flyTimer)
            self.flyTimer.Start(500)
            self.flyCompileHash = None

        self.lastErrors = []
        self.errorsLines = []
        self.markerPanel.SetMarkerColor("warning", ColorSchema.codeEditor["warning_marker_color"])
        self.markerPanel.SetMarkerColor("error", ColorSchema.codeEditor["error_marker_color"])

        self.HighlightErrors(core.Project.GetErrors(self.filePath))

        self.Bind(stc.EVT_STC_UPDATEUI, self.OnUpdateCompleter)
        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)
        self.Bind(wx.EVT_MIDDLE_DOWN, self.OnMiddleMouseClick)

    def SetupEditorMenu(self):
        ErlangHighlightedSTCBase.SetupEditorMenu(self)
        self.editorMenu.AppendSeparator()
        self.editorMenu.AppendMenuItem('Outline', core.MainFrame, lambda e: self.ShowOutline(), "Ctrl-H")
        if self.ModuleType() == TYPE_MODULE:
            self.editorMenu.AppendMenuItem('Add to export', core.MainFrame, lambda e: self.AddToExport(), "Ctrl-E")
        if self.ModuleType() == TYPE_MODULE or self.ModuleType() == TYPE_HRL:
            self.editorMenu.AppendMenuItem('Comment lines', core.MainFrame, lambda e: self.CommentLines(), "Ctrl-/")

    def SetupLanguageStyles(self):
        ErlangHighlightedSTCBase.SetupLanguageStyles(self)

        self.IndicatorSetStyle(1, stc.STC_INDIC_PLAIN)
        self.MarkerDefine(self.MARKER_ERROR, stc.STC_MARK_BACKGROUND,
                          foreground=ColorSchema.codeEditor["error_line_color"],
                          background=ColorSchema.codeEditor["error_line_color"])
        self.MarkerDefine(self.MARKER_WARNING, stc.STC_MARK_BACKGROUND,
                          foreground=ColorSchema.codeEditor["warning_line_color"],
                          background=ColorSchema.codeEditor["warning_line_color"])

        self.MarkerDefine(self.MARKER_ERROR_CIRCLE, stc.STC_MARK_CIRCLE,
                          foreground=ColorSchema.codeEditor["error_marker_color"],
                          background=ColorSchema.codeEditor["error_marker_color"])

        self.MarkerDefine(self.MARKER_WARNING_CIRCLE, stc.STC_MARK_CIRCLE,
                          foreground=ColorSchema.codeEditor["warning_marker_color"],
                          background=ColorSchema.codeEditor["warning_marker_color"])

        self.SetMarginMask(2, ~stc.STC_MASK_FOLDERS)

    def CreatePopupMenu(self):
        menu = CustomSTC.CreatePopupMenu(self)
        if self.ModuleType() == TYPE_MODULE:
            compileOptionMenu = Menu()
            menu.AppendMenu(wx.ID_ANY, "Compile Option", compileOptionMenu)
            compileOptionMenu.AppendMenuItem("With 'P' flag", self,
                                             lambda e: core.Project.CompileOption(self.filePath, "P"))
            compileOptionMenu.AppendMenuItem("With 'E' flag", self,
                                             lambda e: core.Project.CompileOption(self.filePath, "E"))
            compileOptionMenu.AppendMenuItem("With 'core' flag", self,
                                             lambda e: core.Project.CompileOption(self.filePath, "to_core"))
            compileOptionMenu.AppendMenuItem("With 'S' flag", self,
                                             lambda e: core.Project.CompileOption(self.filePath, "S"))

        result = self.GetErlangWordAtPosition(self.popupPos)
        if result:
            style = self.GetStyleAt(self.popupPos)

            if style in self.SimpleFindRefTypes():
                menu.AppendMenuItem("Find references as {}".format(self.StringStyleType(style)), self,
                                    lambda e: self.FindReferences())
            elif style in self.CompoundFindRefTypes():
                submenu = Menu()
                menu.AppendMenu(wx.ID_ANY, "Find references as", submenu)
                submenu.AppendMenuItem(self.StringStyleType(style), self, lambda e: self.FindReferences())
                submenu.AppendMenuItem("Atom", self, lambda e: self.FindReferences(True))

            if style == ErlangHighlightType.RECORD:
                recordName = result[2]
                if recordName[0] == "#":
                    recordName = recordName[1:]
                record = self.completer.GetRecord(recordName)
                if record is None:
                    records = ErlangCache.AllRecords()
                    records = [r for r in records if r.name == recordName]
                    if len(records) > 0:
                        def insertInclude(rec):
                            app = core.Project.GetApp(rec.file)
                            incl = app + "/include/" + os.path.basename(rec.file)
                            return incl, lambda e: self.InsertInclude(app, incl)

                        if len(records) == 1:
                            includeStr, action = insertInclude(records[0])
                            menu.AppendMenuItem("Resolve as " + includeStr, self, action)
                        else:
                            submenu = Menu()
                            menu.AppendMenu(wx.ID_ANY, "Resolve as", submenu)
                            for r in records:
                                includeStr, action = insertInclude(r)
                                submenu.AppendMenuItem(includeStr, self, action)

                else:
                    def unfold(rec, start, end):
                        return lambda e: self.UnfoldRecord(rec, start, end)

                    menu.AppendMenuItem("Unfold fields", self, unfold(record, result[0], result[1]))

            if style == ErlangHighlightType.FUNDEC:
                menu.AppendMenuItem("Add to export", self, lambda e: self.AddToExport())
                menu.AppendMenuItem("Gen spec", self, lambda e: self.GenSpec())

            tokens = self.completer.tokenizer.GetTokens(self.GetLineText(self.LineFromPosition(self.popupPos)))
            if tokens and tokens[0].value.startswith("-behavi"):
                module = tokens[2].value
                menu.AppendMenuItem("Implement behavior {}".format(module), self,
                                    lambda e: self.ImplementBehavior(module))

            if style == ErlangHighlightType.FUNCTION or style == ErlangHighlightType.FUNDEC or style == ErlangHighlightType.VAR:
                menu.AppendMenuItem("Rename", self, lambda e: self.RefactorRename())

        return menu

    def ImplementBehavior(self, module):
        callbacks = ErlangCache.ModuleCallbacks(module)
        if not callbacks:
            return
        behaviorText = readFile(callbacks[0].file)
        self.BeginUndoAction()
        for cb in callbacks:
            if not ErlangCache.ModuleFunction(self.ModuleName(), cb.name, cb.arity):
                exp = "^-callback " + cb.name + "\(" + ",".join([".*?"] * cb.arity) + "\."
                r = re.compile(exp, re.MULTILINE | re.DOTALL)
                match = r.search(behaviorText, 0)
                if not match:
                    continue
                specText = match.group(0).replace("-callback", "-spec")
                callbackImplementation = "\n" + specText + "\n\n" + "{}({}) ->\n    erlang:error({{not_implemented, {}}}).\n".format(
                    cb.name, ", ".join(["_" + p for p in cb.params]), cb.name
                )
                # core.Log(specText)
                # core.Log(callbackImplementation)
                self.InsertText(self.LastPosition, callbackImplementation)
                self.AddToExportFunArity(cb.name, cb.arity)
        self.EndUndoAction()

    def InsertInclude(self, app, include):
        includeStr = "-include_lib(\"" + app + "/include/" + os.path.basename(include) + "\")."
        if app == core.Project.GetApp(self.filePath):
            includeStr = "-include(\"" + os.path.basename(include) + "\")."
        pos = self.lexer.GetExportInsertPosition()
        self.InsertText(pos, includeStr)

    def UnfoldRecord(self, record, start, end):
        self.SetTargetStart(start)
        core.Log(self.GetCharAt(end))
        while not self.GetCharAt(end).isspace():
            end += 1
        self.SetTargetEnd(end)
        recordFields = ", ".join([field + " = " + underscoreToCamelcase(field) for field in record.fields])
        unfoldedRecord = "#" + record.name + "{" + recordFields + "}"
        core.Log(unfoldedRecord)
        self.ReplaceTarget(unfoldedRecord)
        self.GotoPos(start + len(unfoldedRecord))

    def StringStyleType(self, style):
        if style in [ErlangHighlightType.FUNCTION, ErlangHighlightType.FUNDEC]:
            return "Function call\definition"
        elif style in [ErlangHighlightType.RECORD, ErlangHighlightType.RECORDDEF]:
            return "Record use\definition"
        elif style in [ErlangHighlightType.MODULE]:
            return "Module use\definition"
        elif style in [ErlangHighlightType.ATOM]:
            return "Atom"
        elif style in [ErlangHighlightType.MACROS]:
            return "Macros"
        raise Exception("Unsupported type")

    def FindRefTypes(self):
        return self.CompoundFindRefTypes() + self.SimpleFindRefTypes()

    def SimpleFindRefTypes(self):
        return [ErlangHighlightType.ATOM, ErlangHighlightType.MACROS]

    def CompoundFindRefTypes(self):
        return [ErlangHighlightType.FUNCTION, ErlangHighlightType.FUNDEC,
                ErlangHighlightType.RECORD, ErlangHighlightType.RECORDDEF,
                ErlangHighlightType.MODULE]

    def RefactorRename(self):
        result = self.GetErlangWordAtPosition(self.popupPos)
        if not result: return
        (start, end, value) = result
        style = self.GetStyleAt(self.popupPos)
        line = self.LineFromPosition(start)
        text = self.GetLineText(line)

        if style == ErlangHighlightType.VAR:
            funData = self.lexer.GetCurrentFunction(line, self.popupPos)
            if not funData: return
            (funName, funStart, funEnd, funText) = funData
            specData = self.lexer.GetFunctionSpec(line - 1, funName)
            if specData:
                (specStart, specText) = specData
                funStart = specStart
                funText = specText + "\n" + funText


            flags = re.MULTILINE | re.DOTALL
            pattern = r"\b" + value + r"\b"
            replaceRegexp = re.compile(pattern, flags)

            if replaceRegexp.search(text):
                dialog = wx.TextEntryDialog(None, "New name:", "Rename variable", value, style=wx.OK | wx.CANCEL)
                if dialog.ShowModal() == wx.ID_OK:
                    name = dialog.GetValue()
                    funText = replaceRegexp.sub(name, funText)
                    self.SetTargetStart(funStart)
                    self.SetTargetEnd(funEnd)
                    self.ReplaceTarget(funText)
                    dialog.Destroy()
        elif style in [ErlangHighlightType.FUNCTION, ErlangHighlightType.FUNDEC]:
            module = self.ModuleName()
            if style == ErlangHighlightType.FUNCTION:
                prefix = text[:start - self.PositionFromLine(line)]
                if prefix and prefix[-1] == ":":
                    tokens = self.completer.tokenizer.GetTokens(prefix[:-1])
                    lastToken = tokens[-1]
                    if tokens:
                        if lastToken.type == ErlangTokenType.ATOM:
                            module = lastToken.value
                        elif lastToken.type == ErlangTokenType.MACROS:
                            macros = lastToken.value
                            macrosData = ErlangCache.MacrosData(self.ModuleName(), macros)
                            if macrosData:
                                module = macrosData.value

            dialog = wx.TextEntryDialog(None, "New name:", "Rename variable", value, style=wx.OK | wx.CANCEL)
            if dialog.ShowModal() == wx.ID_OK:
                newName = dialog.GetValue()

                for p in [
                            r"(?<=\b{0}:){1}(?=\()",
                            r"(?<=\s{0}:){1}(?=/[0-9])"
                          ]:
                    p = p.format(module, value)
                    ReplaceInProject(re.compile(p, re.MULTILINE | re.DOTALL), newName, [".erl", ".hrl"])

                for p in [
                            r"\b{0}(?=\()",
                            r"(?<=\?MODULE:){0}(?=\()",
                            r"(?<=\?MODULE:){0}(?=/[0-9])",
                            r"\b{0}(?=/[0-9])",
                            r"(?<=-spec ){0}(?=\()"
                      ]:
                    p = p.format(value)
                    regexp = re.compile(p, re.MULTILINE | re.DOTALL)
                    text = self.GetText()
                    if regexp.search(text):
                        text = regexp.sub(newName, text)
                        self.SetText(text)
                        self.Save()


    def FindReferences(self, asAtom=False):
        result = self.GetErlangWordAtPosition(self.popupPos)
        if not result: return
        (start, end, value) = result
        style = self.GetStyleAt(self.popupPos)
        line = self.LineFromPosition(start)
        text = self.GetLineText(line)
        if asAtom:
            Find(value, "Find reference of atom '{}'".format(value),
                 wholeWords=True,
                 matchCase=True,
                 fileExts=[".erl", ".hrl", ".src"],
                 resultsFilter=lambda r: self.CheckResult(r, [value], self.CompoundFindRefTypes()))
        elif style == ErlangHighlightType.ATOM:
            Find(value, "Find reference of atom '{}'".format(value),
                 wholeWords=True,
                 matchCase=True,
                 fileExts=[".erl", ".hrl", ".src"],
                 resultsFilter=lambda r: self.CheckResult(r, [value], [ErlangHighlightType.ATOM]))
        elif style in [ErlangHighlightType.FUNCTION, ErlangHighlightType.FUNDEC]:
            module = self.ModuleName()
            if style == ErlangHighlightType.FUNCTION:
                prefix = text[:start - self.PositionFromLine(line)]
                if prefix and prefix[-1] == ":":
                    tokens = self.completer.tokenizer.GetTokens(prefix[:-1])
                    lastToken = tokens[-1]
                    if tokens:
                        if lastToken.type == ErlangTokenType.ATOM:
                            module = lastToken.value
                        elif lastToken.type == ErlangTokenType.MACROS:
                            macros = lastToken.value
                            macrosData = ErlangCache.MacrosData(self.ModuleName(), macros)
                            if macrosData:
                                module = macrosData.value
            moduleFile = "{}.erl".format(module)
            Find(r"\b{0}:{1}\(|\s{0}:{1}/|^\s*{1}\(|\?MODULE:{1}\(|\?MODULE:{1}/|{1}/|[^:]\b{1}(?=/[0-9])".format(module, value),
                 "Find reference of function '{}'".format(value),
                 useRegexp=True,
                 fileExts=[".erl", ".hrl", moduleFile],
                 resultsFilter=lambda r: self.FunctionCheckResult(r, value, moduleFile))
        elif style == ErlangHighlightType.MODULE:
            Find(r"-module\({0}\)|-extends\({0}\)|\b{0}:".format(value), "Find reference of module '{}'".format(value),
                 useRegexp=True,
                 fileExts=[".erl", ".hrl"],
                 resultsFilter=lambda r: self.CheckResult(r, [value], [ErlangHighlightType.MODULE]))
        elif style in [ErlangHighlightType.RECORD, ErlangHighlightType.RECORDDEF]:
            if value.startswith("#"): value = value[1:]
            Find(r"-record\({0}\b|#{0}\b".format(value), "Find reference of record '{}'".format(value),
                 useRegexp=True,
                 fileExts=[".erl", ".hrl"],
                 resultsFilter=lambda r: self.CheckResult(r, [value, "#" + value],
                                                          [ErlangHighlightType.RECORD, ErlangHighlightType.RECORDDEF]))
        elif style == ErlangHighlightType.MACROS:
            if value.startswith("?"): value = value[1:]
            Find(r"-define\({0}\b|\?{0}\b".format(value), "Find reference of macros '{}'".format(value),
                 useRegexp=True,
                 fileExts=[".erl", ".hrl"],
                 resultsFilter=lambda r: self.CheckResult(r, [value, "?" + value], [ErlangHighlightType.MACROS]))

    def FunctionCheckResult(self, result, value, module):
        tokens = self.lexer.highlighter.GetHighlightingTokens(result.lineText)
        return any([token.value == value and
                    (token.type == ErlangHighlightType.FUNCTION or
                     (token.type == ErlangHighlightType.FUNDEC and os.path.basename(result.file) == module)) for token
                    in tokens])

    def CheckResult(self, result, values, styles):
        tokens = self.lexer.highlighter.GetHighlightingTokens(result.lineText)
        # print values, "tokens:{}".format([token.value for token in tokens])
        # print "result ", any([token.value in values and token.type in styles for token in tokens])
        return any([token.value in values and token.type in styles for token in tokens])

    def ModuleName(self):
        name = self.FileName()
        if IsModule(name):
            return name[:-4]
        else:
            return name

    def ModuleType(self):
        name = self.ModuleName()
        if name.endswith('.hrl'):
            return TYPE_HRL
        elif name.find('.') < 0:
            return TYPE_MODULE
        return TYPE_UNKNOWN

    def OnAutoComplete(self):
        self.UpdateCompleter()
        if len(self.completer.list.Items) == 1 and self.completer.lastData and isinstance(self.completer.lastData,
                                                                                          unicode):
            self.completer.AutoComplete(self.completer.list.Items[0])
        else:
            self.completer.Show()

    def OnUpdateCompleter(self, event):
        event.Skip()
        if not self.completer.IsShown(): return
        self.UpdateCompleter()

    def UpdateCompleter(self, event=None):
        caretPos = self.GetCurrentPos()
        (isRecField, record, prefix) = self.lexer.RecordFieldUnderCursor()
        if isRecField:
            self.completer.UpdateRecordField(record, prefix)
            self.completer.lastText = prefix
        else:
            line = self.GetCurrentLine()
            prefix = self.GetTextRange(self.PositionFromLine(line), caretPos)
            self.completer.Update(prefix)
        self.completer.UpdateCompleterPosition(self.PointFromPosition(caretPos))

    def HandleKeyDownEvent(self, event):
        keyCode = event.GetKeyCode()
        result = CustomSTC.HandleKeyDownEvent(self, event)
        if result: return result

        if (self.completer.IsShown() and
                keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER,
                            wx.WXK_DOWN, wx.WXK_UP, wx.WXK_ESCAPE]):
            self.completer.OnKeyDown(event)
            return True

        if keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER] and event.GetModifiers() == wx.MOD_ALT:
            activeToolPage = core.ToolMgr.GetSelection()
            activeTool = core.ToolMgr[activeToolPage]
            consoles = core.Project.consoles
            if activeTool not in consoles.values():
                activeTool = consoles.values()[0]
            core.ToolMgr.FocusOnWidget(activeTool)
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
        dlg = ErlangOutline(self, self.filePath)
        dlg.ShowModal()

    def CommentLines(self):
        if core.TabMgr.GetActiveEditor() != self:
            return

        start = self.GetSelectionStart()
        end = self.GetSelectionEnd()
        startLine = self.LineFromPosition(start)
        endLine = self.LineFromPosition(end)
        if not (abs(endLine - startLine) > 1 and end == self.PositionFromLine(endLine)):
            endLine += 1
        lines = range(startLine, endLine)
        allComments = True
        for line in lines:
            text = self.GetLine(line).strip()
            if text and not text.startswith('%'):
                allComments = False
                break
        self.BeginUndoAction()
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
        if start - end != 0:
            self.SetSelectionStart(self.PositionFromLine(startLine))
            self.SetSelectionEnd(self.GetLineEndPosition(endLine - 1))
        self.EndUndoAction()

    def OnMouseMove(self, event):
        event.Skip()
        self.ClearIndicator(1)
        self.navigateTo = None
        pos = self.PositionFromPoint(event.GetPosition())

        if event.GetPosition()[0] < self.LineNumbersWidth() + self.FoldWidth + 10:
            self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
            return

        if event.GetModifiers() == wx.MOD_CONTROL and self.HasFocus():
            if not self.CheckNavigation(pos):
                self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
        elif self.HasFocus():
            self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))

    def OnRequestTooltipText(self):
        if wx.GetKeyState(wx.WXK_CONTROL):
            return None
        if self.completer.IsShown():
            return None
        data = self.GetContextData()
        if not data:
            pos = self.PositionFromPoint(self.ScreenToClient(wx.GetMousePosition()))
            line = self.LineFromPosition(pos)
            errs = list(filter(lambda e: e.line == line, self.lastErrors))
            if errs:
                data = reduce(lambda msg, e: msg + e.msg + "\n", errs, "")
        return data

    def GetErlangWordAtPosition(self, pos):
        style = self.GetStyleAt(pos)
        start = pos
        end = pos
        while self.GetStyleAt(start - 1) == style:
            start -= 1
        while self.GetStyleAt(end + 1) == style:
            end += 1
        if start == end and not self.GetTextRange(start, start + 1).isalpha():
            return False
        end += 1
        return (start, end, self.GetTextRange(start, end))

    def GetContextData(self):
        pos = self.PositionFromPoint(self.ScreenToClient(wx.GetMousePosition()))
        style = self.GetStyleAt(pos)
        if style not in [ErlangHighlightType.FUNCTION,
                         ErlangHighlightType.MACROS,
                         ErlangHighlightType.RECORD]:
            return None

        result = self.GetErlangWordAtPosition(pos)
        if not result: return
        (start, end, value) = result
        line = self.LineFromPosition(pos)
        lineStart = self.PositionFromLine(line)
        prefix = self.GetTextRange(lineStart, start)
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
                         ErlangHighlightType.FUNDEC,
                         ErlangHighlightType.MACROS,
                         ErlangHighlightType.MODULE,
                         ErlangHighlightType.RECORD,
                         ErlangHighlightType.MODULEATTR,
                         ErlangHighlightType.STRING]:
            return False
        result = self.GetErlangWordAtPosition(pos)
        if not result: return
        (start, end, value) = result

        line = self.LineFromPosition(pos)
        lineText = self.GetLine(line).strip()
        lineStart = self.PositionFromLine(line)
        lineEnd = self.GetLineEndPosition(line)
        prefix = self.GetTextRange(lineStart, start)
        postfix = self.GetTextRange(end, lineEnd)
        data = None
        if style == ErlangHighlightType.FUNDEC:
            (exports, _s, _end, _last, _r) = self.lexer.GetAllExports()
            if value + "/" in exports:
                self.navigateTo = (self.filePath, 1 + self.LineFromPosition(self.Text.find(value + "/")))
        elif style == ErlangHighlightType.FUNCTION:
            data = self.completer.GetFunctionNavAndHelp(value, prefix, end)
        elif style == ErlangHighlightType.RECORD:
            data = self.completer.GetRecordNavAndHelp(value)
        elif style == ErlangHighlightType.MACROS:
            data = self.completer.GetMacrosNavAndHelp(value)
        elif style in [ErlangHighlightType.ATOM, ErlangHighlightType.MODULE]:
            if value in ErlangCache.AllModules():
                self.navigateTo = (ErlangCache.modules[value].file, 0)

        elif style == ErlangHighlightType.MODULEATTR or ErlangHighlightType.STRING:
            path = None
            isIncludeLib = False
            if (style == ErlangHighlightType.MODULEATTR and value in ["include", "include_lib"]):
                isIncludeLib = value == "include_lib"
                path = postfix[2:len(postfix) - 3]
            elif style == ErlangHighlightType.STRING and lineText.startswith("-include"):
                s = 14 if lineText.startswith("-include_lib") else 10
                isIncludeLib = lineText.startswith("-include_lib")
                path = lineText[s:len(lineText) - 3]
            if path:
                if isIncludeLib:
                    app = path.split("/")[0]
                else:
                    app = core.Project.GetApp(self.filePath)
                include = (app, path.split("/")[-1])
                if include in ErlangCache.includes:
                    self.navigateTo = (ErlangCache.includes[include].file, 0)
                    start = lineStart
                    end = lineEnd
        if style in [ErlangHighlightType.FUNCTION, ErlangHighlightType.RECORD, ErlangHighlightType.MACROS]:
            if data:
                self.navigateTo = data[0]
            else:
                return False
        if self.navigateTo:
            line = self.LineFromPosition(pos)
            self.navigateTo += (line,)
            if self.navigateTo[0]:
                self.SetCursor(wx.StockCursor(wx.CURSOR_HAND))
                self.SetIndicatorCurrent(1)
                self.IndicatorFillRange(start, end - start)
            return True
        return False

    def OnMouseClick(self, event):
        if event.GetModifiers() == wx.MOD_CONTROL:
            if self.navigateTo:
                core.TabMgr.LoadFileLine(self.navigateTo[0], self.navigateTo[1] - 1, True, self.navigateTo[2])
                return
        event.Skip()

    def OnMiddleMouseClick(self, event):
        event.Skip()

    def OnFlyTimer(self, event):
        if core.Project.IsFlyCompileEnabled() and self.changed and os.path.exists(self.filePath):
            currentHash = hash(self.GetText())
            if currentHash == self.flyCompileHash: return
            self.flyCompileHash = currentHash
            self.CompileFly()

    def CompileFly(self):
        core.Project.CompileFileFly(os.path.basename(self.filePath), self.filePath, self.GetText())

    def HighlightErrors(self, errors):
        self.MarkerDeleteAll(self.MARKER_WARNING)
        self.MarkerDeleteAll(self.MARKER_ERROR)
        self.MarkerDeleteAll(self.MARKER_WARNING_CIRCLE)
        self.MarkerDeleteAll(self.MARKER_ERROR_CIRCLE)
        self.lastErrors = errors
        self.errorsLines = map(lambda x: x.line, errors)
        wMarkers = []
        eMarkers = []
        errors = sorted(errors, key=lambda e: e.type)
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
            if highlightLine:
                self.MarkerAdd(e.line, indic_line)
            self.MarkerAdd(e.line, indic_margin)
        self.markerPanel.SetMarkers("warning", wMarkers)
        self.markerPanel.SetMarkers("error", eMarkers)
        self.Refresh()

    def DoIndent(self):
        text = self.GetLine(self.CurrentLine - 1).strip()
        indent = self.GetLineIndentation(self.CurrentLine - 1)
        if (text.endswith("{") or
                text.endswith("[") or
                text.endswith("[") or
                text.endswith("||") or
                text.endswith("=") or
                text.endswith("begin") or
                text.endswith("try") or
                text.endswith("catch") or
                text.endswith("when") or
                text.endswith("of") or
                text.endswith("->") or
                text.endswith("(")):
            indent += 4
        elif text.endswith("."):
            indent = 0
        elif text.endswith(";"):
            indent -= 4
        else:
            for (op, cl) in [("(", ")"), ("{", "}"), ("[", "]")]:
                if text.count(op) > text.count(cl) and text.endswith(","):
                    indent += 4

        indent = 0 if indent < 0 else indent
        self.InsertText(self.CurrentPos, " " * indent)
        pos = self.PositionFromLine(self.CurrentLine)
        self.GotoPos(pos + indent)

    def AddToExportFunArity(self, fun, arity):
        funStr = "{}/{}".format(fun, arity)
        (exports, _startPos, _endPos, _insertPos, ranges) = self.lexer.GetAllExports()
        if funStr in str(exports.replace(" ", "").split(",")):
            return

        if len(ranges) > 0:
            (start, end) = ranges[0]
            funStr = "    {}/{}".format(fun, arity)
            if self.Text[start:end].strip() != "":
                funStr = ",\n" + funStr
            else:
                end = start
                funStr += "\n"
            self.InsertText(end, funStr)

    def AddToExport(self):
        funData = self.lexer.GetCurrentFunction()
        if not funData: return

        fun = funData[0]
        arity = self.completer.GetFunArity(funData[1] + len(fun))
        self.AddToExportFunArity(fun, arity)

    def GenSpec(self):
        funData = self.lexer.GetCurrentFunction()
        if not funData: return
        fun = funData[0]
        arity = self.completer.GetFunArity(funData[1] + len(fun))
        data = ErlangCache.ModuleFunction(self.ModuleName(), fun, arity)
        spec = "-spec {} -> FunResult.\n\n".format(self.completer.FunctionText(data))
        self.InsertText(funData[1], spec)

    def GoToExport(self):
        funData = self.lexer.GetCurrentFunction()
        if not funData: return

        fun = funData[0]
        arity = self.completer.GetFunArity(funData[1] + len(fun))
        funStr = "{}/{}".format(fun, arity)
        (exports, startPos, _endPos, insertPos, _r) = self.lexer.GetAllExports()
        if funStr not in exports: return
        self.SetTargetStart(startPos)
        self.SetTargetEnd(insertPos)
        pos = self.SearchInTarget(funStr)
        if pos >= 0:
            self.SetAnchor(pos + len(funStr))
            self.GotoPos(pos)

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

    def OnClose(self):
        ErlangHighlightedSTCBase.OnClose(self)
        self.CompileFly()
        if self.flyTimer:
            self.flyTimer.Stop()



class ErlangSTCReadOnly(ErlangSTC):
    def __init__(self, parent, panel, filePath, option, text):
        ErlangSTC.__init__(self, parent, panel)
        self.AppendText(text)
        self.filePath = filePath
        self.option = option

    def OnInit(self):
        ErlangSTC.OnInit(self)

        self.SetReadOnly(True)
        self.SetToolTip(None)

        core.Project.explorer.ProjectFilesModifiedEvent += self.OnProjectFilesModified

    def SetupEditorMenu(self):
        ErlangHighlightedSTCBase.SetupEditorMenu(self)

    def CreatePopupMenu(self, event):
        return None

    def Changed(self, changed=True):
        pass

    def OnClose(self):
        core.Project.explorer.ProjectFilesModifiedEvent -= self.OnProjectFilesModified
        ErlangSTC.OnClose(self)

    def Save(self):
        pass

    def OnProjectFilesModified(self, files):
        if not self: return
        for f in files:
            if f == self.filePath:
                core.Project.CompileOption(self.filePath, self.option)

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
            core.Project.CompileOption(self.filePath, self.option)
            return True
        else:
            return False


class ErlangConsoleSTC(ConsoleSTC):
    erlangConsoleRe = re.compile(
        r"""
        (?P<reload>Module:.*?Reload\ssucceeded\.)|
        (?P<check_cache>Checking\scache\sfor\serlang\slibs.*?$)|
        (?P<custom_log>%Log:.*?$)|
        (?P<error_report>\b(level=error)|(ERROR\sREPORT)\b)|
        (?P<crash_report>CRASH\sREPORT)|
        (?P<module_fun_line>\{(?P<module>[a-zA-Z_][a-zA-Z_\d]*),(?P<fun>[a-zA-Z_][a-zA-Z_\d]*),(?P<arity>\d+),)|
        (?P<mfa>(?P<module1>[a-zA-Z_][a-zA-Z_\d]*):(?P<fun1>[a-zA-Z_][a-zA-Z_\d]*)\/(?P<arity1>\d+)(:(?P<line1>\d+))?)|
        (?P<file_line>\{file,\s*"(?P<file>.*?)"\},\s*?\{line,\s*(?P<fline>\d+)\})|
        (?P<f>\((?P<file1>.*?),\s+line\s+(?P<fline1>\d+)\))
        """,
        re.VERBOSE | re.MULTILINE)

    navigationData = {}

    def Append(self, text):
        text = text.rstrip()
        text += "\n"
        currentPos = self.Length
        ConsoleSTC.Append(self, text)
        self.ParseText(currentPos, text)
        self.markerPanel.Paint()

    def ParseText(self, lastPosition, text):
        endPos = 0
        while True:
            m = self.erlangConsoleRe.search(text, endPos)
            if not m: break
            last = m.lastgroup
            d = m.groupdict()
            startPos = m.start()
            endPos = m.end()
            if last == "module_fun_line" or last == "mfa":
                postfix = ""
                if last == "mfa":
                    postfix = "1"
                module = d["module" + postfix]
                fun = d["fun" + postfix]
                arity = int(d["arity" + postfix])
                line = self.LineFromPosition(lastPosition + startPos)
                navTo = ErlangCache.ModuleFunction(module, fun, arity)

                if navTo:
                    targetLine = navTo.line
                    if navTo and last == "mfa" and d["line" + postfix]:
                        targetLine = int(d["line" + postfix])
                    self.navigationData[self.GetTextRangeUTF8(lastPosition + startPos, lastPosition + endPos)] = (navTo.file, targetLine)
                    self.SetIndicatorCurrent(1)
                    self.IndicatorFillRange(lastPosition + startPos, endPos - startPos)

            elif last == "file_line" or last == "f":
                postfix = ""
                if last == "f":
                    postfix = "1"
                filePath = d["file" + postfix]
                fline = int(d["fline" + postfix])
                if not os.path.isfile(filePath):
                    (module, _) = os.path.splitext(filePath)
                    if module in ErlangCache.modules:
                        filePath = ErlangCache.modules[module].file
                    else:
                        continue
                filePath = os.path.normpath(filePath)
                startLine = self.LineFromPosition(lastPosition + startPos)
                endLine = self.LineFromPosition(lastPosition + endPos)
                for line in range(startLine, endLine + 1):
                    self.navigationData[self.GetTextRangeUTF8(lastPosition + startPos, lastPosition + endPos)] = (filePath, fline)
                    self.SetIndicatorCurrent(1)
                    self.IndicatorFillRange(lastPosition + startPos, endPos - startPos)
            elif last == "custom_log":
                self.StartStyling(lastPosition + startPos, 0x1f)
                self.SetStyling(endPos - startPos, 2)
            elif last in ["reload", 'check_cache']:
                self.StartStyling(lastPosition + startPos, 0x1f)
                self.SetStyling(endPos - startPos, 1)
            elif last in ["error_report", "crash_report"]:
                errorMarkers = self.markerPanel.markers["error"]
                errorMarkers.append(Marker(self.LineFromPosition(lastPosition + startPos), last.replace("_", " ")))
                self.markerPanel.SetMarkers("error", errorMarkers)

    def OnInit(self):
        self.markerPanel.SetMarkerColor("error", ColorSchema.codeEditor["error_marker_color"])

        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)

    def Clear(self):
        ConsoleSTC.Clear(self)
        self.markerPanel.SetMarkers("error", [])
        self.navigationData = {}
        self.ClearIndicator(1)

    def OnMouseMove(self, event):
        self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
        pos = self.PositionFromPoint(event.GetPosition())
        if (self.IndicatorAllOnFor(pos) & (1 << 1) == (1 << 1)) and event.GetModifiers() == wx.MOD_CONTROL:
            self.SetCursor(wx.StockCursor(wx.CURSOR_HAND))
        event.Skip()

    def OnMouseClick(self, event):
        if event.GetModifiers() == wx.MOD_CONTROL:
            pos = self.PositionFromPoint(event.GetPosition())
            start = self.IndicatorStart(1, pos)
            end = self.IndicatorEnd(1, pos)
            text = self.GetTextRangeUTF8(start, end)
            if text in self.navigationData:
                (f, line) = self.navigationData[text]
                core.TabMgr.LoadFileLine(f, line - 1, False)
                return
        event.Skip()

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("erlang")
        self.StyleSetSpec(1, formats["module"])
        self.StyleSetSpec(2, formats["string"])


class IgorSTC(CustomSTC):
    def OnInit(self):
        self.navigateTo = None

        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_DOWN, self.OnMouseClick)

    def SetupLexer(self):
        self.lexer = IgorLexer(self)
        self.SetLexer(stc.STC_LEX_CONTAINER)

    def SetupLanguageStyles(self):
        formats = ColorSchema.LanguageFormats("igor")
        self.StyleSetSpec(IgorHighlightType.DEFAULT, formats["default"])
        self.StyleSetSpec(IgorHighlightType.STRING, formats["string"])
        self.StyleSetSpec(IgorHighlightType.NUMBER, formats["number"])
        self.StyleSetSpec(IgorHighlightType.KEYWORD, formats["keyword"])
        self.StyleSetSpec(IgorHighlightType.BASE_TYPE, formats["base_type"])
        self.StyleSetSpec(IgorHighlightType.CUSTOM_TYPE, formats["custom_type"])
        self.StyleSetSpec(IgorHighlightType.SPECIAL_SYMBOL, formats["special"])
        self.StyleSetSpec(IgorHighlightType.COMMENT, formats["comment"])
        self.StyleSetSpec(IgorHighlightType.ATTRIBUTE, formats["attribute"])
        self.StyleSetSpec(IgorHighlightType.ATTRIBUTE_TARGET, formats["attribute_t"])
        self.StyleSetSpec(IgorHighlightType.BRACKET, formats["bracket"])
        self.StyleSetSpec(IgorHighlightType.FIELD, formats["field"])
        self.StyleSetSpec(IgorHighlightType.ENUM_FIELD, formats["enum_field"])
        self.StyleSetSpec(IgorHighlightType.FUNCTION, formats["function"])

    def OnMouseMove(self, event):
        event.Skip()
        self.ClearIndicator(1)
        self.navigateTo = None
        pos = self.PositionFromPoint(event.GetPosition())

        if event.GetPosition()[0] < self.LineNumbersWidth() + self.FoldWidth + 10:
            self.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
            return

        if event.GetModifiers() == wx.MOD_CONTROL and self.HasFocus():
            if not self.CheckNavigation(pos):
                self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))
        elif self.HasFocus():
            self.SetCursor(wx.StockCursor(wx.CURSOR_IBEAM))

    def CheckNavigation(self, pos):
        self.navigateTo = None
        style = self.GetStyleAt(pos)
        if style not in [IgorHighlightType.CUSTOM_TYPE, IgorHighlightType.FUNCTION]:
            return False
        start = self.WordStartPosition(pos, True)
        end = self.WordEndPosition(pos, True)
        if start == end: return False
        line = self.LineFromPosition(pos)
        lineData = self.GetLine(line).strip()
        value = self.GetTextRange(start, end)
        if style == IgorHighlightType.CUSTOM_TYPE:
            if IgorCache.GetTypeOfEntry(value) == "record" and lineData.startswith("record"):
                recordData = ErlangCache.AllRecordsData(camelToLowerUnderscore(value))
                if recordData:
                    self.navigateTo = (recordData.file, recordData.line)
            else:
                self.navigateTo = IgorCache.FindCustomType(value)

        elif style == IgorHighlightType.FUNCTION:
            if lineData.startswith("c->s"):
                fileName, ext = os.path.splitext(os.path.basename(self.filePath))
                fileName = fileName.replace("protocol_", "")
                callbackModule = "handler_" + fileName
                funName = "on_" + camelToLowerUnderscore(value)
                navTo = ErlangCache.ModuleFunction(callbackModule, funName, None)
                if navTo:
                    self.navigateTo = (navTo.file, navTo.line)
            elif lineData.startswith("s->c"):
                fileName, ext = os.path.splitext(os.path.basename(self.filePath))
                fileName = fileName.replace("protocol_", "")
                callbackModule = "service_" + fileName
                funName = camelToLowerUnderscore(value)
                navTo = ErlangCache.ModuleFunction(callbackModule, funName, None)
                if navTo:
                    self.navigateTo = (navTo.file, navTo.line)

        if self.navigateTo:
            self.navigateTo += (line,)
            if self.navigateTo[0]:
                self.SetCursor(wx.StockCursor(wx.CURSOR_HAND))
                self.SetIndicatorCurrent(1)
                self.IndicatorFillRange(start, end - start)
            return True
        return False

    def OnMouseClick(self, event):
        if event.GetModifiers() == wx.MOD_CONTROL:
            if self.navigateTo:
                core.TabMgr.LoadFileLine(self.navigateTo[0], self.navigateTo[1] - 1, True, self.navigateTo[2])
                return
        event.Skip()

    def OnFileSaved(self):
        core.Project.FileSaved(self.filePath)
