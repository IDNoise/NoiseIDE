__author__ = 'Yaroslav'

import os
import wx
from idn_config import Config
from idn_cache import ErlangCache, Function, Record, ExportedType, Macros
from idn_colorschema import ColorSchema
from idn_erlang_constats import TYPE_MODULE, TYPE_HRL
from idn_token import ErlangTokenizer, ErlangTokenType
from idn_utils import readFile
from idn_completer import Completer
import core

class ErlangCompleter(Completer):
    def __init__(self, stc):
        Completer.__init__(self, stc)
        self.tokenizer = ErlangTokenizer()
        self.module = self.stc.ModuleName()
        self.moduleType = self.stc.ModuleType()

    def UpdateRecordField(self, record, prefix):
        self.prefix = prefix.strip()
        fields = ErlangCache.RecordFields(self.module, record)
        self._PrepareData(fields)

    def OnUpdate(self, text, nextChar = None):
        self.module = self.stc.ModuleName()
        self.moduleType = self.stc.ModuleType()
        tokens = self.tokenizer.GetTokens(text)
        tokens.reverse()
        data = []
        isReference = False
        if not tokens:
            data = self.GetVars()
        else:
            fToken = tokens[0]
            fType = fToken.type
            fValue = fToken.value
            fIsAtom = fType == ErlangTokenType.ATOM
            isReference = ((len(tokens) > 3 and tokens[3].value == "fun") or (len(tokens) > 4 and tokens[4].value == "fun"))
            if (fType == ErlangTokenType.SPACE or
                (len(tokens) == 1 and fIsAtom) or
                (fIsAtom and tokens[1].type == ErlangTokenType.SPACE) or
                (fIsAtom and tokens[1].value in self.separators) or
                fValue in self.separators):
                if fValue in self.separators or fType == ErlangTokenType.SPACE:
                    self.prefix = ""
                else:
                    self.prefix = fValue.strip()
                if self.moduleType == TYPE_MODULE:
                    if self.stc.lexer.IsInTypeBlock():
                        data += ErlangCache.ModuleExportedTypes(self.module)
                        data += ErlangCache.ERLANG_TYPES
                        if self.prefix:
                            data += ErlangCache.AllModules()
                    else:
                        data += ErlangCache.ModuleFunctions(self.module, False)
                        data += ErlangCache.Bifs()
                        data += ErlangCache.AllModules()
                else:
                    if self.moduleType == TYPE_HRL:
                        if self.stc.lexer.IsInTypeBlock():
                            data += ErlangCache.ERLANG_TYPES
                            if self.prefix:
                                data += ErlangCache.AllModules()
                        else:
                            data += ErlangCache.Bifs()
                            data += ErlangCache.AllModules()
            elif (len(tokens) > 1 and
                  ((fIsAtom and tokens[1].value == ":") or fValue == ":")):
                i = 1 if fValue == ":" else 2
                moduleName = str(tokens[i].value)
                onlyExported = True
                if moduleName == "?MODULE":
                    moduleName = self.module
                elif moduleName.startswith("?"):
                    macrosData = ErlangCache.MacrosData(self.module, moduleName[1:])
                    if macrosData:
                        moduleName = macrosData.value
                self.prefix = "" if fValue == ":" else fValue
                if self.stc.lexer.IsInTypeBlock():
                    data += ErlangCache.ModuleExportedTypes(moduleName)
                else:
                    data += ErlangCache.ModuleFunctions(moduleName, onlyExported)
            elif (fValue == "?" or fType == ErlangTokenType.MACROS):
                self.prefix = "" if fValue == "?" else fValue[1:]
                data = ErlangCache.Macroses(self.module)
            elif fType == ErlangTokenType.RECORD or fValue == "#":
                self.prefix = "" if fValue == "#" else fValue[1:]
                data = ErlangCache.ModuleRecords(self.module)
            elif (len(tokens) > 2 and fIsAtom and tokens[1].value == "."
                  and tokens[2].type == ErlangTokenType.RECORD):
                self.prefix = fValue
                record = tokens[2].value[1:]
                data = ErlangCache.RecordFields(self.module, record)
            elif (len(tokens) > 1 and fValue == "." and tokens[1].type == ErlangTokenType.RECORD):
                self.prefix = ""
                record = tokens[1].value[1:]
                data = ErlangCache.RecordFields(self.module, record)
            elif fType == ErlangTokenType.VAR:
                self.prefix = fValue
                data = self.GetVars()
            elif fType == ErlangTokenType.MODULEATTR and fValue.startswith("-i"):
                self.prefix = fValue
                data = ["-include(\"", "-include_lib(\""]
            elif (len(tokens) == 3 and fType == ErlangTokenType.STRING and tokens[2].value == "-include" and
                  tokens[2].type == ErlangTokenType.MODULEATTR):
                self.prefix = fValue[1:]
                data = ErlangCache.ApplicationIncludes(self.module)
            elif (len(tokens) == 3 and fType == ErlangTokenType.STRING and tokens[2].value == "-include_lib" and
                  tokens[2].type == ErlangTokenType.MODULEATTR):
                self.prefix = fValue[1:]
                data = ErlangCache.GlobalIncludes()
        self._PrepareData(data, isReference)

    def FunctionText(self, funData):
        params = funData.params[0][:]
        def prepare_param(p):
            if "\"" in p: return "String"
            if p[0].islower():
                return "Atom"
            return p
        params = [prepare_param(p) for p in params]
        text = "{}({})".format(funData.name, ", ".join(params))
        return text

    def _PrepareData(self, data, isReference = False):
        self.list.Clear()
        self.lastData = []
        for d in set(data):
            helpText = None
            if isinstance(d, Function):
                (_f, s, e, _l, _r) = self.stc.lexer.GetAllExports()
                if (isReference or (self.stc.CurrentPos >= s and self.stc.CurrentPos <= e)):
                    text = "{}/{}".format(d.name, d.arity)
                    helpText = self._FunctionHelp(d)
                else:
                    if Config.ShowMultiComplete():
                        text = []
                        helpText = []
                        for i in range(len(d.params)):
                            text.append("{}({})".format(d.name, ", ".join(d.params[i])))
                            helpText.append(self._FunctionHelp(d, i))
                    else:
                        params = d.params[0][:]
                        def prepare_param(p):
                            if "\"" in p: return "String"
                            if p[0].islower():
                                return "Atom"
                            return p
                        params = [prepare_param(p) for p in params]
                        text = "{}({})".format(d.name, ", ".join(params))
                        helpText = self._FunctionHelp(d)
            elif isinstance(d, Record):
                text = d.name
                helpText = self._RecordHelp(d)
            elif isinstance(d, ExportedType):
                text = d.name + "()"
                helpText = self._ExportedTypeHelp(d)
            elif isinstance(d, Macros):
                text = d.name
                helpText = self._MacrosHelp(d)
            elif isinstance(d, tuple):
                (text, helpText) = d
            else:
                text = d
                self.lastData = d
            if not isinstance(text, list):
                text = [text]
            if not isinstance(helpText, list):
                helpText = [helpText]
            for i in range(len(text)):
                if text[i].startswith(self.prefix):
                    self.list.Append(text[i], helpText[i])

    def _RecordHelp(self, record):
        fields = record.FieldsData()
        fields = [f[0] + "&nbsp;&nbsp;::&nbsp;&nbsp;" + f[1]  for f in fields]
        return "#{} [<br/>&nbsp;&nbsp;&nbsp;{}<br/>]<br/><br/>{}:{}".format(record.name, ",<br/>&nbsp;&nbsp;&nbsp;".join(fields),
            record.module, record.line)

    def _ExportedTypeHelp(self, expType):
        return "Types:{}<br/><br/>{}:{}".format(expType.types, expType.module, expType.line)

    def _MacrosHelp(self, macros):
        return "?{} -> {}<br/><br/>{}:{}".format(macros.name, macros.value, macros.module, macros.line)

    def _FunctionHelp(self, fun, clause = -1):
        if not fun.docref or not os.path.isfile(fun.docref):
            p = fun.params[:]
            t = [i[:] for i in p]
            if fun.types:
                 for i in range(len(p)):
                     for j in range(len(fun.types[i])):
                         t[i][j] = p[i][j] + " :: " + fun.types[i][j]
            #print "t: ", t
            res = fun.result[:]
            for i in range(len(fun.result)):
                if " :: " in fun.result[i]:
                    res[i] = fun.result[i].split(" :: ")
                    t[i].append(fun.result[i])
            #print "res: ", res
            comment = fun.comment.replace("\n", "<br/>").replace("%", "")
            t = [[tii for tii in ti  if  "::" in tii]for ti in t]
            if clause >= 0:
                params = ", ".join(p[clause])
                types = "Types:<br/>&nbsp;&nbsp;&nbsp;&nbsp;{}<br/>".format(",<br/>&nbsp;&nbsp;&nbsp;&nbsp;".join(t[clause])) if t[clause] else ""
                help = "{}({}) -> {}. <br/>".format(fun.name, params, res[clause], types)
            else:
                help = "".join(["{}({}) -> {}. <br/>{}"
                             .format(fun.name, ", ".join(p[i]), res[i],
                               "Types:<br/>&nbsp;&nbsp;&nbsp;&nbsp;{}<br/>".format(",<br/>&nbsp;&nbsp;&nbsp;&nbsp;".join(t[i])) if t[i]
                                     else "")
                             for i in range(len(p))])
            #if t:
            #    types = ["(" + ",<br/>&nbsp;&nbsp;".join(i) + ")" for i in t]
            #    help += "Types:<br/>&nbsp;&nbsp;{}".format(",<br/>&nbsp;&nbsp;".join(types))
            if comment:
                help += "<br/>{}".format(comment)
        else:
            help = readFile(fun.docref)
        return help

    def GetVars(self):
        funData = self.stc.lexer.GetCurrentFunction()
        if funData:
            text = funData[3][:self.stc.GetCurrentPos()]
            tokens = self.tokenizer.GetTokens(text)
            return list({token.value for token in tokens
                         if token.type == ErlangTokenType.VAR and token.value != self.prefix})
        return []

    def AutoComplete(self, text):
        toInsert = text[len(self.prefix):]
        nextChar = self.stc.GetCharAt(self.stc.CurrentPos)
        if nextChar == "(" and "(" in toInsert:
            toInsert = toInsert[:toInsert.find(nextChar)]

        line = self.stc.LineFromPosition(self.stc.CurrentPos)
        if self.stc.GetLineText(line).startswith("-include"):
            self.stc.SetTargetStart(self.stc.CurrentPos)
            self.stc.SetTargetEnd(self.stc.GetLineEndPosition(line))
            self.stc.ReplaceTarget(toInsert)
        else:
            self.stc.AddText(toInsert)
        self.HideCompleter()

    def GetFunctionNavAndHelp(self, fun, prefix, pos):
        arity = self.GetFunArity(pos)
        module = ""
        if prefix and prefix[-1] == ":" and prefix[-2] != ")":
            i = -2
            while abs(i) <= len(prefix) and (prefix[i].isalpha() or prefix[i].isdigit() or prefix[i] == "_"):
                module += prefix[i]
                i -= 1
            module = module[::-1]
            if abs(i) <= len(prefix) and prefix[i] == "?":
                macrosData = ErlangCache.MacrosData(self.module, module)
                if macrosData:
                    module = macrosData.value
            if not module[0].islower():
                module = self.module
        else:
            module = self.module
        data = ErlangCache.ModuleFunction(module, fun, arity)
        if not data:
            data = ErlangCache.ModuleExportedData(module, fun)
            if not data:
                data = ErlangCache.ModuleExportedData(module + ".hrl", fun)
                if not data:
                    data = ErlangCache.Bif(fun, arity)
                    if not data:
                        fun = fun.replace("@", "")
                        data = ErlangCache.ModuleFunction(module, fun, arity)
                        if not data:
                            return (None, None)
                    help = self._FunctionHelp(data)
                else:
                    help = self._ExportedTypeHelp(data)
            else:
                help = self._ExportedTypeHelp(data)
        else:
            help = self._FunctionHelp(data)
        f = data.moduleData.file if data.moduleData else None
        return ((f, data.line), help)

    def GetFunArity(self, pos):
        open = ['[', '(', '{', 'fun', 'case', 'if', 'try', 'begin', 'receive', '<<']
        close = {
            '[' : ']',
            '(' : ')',
            '{' : '}',
            'fun' : 'end',
            'case' : 'end',
            'if' : 'end',
            'try' : 'end',
            'begin' : 'end',
            'receive' : 'end',
            '<<' : '>>'
        }
        lvl = 0
        if self.stc.GetCharAt(pos) == "/":
            pos += 1
            arity = "0"
            while self.stc.GetCharAt(pos).isdigit():
                arity += self.stc.GetCharAt(pos)
                pos += 1
            return int(arity)

        arity = 0
        if self.stc.GetCharAt(pos) != "(":
            if self.stc.GetCharAt(pos + 1) == "(":
                pos += 1
            else:
                return arity
        sF = pos + 1 if pos + 1 < self.stc.GetLength() else self.stc.GetLength() - 1
        sT = pos + 6 if pos + 6 < self.stc.GetLength() else self.stc.GetLength() - 1
        postfix = self.stc.GetTextUTF8()[sF : sT].strip()
        if self.stc.GetCharAt(pos) == "(" and postfix and postfix[0] == ")":
            return 0
        else:
            arity = 1
        text = self.stc.GetTextUTF8()[pos:pos + 1000]
        gtokens = self.tokenizer.GetTokens(text)
        tokens = []
        try:
            for token in gtokens:
                if token.value == " ": continue
                if len(tokens) > 0 and tokens[-1] == token.value and (tokens[-1] == "<" or tokens[-1] == ">"):
                    tokens[-1] = token.value + token.value
                else:
                    tokens.append(token.value)
        except:
            pass

        for token in tokens:
            if token in open:
                lvl += 1
            elif token == "," and lvl == 1:
                arity += 1
            elif token in close.values():
                lvl -= 1
            if lvl == 0:
                break
        return arity

    def GetRecordNavAndHelp(self, record):
        recordData = self.GetRecord(record)
        if not recordData: return
        return (recordData.moduleData.file, recordData.line), self._RecordHelp(recordData)

    def GetRecord(self, recordName):
        if recordName[0] == "#": recordName = recordName[1:]
        return ErlangCache.RecordData(self.module, recordName)

    def GetMacrosNavAndHelp(self, macros):
        if macros[0] == "?": macros = macros[1:]
        macrosData = ErlangCache.MacrosData(self.module, macros)
        if not macrosData: return
        return ((macrosData.moduleData.file, macrosData.line), self._MacrosHelp(macrosData))


class ErlangSimpleCompleter(wx.Frame):
    SIZE = (340, 100)

    def __init__(self, textctrl):
        style = wx.BORDER_NONE | wx.STAY_ON_TOP | wx.FRAME_NO_TASKBAR
        pre = wx.PreFrame()
        pre.SetBackgroundStyle(wx.BG_STYLE_TRANSPARENT)
        pre.Create(textctrl, style = style, size = self.SIZE)
        self.PostCreate(pre)
        self.tokenizer = ErlangTokenizer()

        self.textctrl = textctrl
        self.lineHeight = 15
        self.separators = ",;([{<-"
        self.lastText = None

        self.list = wx.ListBox(self, size = self.SIZE, style = wx.LB_SORT | wx.LB_SINGLE | wx.WANTS_CHARS)
        self.list.SetBackgroundColour(ColorSchema.codeEditor["completer_list_back"])
        self.list.SetForegroundColour(ColorSchema.codeEditor["completer_list_fore"])

        sizer = wx.BoxSizer()
        sizer.Add(self.list, 1, flag = wx.EXPAND)
        self.SetSizer(sizer)
        self.Layout()
        self.Hide()

        self.list.Bind(wx.EVT_LISTBOX_DCLICK, self.OnItemDoubleClick)
        self.list.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.textctrl.Bind(wx.EVT_MOUSE_EVENTS, self.OnTextCtrlMouseDown)
        wx.GetApp().Bind(wx.EVT_ACTIVATE_APP, self.OnAppFocusLost)

    def OnAppFocusLost(self, event):
        try:
            self.HideCompleter()
        except:
            pass
        event.Skip()

    def OnTextCtrlMouseDown(self, event):
        event.Skip()
        if event.ButtonDown(wx.MOUSE_BTN_LEFT) or event.ButtonDown(wx.MOUSE_BTN_RIGHT):
            self.HideCompleter()

    def UpdateCompleterPosition(self, pos):
        pos = self.textctrl.ClientToScreen((pos[0], pos[1] - self.list.Size[1]))
        self.SetPosition(pos)

    def ValidateCompleter(self):
        if len(self.list.GetStrings()) == 0:
            self.HideCompleter()
            return
        self.list.SetSelection(0)

    def Update(self, text, nextChar = None):
        if self.lastText == text: return
        self.lastText = text
        tokens = self.tokenizer.GetTokens(text)
        tokens.reverse()
        data = []
        self.prefix = ""
        if not tokens:
            self.HideCompleter()
            return
        else:
            fToken = tokens[0]
            fType = fToken.type
            fValue = fToken.value
            fIsAtom = fType == ErlangTokenType.ATOM
            if (fType == ErlangTokenType.SPACE or
                (len(tokens) == 1 and fIsAtom) or
                (fIsAtom and tokens[1].type == ErlangTokenType.SPACE) or
                (fIsAtom and tokens[1].value in self.separators) or
                fValue in self.separators):
                if fValue in self.separators or fType == ErlangTokenType.SPACE:
                    self.prefix = ""
                else:
                    self.prefix = fValue.strip()
                data += ErlangCache.Bifs()
                data += ErlangCache.AllModules()
            elif (len(tokens) > 1 and
                  ((fIsAtom and tokens[1].value == ":") or fValue == ":")):
                i = 1 if fValue == ":" else 2
                moduleName = tokens[i].value
                onlyExported = True
                self.prefix = "" if fValue == ":" else fValue
                data += ErlangCache.ModuleFunctions(moduleName, onlyExported)
            elif (len(tokens) > 2 and fIsAtom and tokens[1].value == "."
                  and tokens[2].type == ErlangTokenType.RECORD):
                self.prefix = fValue
                record = tokens[2].value[1:]
                data = ErlangCache.AllRecordFields(record)
            elif (len(tokens) > 1 and fValue == "." and tokens[1].type == ErlangTokenType.RECORD):
                self.prefix = ""
                record = tokens[1].value[1:]
                data = ErlangCache.AllRecordFields(record)
            elif fType == ErlangTokenType.RECORD or fValue == "#":
                self.prefix = "" if fValue == "#" else fValue[1:]
                data = ErlangCache.AllRecords()
        self._PrepareData(data)

    def _PrepareData(self, data):
        self.list.Clear()
        self.lastData = []
        for d in set(data):
            if isinstance(d, Function):
                if Config.ShowMultiComplete():
                    text = []
                    for i in range(len(d.params)):
                        text.append("{}({})".format(d.name, ", ".join(d.params[i])))
                else:
                    params = d.params[0][:]
                    def prepare_param(p):
                        if "\"" in p: return "String"
                        if p[0].islower():
                            return "Atom"
                        return p
                    params = [prepare_param(p) for p in params]
                    text = "{}({})".format(d.name, ", ".join(params))
            elif isinstance(d, Record):
                text = d.name
            elif isinstance(d, tuple):
                (text, _h) = d
            else:
                text = d
                self.lastData = d
            if not isinstance(text, list):
                text = [text]
            for t in text:
                if t.startswith(self.prefix):
                    self.list.Append(t)
        self.ValidateCompleter()

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
        elif keyCode == wx.WXK_DOWN:
            current = self.list.GetSelection()
            if current == self.list.Count - 1:
                current = 0
            else:
                current += 1
            self.list.SetSelection(current)
        elif keyCode == wx.WXK_ESCAPE:
            self.HideCompleter()


    def AutoComplete(self, text):
        toInsert = text[len(self.prefix):]
        self.textctrl.WriteText(toInsert)
        self.HideCompleter()

    def HideCompleter(self):
        wx.Frame.Hide(self)

    def Show(self, show = True):
        if len(self.list.GetStrings()) > 0:
            wx.Frame.Show(self, show)
            self.textctrl.SetFocus()
