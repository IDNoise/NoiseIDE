import os
import wx
from idn_cache import ErlangCache, Function, Record, ExportedType, Macros
from idn_colorschema import ColorSchema
from idn_customstc import HtmlWin
from idn_erlang_constats import TYPE_MODULE
from idn_token import ErlangTokenizer, ErlangTokenType
from idn_utils import readFile

__author__ = 'Yaroslav'

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
        self.list.SetBackgroundColour(ColorSchema.codeEditor["completer_list_back"])
        self.list.SetForegroundColour(ColorSchema.codeEditor["completer_list_fore"])

        self.helpWindow = HtmlWin(self)

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
        wx.GetApp().Bind(wx.EVT_ACTIVATE_APP, self.OnAppFocusLost)

    def OnAppFocusLost(self, event):
        try:
            self.HideCompleter()
        except:
            pass
        event.Skip()

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
            #print len(tokens)
            #print tokens[0].value
            #print tokens[1].value
            #print tokens[2].value
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
                if self.moduleType == TYPE_MODULE:
                    if self.stc.lexer.IsInFunction():
                        data += ErlangCache.ModuleFunctions(self.module, False)
                        data += ErlangCache.Bifs()
                    else:
                        data += ErlangCache.ModuleExportedTypes(self.module)
                        data += ErlangCache.ERLANG_TYPES

                data += ErlangCache.AllModules()

            elif (len(tokens) > 1 and
                  ((fIsAtom and tokens[1].value == ":") or fValue == ":")):
                i = 1 if fValue == ":" else 2
                moduleName = tokens[i].value
                onlyExported = True
                if moduleName == "?MODULE":
                    moduleName = self.module
                    #onlyExported = False mb make it show all funs
                self.prefix = "" if fValue == ":" else fValue
                #print self.stc.lexer.IsInFunction()
                if self.stc.lexer.IsInFunction():
                    data = ErlangCache.ModuleFunctions(moduleName, onlyExported)
                else:
                    data += ErlangCache.ModuleExportedTypes(moduleName)
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
            elif fType == ErlangTokenType.MODULEATTR and fValue.startswith("-inc"):
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
        self._PrepareData(data)

    def _PrepareData(self, data):
        self.list.Clear()
        self.lastData = []
        for d in set(data):
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
                self.lastData.append(d)
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
            comment = fun.comment.replace("\n", "<br/>").replace("%", "")
            help = "{}({}) -> {}. <br/>".format(fun.name, ", ".join(p), res[0])
            if t:
                help += "Types:<br/>&nbsp;&nbsp;{}".format(",<br/>&nbsp;&nbsp;".join(t))
            if comment:
                help += "<br/>{}".format(comment)
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
        if prefix and prefix[-1] == ":" and prefix[-2] != ")":
            i = -2
            while abs(i) <= len(prefix) and (prefix[i].isalpha() or prefix[i].isdigit() or prefix[i] == "_"):
                module += prefix[i]
                i -= 1
            module = module[::-1]
            #print module
            if not module.islower():
                module = self.module
        else:
            module = self.module
       # print "show fun help", module, fun, arity
        data = ErlangCache.ModuleFunction(module, fun, arity)
        if not data:
            data = ErlangCache.ModuleExportedData(module, fun)
            if not data:
                data = ErlangCache.ModuleExportedData(module + ".hrl", fun)
                if not data:
                    data = ErlangCache.Bif(fun, arity)
                    if not data:
                        return
                    help = self._FunctionHelp(data)
                else:
                    help = self._ExportedTypeHelp(data)
            else:
                help = self._ExportedTypeHelp(data)
        else:
            help = self._FunctionHelp(data)
        #print "fun help", fun, prefix, pos,  help
        self.ShowHelp(help)
        file = data.moduleData.file if data.moduleData else None
        return (file, data.line)

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
        #print "macros help", macros,  help
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