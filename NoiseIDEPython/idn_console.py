from idn_colorschema import ColorSchema
from idn_erlang_completer import ErlangSimpleCompleter
from idn_erlangstc import ErlangConsoleSTC
from idn_events import Event
from idn_cache import ErlangCache
import core
from idn_highlight import ErlangHighlighter, ErlangHighlightType
from idn_notebook import ConsolePanel

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import re
import wx
import os
from idn_utils import CreateBitmapButton, extension, Menu
from idn_customstc import ConsoleSTC
import idn_connect as connect


class ErlangConsole(wx.Panel):
    def __init__(self, parent, cwd = os.getcwd(), params = []):
        wx.Panel.__init__(self, parent)

        self.DataReceivedEvent = Event()
        self.startButton = CreateBitmapButton(self, 'start_console.png', lambda e: self.Start())
        self.stopButton = CreateBitmapButton(self, 'stop_console.png', lambda e: self.Stop())
        self.clearButton = CreateBitmapButton(self, 'clear_console.png', lambda e: self.Clear())
        self.commandListButton = CreateBitmapButton(self, 'history_console.png', lambda e: self.ShowCommandHistory())

        self.stopButton.Enabled = False
        self.startButton.SetToolTip( wx.ToolTip("Start console") )
        self.stopButton.SetToolTip( wx.ToolTip("Stop console") )
        self.clearButton.SetToolTip( wx.ToolTip("Clear console output") )
        self.commandListButton.SetToolTip( wx.ToolTip("Command list") )

        self.buttonSizer = wx.BoxSizer(wx.VERTICAL)
        self.buttonSizer.Add(self.startButton)
        self.buttonSizer.Add(self.stopButton)
        self.buttonSizer.Add(self.clearButton)
        self.buttonSizer.AddStretchSpacer()
        self.buttonSizer.Add(self.commandListButton, 0, wx.ALIGN_BOTTOM)

        splitter = wx.SplitterWindow(self)
        self.consolePanel = ConsolePanel(splitter, ErlangConsoleSTC)
       # self.consolePanel.SetMinSize((400, 100))
        self.consoleOut = self.consolePanel.editor

        bottomPanel = wx.Panel(splitter)

        self.commandText = wx.TextCtrl(bottomPanel, wx.ID_ANY, size = (500, 25), style = wx.TE_MULTILINE | wx.TE_RICH)
        self.commandText.SetBackgroundColour(ColorSchema.codeEditor["background"])
        self.commandText.SetDoubleBuffered(True)
        self.commandButton = CreateBitmapButton(bottomPanel, 'exec_command.png', lambda e: self.Exec())
        self.commandButton.SetToolTip( wx.ToolTip("Exec command") )

        commandSizer = wx.BoxSizer(wx.HORIZONTAL)

        commandSizer.Add(self.commandText, 1, wx.EXPAND)
        commandSizer.AddSpacer(5)
        commandSizer.Add(self.commandButton, 0, wx.ALIGN_CENTER)

        # split the window

        bottomPanel.SetSizer(commandSizer)
        splitter.SplitHorizontally(self.consolePanel, bottomPanel, 300)
        splitter.SetSashGravity(1)
        splitter.SetMinimumPaneSize(25)

        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.buttonSizer, 1, wx.EXPAND)
        #mainSizer.Add(consoleSizer, 1, wx.EXPAND)
        mainSizer.Add(splitter, 1000, wx.EXPAND)
        self.SetSizer(mainSizer)
        self.Layout()

        self.commandText.Bind(wx.EVT_KEY_DOWN, self.OnCommandTextKeyDown)
        self.SetDefaultFontForCommandText()
        #font_name:  'courier new'
#font_size:  10
        #int pointSize, int family, int style, int weight, face
        self.commandText.Bind(wx.EVT_TEXT, self.OnTextChanged)

        self.CreateShell(cwd, params)
        self.promptRegexp = re.compile(r"(^|\(.*?\))\d*>\s*")
        self.recordRegexp = re.compile(r"#([a-z][a-z0-9A-Z_]*)\{")
        self.importedRecorModules = []
        self.lastCommands = []

        self.highlighter = ErlangHighlighter()
        self.completer = ErlangSimpleCompleter(self.commandText)

        formats = ColorSchema.LanguageFormats("erlang")
        self.typeToFormat = {
            ErlangHighlightType.DEFAULT: formats["default"],
            ErlangHighlightType.STRING: formats["string"],
            ErlangHighlightType.COMMENT: formats["comment"],
            ErlangHighlightType.ARROW: formats["arrow"],
            ErlangHighlightType.VAR: formats["variable"],
            ErlangHighlightType.MACROS: formats["macros"],
            ErlangHighlightType.ATOM: formats["atom"],
            ErlangHighlightType.MODULE: formats["module"],
            ErlangHighlightType.FUNCTION: formats["function"],
            ErlangHighlightType.KEYWORD: formats["keyword"],
            ErlangHighlightType.SPECIAL: formats["special"],
            ErlangHighlightType.MODULEATTR: formats["moduleattr"],
            ErlangHighlightType.RECORD: formats["record"],
            ErlangHighlightType.RECORDDEF: formats["record"],
            ErlangHighlightType.NUMBER: formats["number"],
            ErlangHighlightType.FUNDEC: formats["fundec"],
            ErlangHighlightType.BRACKET: formats["bracket"],
            ErlangHighlightType.BIF: formats["bif"]
        }
        self.defaultFormat = formats["default"]
        self.consolePanel.editor.Bind(wx.EVT_KEY_DOWN, self.OnEditorKeyDown)



    def SetDefaultFontForCommandText(self):
        font = wx.Font(pointSize = int(ColorSchema.codeEditor["command_text_font_size"]),
            family = wx.FONTFAMILY_DEFAULT,
            style = wx.FONTSTYLE_NORMAL,
            weight = wx.FONTWEIGHT_NORMAL,
            face = ColorSchema.codeEditor["command_text_font_name"])
        self.commandText.SetFont(font)

    def OnEditorKeyDown(self, event):
        event.Skip()
        if event.GetKeyCode() in [wx.WXK_SHIFT, wx.WXK_CONTROL] or event.ControlDown() or event.AltDown():
            return
        try:
            ch = chr(event.GetKeyCode())
            if ch.isalpha() or ch.isdigit() or ch.isspace():
                if event.ShiftDown(): ch = ch.upper()
                else: ch = ch.lower()
                self.commandText.AppendText(ch)
            self.commandText.SetFocus()
        except Exception, e:
            core.Log("OnEditorKeyDown", e)

    def ShowCommandHistory(self):
        menu = Menu()

        def putcmd(cmd):
            def onclick(e):
                self.commandText.Clear()
                self.commandText.WriteText(cmd)
            return onclick

        for cmd in self.lastCommands[:20]:
            menu.AppendMenuItem(cmd, self, putcmd(cmd))
        self.PopupMenu(menu)


    def Start(self):
        self.Clear()
        self.shell.Start()
        self.startButton.Enabled = False
        self.stopButton.Enabled = True
        self.importedRecorModules = []

    def Stop(self, kill = False):
        noLog = wx.LogNull()
        try:
            self.shell.Stop()
            if not kill:
                self.WriteToConsoleOut("\n\nSTOPPED\n\n")
                self.startButton.Enabled = True
                self.stopButton.Enabled = False
        except Exception, e:
            core.Log(e)
        finally:
            del noLog

    def Clear(self):
        self.consoleOut.Clear()

    def Exec(self, cmd = None):
        if not cmd:
            cmd = self.commandText.GetValue()
        if cmd:
            lastCommands = list(filter(lambda x: x != cmd, self.lastCommands))
            lastCommands.append(cmd)
            self.lastCommands = lastCommands
            self.ImportRecords(cmd)
            self.shell.SendCommandToProcess(cmd)

        self.commandText.Clear()
        self.WriteToConsoleOut(cmd + '\n')

    def ImportRecords(self, cmd):
        recordNames = re.findall(self.recordRegexp, cmd)
        for recordName in recordNames:
            for record in ErlangCache.AllRecords():
                if record.name == recordName:
                    app = core.Project.GetApp(record.file)
                    includeStr = app + "/include/" + os.path.basename(record.file)
                    if includeStr in self.importedRecorModules:
                        continue
                    self.shell.SendCommandToProcess("rr(\"" + includeStr +"\").")
                    self.importedRecorModules.append(includeStr)

    def OnCommandTextKeyDown(self, event):
        if self.completer.IsShown():
            self.UpdateCompleter()

        keyCode = event.GetKeyCode()
        if (self.completer.IsShown() and
            keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER,
                        wx.WXK_DOWN, wx.WXK_UP, wx.WXK_ESCAPE]):
            self.completer.OnKeyDown(event)
            return
        elif keyCode == wx.WXK_ESCAPE:
            self.commandText.Clear()
        elif event.GetModifiers() == wx.MOD_CONTROL and self.lastCommands and keyCode == wx.WXK_UP:
            newText = self.lastCommands[-1]
            self.commandText.Clear()
            self.commandText.WriteText(newText)
            self.lastCommands = self.lastCommands[:-1]
            self.lastCommands.insert(0, newText)
        elif event.GetModifiers() == wx.MOD_CONTROL and self.lastCommands and keyCode == wx.WXK_DOWN:
            newText = self.lastCommands[0]
            self.commandText.Clear()
            self.commandText.WriteText(newText)
            self.lastCommands = self.lastCommands[1:]
            self.lastCommands.append(newText)
        elif keyCode in [wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER] and event.GetModifiers() == wx.MOD_ALT:
            editor = core.TabMgr.GetActiveEditor()
            if editor: editor.SetFocus()
        elif event.GetModifiers() == wx.MOD_CONTROL and keyCode == wx.WXK_SPACE:
            self.UpdateCompleter()
            self.completer.Show()
        elif keyCode == wx.WXK_RETURN:
            if event.ControlDown():
                self.commandText.AppendText("\n")
            else:
                self.Exec()
        else:
            event.Skip()
        #self.OnTextChanged()

    def UpdateCompleter(self):
        text = self.commandText.Value[:self.commandText.GetInsertionPoint()]
        self.completer.Update(text)
        x, y = self.commandText.PositionToXY(self.commandText.GetInsertionPoint())
        self.completer.UpdateCompleterPosition(wx.Point(x * (int(ColorSchema.codeEditor["command_text_font_size"]) - 3),y))

    def OnTextChanged(self, event = None):
        event.Skip()
        text = self.commandText.Value
        tokens = self.highlighter.GetHighlightingTokens(text)
        for token in tokens:
            self.commandText.SetStyle(token.start, token.start + len(token.value), self._TokenTypeToTextAttr(token.type))

    def _TokenTypeToTextAttr(self, type):
        format = self.typeToFormat.get(type, self.defaultFormat)
        color = format[5:12]
        attr = wx.TextAttr(colText = color)
        return attr

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcess(cwd, params)
        self.shell.DataReceivedEvent += self.WriteToConsoleOut

    def WriteToConsoleOut(self, text):
        text = "\n".join([re.sub(self.promptRegexp, "", line) for line in text.split("\n")])
        self.DataReceivedEvent(text)
        self.consoleOut.Append(text)

    def SetParams(self, params):
        self.shell.SetParams(params)

    def SetCWD(self, cwd):
        self.shell.SetCWD(cwd)


class ErlangProjectConsole(ErlangConsole):
    def __init__(self, parent, cwd, params):
        ErlangConsole.__init__(self, parent, cwd, params)

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcessWithClientConnection(cwd, params)
        self.shell.DataReceivedEvent += self.WriteToConsoleOut

    def SetStartCommand(self, command):
        self.startCommand = command

    def Start(self):
        ErlangConsole.Start(self)
        if self.startCommand:
            self.shell.SendCommandToProcess(self.startCommand)
            self.WriteToConsoleOut(self.startCommand + '\n')

class ErlangIDEConsole(ErlangConsole):
    def __init__(self, parent, cwd):
        ErlangConsole.__init__(self, parent, cwd)
        self.buttonSizer.Hide(self.startButton)
        self.buttonSizer.Hide(self.stopButton)

        self.Start()

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcessWithConnection(cwd)
        self.shell.DataReceivedEvent += self.WriteToConsoleAndLog

    def WriteToConsoleAndLog(self, text):
        text = "\n".join([re.sub(self.promptRegexp, "", line) for line in text.split("\n")])
        self.DataReceivedEvent(text)
        self.consoleOut.Append(text)
        core.Log(text)
