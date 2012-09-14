from idn_global import Log
from idn_highlight import ErlangHighlighter
from idn_notebook import ConsolePanel

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import re
import wx
import os
from idn_utils import CreateBitmapButton
from idn_customstc import ConsoleSTC
import idn_connect as connect



class ErlangConsole(wx.Panel):
    def __init__(self, parent, cwd = os.getcwd(), params = []):
        wx.Panel.__init__(self, parent)

        self.startButton = CreateBitmapButton(self, 'start_console.png', lambda e: self.Start())
        self.stopButton = CreateBitmapButton(self, 'stop_console.png', lambda e: self.Stop())
        self.clearButton = CreateBitmapButton(self, 'clear_console.png', lambda e: self.Clear())
        self.stopButton.Enabled = False
        self.startButton.SetToolTip( wx.ToolTip("Start console") )
        self.stopButton.SetToolTip( wx.ToolTip("Stop console") )
        self.clearButton.SetToolTip( wx.ToolTip("Clear console output") )

        self.buttonSizer = wx.BoxSizer(wx.VERTICAL)
        self.buttonSizer.Add(self.startButton)
        self.buttonSizer.Add(self.stopButton)
        self.buttonSizer.Add(self.clearButton)
        self.buttonSizer.AddStretchSpacer()

        splitter = wx.SplitterWindow(self)
        self.consolePanel = ConsolePanel(splitter)
       # self.consolePanel.SetMinSize((400, 100))
        self.consoleOut = self.consolePanel.editor

        bottomPanel = wx.Panel(splitter)
        self.commandText = wx.TextCtrl(bottomPanel, wx.NewId(), size = (500, 25), style = wx.TE_MULTILINE)# | wx.TE_RICH)
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

        #sizer = wx.BoxSizer(wx.VERTICAL)
        #sizer.Add(splitter, 1, wx.EXPAND)

#        consoleSizer = wx.BoxSizer(wx.VERTICAL)
#        consoleSizer.Add(self.consolePanel, 1, wx.EXPAND | wx.BOTTOM)
#        consoleSizer.AddSpacer(5)
#        consoleSizer.Add(commandSizer, 0, wx.EXPAND | wx.RIGHT)
#        consoleSizer.AddSpacer(5)

        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.buttonSizer)
        #mainSizer.Add(consoleSizer, 1, wx.EXPAND)
        mainSizer.Add(splitter, 1, wx.EXPAND)
        self.SetSizer(mainSizer)
        self.Layout()

        self.commandText.Bind(wx.EVT_KEY_DOWN, self.OnCommandTextKeyDown)
        #self.commandText.Bind(wx.EVT_TEXT, self.OnTextChanged)

        self.CreateShell(cwd, params)
        self.promptRegexp = re.compile(r"(^|\(.*?\))\d*>")

        self.lastCommands = []

        self.highlighter = ErlangHighlighter()

    def Start(self):
        self.Clear()
        self.shell.Start()
        self.startButton.Enabled = False
        self.stopButton.Enabled = True

    def Stop(self):
        try:
            self.shell.Stop()
        except Exception, e:
            Log(e)
        finally:
            self.WriteToConsoleOut("\n\nSTOPPED\n\n")
            self.startButton.Enabled = True
            self.stopButton.Enabled = False

    def Clear(self):
        self.consoleOut.SetReadOnly(False)
        self.consoleOut.ClearAll()
        self.consoleOut.SetReadOnly(True)

    def Exec(self):
        cmd = self.commandText.GetValue()
        if cmd:
            lastCommands = list(filter(lambda x: x != cmd, self.lastCommands))
            lastCommands.append(cmd)
            self.lastCommands = lastCommands
            self.shell.SendCommandToProcess(cmd)
        self.commandText.SetValue("")
        self.commandText.SetInsertionPoint(0)
        self.WriteToConsoleOut(cmd + '\n')

    def OnCommandTextKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_RETURN:
            if event.ControlDown():
                self.commandText.AppendText("\n")
            else:
                self.Exec()
        elif keyCode == wx.WXK_ESCAPE:
            self.commandText.SetValue("")
        elif event.ControlDown() and self.lastCommands and keyCode == wx.WXK_UP:
            newText = self.lastCommands[-1]
            self.commandText.SetValue(newText)
            self.lastCommands = self.lastCommands[:-1]
            self.lastCommands.insert(0, newText)
        elif event.ControlDown() and self.lastCommands and keyCode == wx.WXK_DOWN:
            newText = self.lastCommands[0]
            self.commandText.SetValue(newText)
            self.lastCommands = self.lastCommands[1:]
            self.lastCommands.append(newText)
        else:
            event.Skip()

    def OnTextChanged(self, event):
        event.Skip()
        text = self.commandText.Value
        tokens = self.highlighter.GetHighlightingTokens(text)
        for token in tokens:
            self.commandText.SetStyle(0, len(text), None)
            self.commandText.SetStyle(token.start, len(token.value), token.type)

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcess(cwd, params)
        self.shell.SetOutputHandler(self.WriteToConsoleOut)

    def WriteToConsoleOut(self, text):
        text = "\n".join([re.sub(self.promptRegexp, "", line) for line in text.split("\n")])
        self.consoleOut.Append(text)

    def SetParams(self, params):
        self.shell.SetParams(params)


class ErlangProjectConsole(ErlangConsole):
    def __init__(self, parent, cwd, params):
        ErlangConsole.__init__(self, parent, cwd, params)

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcess(cwd, params)
        self.shell.SetOutputHandler(self.WriteToConsoleOut)

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
        self.shell.SetOutputHandler(self.WriteToConsoleOut)


