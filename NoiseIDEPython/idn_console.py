import re

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
import os
from idn_customstc import ConsoleSTC
import idn_connect as connect

class ErlangConsole(wx.Panel):
    def __init__(self, parent, cwd = os.getcwd(), params = []):
        wx.Panel.__init__(self, parent)

        self.startButton = wx.BitmapButton(self, wx.NewId(), bitmap = wx.Bitmap('data/images/start_console.png'))
        self.stopButton = wx.BitmapButton(self, wx.NewId(), bitmap = wx.Bitmap('data/images/stop_console.png'))
        self.clearButton = wx.BitmapButton(self, wx.NewId(), bitmap = wx.Bitmap('data/images/clear_console.png'))
        self.stopButton.Enabled = False
        self.startButton.SetToolTip( wx.ToolTip("Start console") )
        self.stopButton.SetToolTip( wx.ToolTip("Stop console") )
        self.clearButton.SetToolTip( wx.ToolTip("Clear console output") )

        self.buttonSizer = wx.BoxSizer(wx.VERTICAL)
        self.buttonSizer.Add(self.startButton)
        self.buttonSizer.Add(self.stopButton)
        self.buttonSizer.Add(self.clearButton)
        self.buttonSizer.AddStretchSpacer()

        self.consoleOut = ConsoleSTC(self)
        self.commandText = wx.TextCtrl(self, wx.NewId())
        self.commandButton = wx.BitmapButton(self, wx.NewId(), bitmap = wx.Bitmap('data/images/exec_command.png'))
        self.commandButton.SetToolTip( wx.ToolTip("Exec command") )

        commandSizer = wx.BoxSizer(wx.HORIZONTAL)
        commandSizer.Add(self.commandText, 1, wx.EXPAND)
        commandSizer.AddSpacer(5)
        commandSizer.Add(self.commandButton)

        consoleSizer = wx.BoxSizer(wx.VERTICAL)
        consoleSizer.Add(self.consoleOut, 1, wx.EXPAND | wx.BOTTOM)
        consoleSizer.AddSpacer(5)
        consoleSizer.Add(commandSizer, 0, wx.EXPAND | wx.RIGHT)
        consoleSizer.AddSpacer(5)

        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.buttonSizer)
        mainSizer.Add(consoleSizer, 1, wx.EXPAND)
        self.SetSizer(mainSizer)
        self.Layout()

        self.commandButton.Bind(wx.EVT_BUTTON, lambda e: self.Exec())
        self.startButton.Bind(wx.EVT_BUTTON, lambda e: self.Start())
        self.stopButton.Bind(wx.EVT_BUTTON, lambda e: self.Stop())
        self.clearButton.Bind(wx.EVT_BUTTON, lambda e: self.Clear())
        self.commandText.Bind(wx.EVT_KEY_UP, self.OnCommandTextKeyUp)

        self.CreateShell(cwd, params)
        self.promptRegexp = re.compile(r"^\s*(\([\S]*\))?\d*>\s*")

    def Start(self):
        self.Clear()
        self.shell.Start()
        self.startButton.Enabled = False
        self.stopButton.Enabled = True

    def Stop(self):
        try:
            self.shell.Stop()
        except Exception, e:
            print e
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
            self.shell.SendCommandToProcess(cmd)
        self.commandText.SetValue("")
        self.WriteToConsoleOut(cmd + '\n')

    def OnCommandTextKeyUp(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_RETURN:
            self.Exec()
        elif keyCode == wx.WXK_ESCAPE:
            self.commandText.SetValue("")
        else:
            event.Skip()

    def CreateShell(self, cwd, params):
        self.shell = connect.ErlangProcess(cwd, params)
        self.shell.SetOutputHandler(self.WriteToConsoleOut)

    def WriteToConsoleOut(self, text):
        text = "\n".join([re.sub(self.promptRegexp, "", line) for line in text.split("\n")])
        maxLine = self.consoleOut.GetLineCount()
        self.consoleOut.SetReadOnly(False)
        self.consoleOut.AppendText(text)
        self.consoleOut.SetReadOnly(True)
        if self.GetLastVisibleLine() >= maxLine:
            self.consoleOut.ScrollToLine(self.consoleOut.GetLineCount())

    def GetLastVisibleLine(self):
        """
        return the last visible line on the screen,
        taking into consideration the folded lines
        """
        return self.consoleOut.LineFromPosition(
            self.consoleOut.PositionFromPoint(
                wx.Point(self.consoleOut.GetPosition()[0],
                    self.consoleOut.GetPosition()[1] + self.consoleOut.GetSize()[1]))
        )

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


