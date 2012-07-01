__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
from idn_customstc import ConsoleSTC
import idn_connect as connect

class ErlangConsole(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)

        self.runButton = wx.Button(self, wx.NewId(), label = "Start")
        self.stopButton = wx.Button(self, wx.NewId(), label = "Stop")
        self.clearButton = wx.Button(self, wx.NewId(), label = "Clear")

        self.buttonSizer = wx.BoxSizer(wx.VERTICAL)
        self.buttonSizer.Add(self.runButton)
        self.buttonSizer.Add(self.stopButton)
        self.buttonSizer.Add(self.clearButton)
        self.buttonSizer.AddStretchSpacer()

        self.consoleOut = ConsoleSTC(self)#, size = (800, 300))
        #self.consoleOut.SetSize((700, 500))
        self.commandText = wx.TextCtrl(self, wx.NewId())
        self.commandButton = wx.Button(self, wx.NewId(), label = "Exec")

        commandSizer = wx.BoxSizer(wx.HORIZONTAL)
        commandSizer.Add(self.commandText, 1, wx.EXPAND)
        commandSizer.AddSpacer(5)
        commandSizer.Add(self.commandButton)

        consoleSizer = wx.BoxSizer(wx.VERTICAL)
        consoleSizer.Add(self.consoleOut, 1, wx.EXPAND | wx.BOTTOM)
        consoleSizer.AddSpacer(5)
        consoleSizer.Add(commandSizer, 0, wx.EXPAND | wx.RIGHT)

        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.buttonSizer)
        mainSizer.Add(consoleSizer, 1, wx.EXPAND)
        self.SetSizer(mainSizer)
        self.Layout()

        self.commandButton.Bind(wx.EVT_BUTTON, self.OnExec)

        self.shell = None

    def Start(self):
        if self.shell:
            self.shell.Start()

    def Stop(self):
        if self.shell:
            self.shell.Stop()

    def Clear(self):
        self.consoleOut.ClearAll()

    def OnExec(self, event):
        cmd = self.commandText.GetValue()
        if cmd:
            self.shell.ExecCommand(cmd)
        self.commandText.SetValue("")

class ErlangIDEConsole(ErlangConsole):
    def __init__(self, parent):
        ErlangConsole.__init__(self, parent)
        self.buttonSizer.Hide(self.runButton)
        self.buttonSizer.Hide(self.stopButton)

        #self.shell = connect.ErlangSubprocessWithSocketConnection()
        #self.shell.SetOutputHandler(self.WriteToConsoleOut)
        #self.shell.SetInputHandler(self.WriteToConsoleOut)
        #print self.shell.outputHandler
        self.shell = connect.ErlangProcessWithConnection()
        self.shell.SetOutputHandler(self.WriteToConsoleOut)
        self.shell.SetSocketHandler(self.WriteToConsoleOut)
        self.Start()
        #self.shell.Start()
        #self.shell.ExecCommand(u'[io:format("~p~n", [V]) || V <- lists:seq(1, 4)].')

    def WriteToConsoleOut(self, text):
        #print "WriteToConsoleOut ", text, "end"
        self.consoleOut.SetReadOnly(False)
        self.consoleOut.AddText(text)
        self.consoleOut.SetReadOnly(True)
