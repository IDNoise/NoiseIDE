__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx

MainFrame = None

def GetProject():
    return MainFrame.project

def GetMainFrame():
    return MainFrame

def GetTabMgr():
    return MainFrame.TabMgr

def GetToolMgr():
    return MainFrame.ToolMgr

def Log(*text):
    MainFrame.log.SetReadOnly(False)
    MainFrame.log.AppendText(" ".join([str(t) for t in text]) + "\n")
    MainFrame.log.SetReadOnly(True)
    MainFrame.log.ScrollToLine(MainFrame.log.GetLineCount())