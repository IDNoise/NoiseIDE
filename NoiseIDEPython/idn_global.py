__author__ = 'Yaroslav Nikityshev aka IDNoise'

MainFrame = None

def GetProject():
    return MainFrame.project

def GetMainFrame():
    return MainFrame

def GetTabMgr():
    return MainFrame.TabMgr

def GetToolMgr():
    return MainFrame.ToolMgr

def GetWinMgr():
    return MainFrame.WinMgr

def Log(*text):
    text = " ".join([str(t) for t in text])
    MainFrame.Log(text)