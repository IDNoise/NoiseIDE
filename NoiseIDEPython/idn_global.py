__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx

MainFrame = None

def GetProject():
    return MainFrame.project

def GetMainFrame():
    return MainFrame

def GetTabMgr():
    return MainFrame.TabMgr