__author__ = 'Yaroslav Nikityshev aka IDNoise'

import wx
import os.path

App = None
MainFrame = None
Project = None
TabMgr = None
ToolMgr = None
WinMgr = None
logFile = None
dataDir = None
tempDir = None
userDataDir = None

def Init():
    global logFile, dataDir, tempDir, userDataDir
    sp = wx.StandardPaths.Get()
    dataDir = os.path.join(sp.GetUserConfigDir(), 'noiseide')
    tempDir = os.path.join(sp.GetTempDir(), 'noiseide')
    userDataDir = os.path.join(dataDir, 'userdata')
    logFile = open(os.path.join(dataDir, "ide.log"), 'w')

def DataDir():
    return dataDir

def TempDir():
    return tempDir

def UserDataDir():
    return userDataDir

def Log(*text):
    #print text
    text = " ".join([str(t) for t in text])
    try:
        print text
    except Exception, e:
        pass
    if not text.endswith("\n"):
        text += "\n"
    logFile.write(text)
    logFile.flush()