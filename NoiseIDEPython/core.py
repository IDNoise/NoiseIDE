__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import tempfile
import sys

APPNAME = 'noiseide'


App = None
MainFrame = None
Project = None
TabMgr = None
ToolMgr = None
WinMgr = None

if sys.platform == 'win32':
    dataDir = os.path.join(os.getenv('APPDATA'), APPNAME)
else:
    dataDir = os.path.expanduser(os.path.join("~", "." + APPNAME))

if not os.path.exists(dataDir):
    os.makedirs(dataDir)

logFile = open(os.path.join(dataDir, "ide.log"), 'w')

tempDir = os.path.join(tempfile.gettempdir(), 'noiseide')
userDataDir = os.path.join(dataDir, 'userdata')

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
        if not text.endswith("\n"):
            text += "\n"
        logFile.write(text)
        logFile.flush()
    except Exception, e:
        pass
