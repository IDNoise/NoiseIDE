__author__ = 'Yaroslav Nikityshev aka IDNoise'

MainFrame = None
Project = None
TabMgr = None
ToolMgr = None
WinMgr = None
logFile = open("ide.log", 'w')

def Log(*text):
    text = " ".join([str(t) for t in text])
    print text
    logFile.write(text)
    logFile.flush()