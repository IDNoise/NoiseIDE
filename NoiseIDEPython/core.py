__author__ = 'Yaroslav Nikityshev aka IDNoise'

MainFrame = None
Project = None
TabMgr = None
ToolMgr = None
WinMgr = None
logFile = open("ide.log", 'w')

def Log(*text):
    text = " ".join([str(t) for t in text])
    try:
        print text
    except Exception, e:
        pass
    if not text.endswith("\n"):
        text += "\n"
    logFile.write(text)
    logFile.flush()