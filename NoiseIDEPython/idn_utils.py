__author__ = 'Yaroslav'

from threading import Thread, Event
from idn_global import GetMainFrame

import os
import wx

def readFile(file):
    f = open(file)
    data = f.read()
    f.close()
    return data

def writeFile(file, data):
    f = open(file, 'w')
    f.write(data)
    f.flush()
    f.close()

def extension(path):
    name, ext = os.path.splitext(path)
    return ext

class Timer(Thread):
    def __init__(self, interval, function):
        Thread.__init__(self)
        self.setDaemon(True)
        self.interval = interval
        self.function = function
        self.finished = Event()

    def Start(self):
        self.start()

    def Stop(self):
        if self.isAlive():
            self.finished.set()

    def run(self):
        while not self.finished.is_set():
            self.function()
            self.finished.wait(self.interval)

def CreateButton(parent, label, handler, style = 0):
    button = wx.Button(parent, label = label, style = style)
    button.Bind(wx.EVT_BUTTON, handler)
    return button

def GetImagePath(image):
    return os.path.join(GetMainFrame().cwd, "data", "images", image)

def GetImage(image):
    path = GetImagePath(image)
    return wx.Bitmap(path, wx.BITMAP_TYPE_PNG)

def CreateBitmapButton(parent, image, handler, drawBorder = True):
    if drawBorder:
        button = wx.BitmapButton(parent, wx.NewId(), bitmap = wx.Bitmap(GetImagePath(image)))
    else:
        button = wx.BitmapButton(parent, wx.NewId(), bitmap = wx.Bitmap(GetImagePath(image)), style = wx.BORDER_NONE)

    button.Bind(wx.EVT_BUTTON, handler)
    return button

def CreateLabel(parent, text):
    return wx.StaticText(parent, label = text)

class Menu(wx.Menu):
    def AppendMenuItem(self, text, handlerObject, handler):
        item = self.Append(wx.NewId(), text, text)
        handlerObject.Connect(item.GetId(), -1, wx.wxEVT_COMMAND_MENU_SELECTED, handler)
        return item

    def AppendCheckMenuItem(self, text, handlerObject, handler, check = False):
        item = self.Append(wx.NewId(), text, text, wx.ITEM_CHECK)
        self.Check(item.Id, check)
        handlerObject.Bind(wx.EVT_MENU, handler, item)
