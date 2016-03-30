import re

__author__ = 'Yaroslav'

from threading import Thread, Event
import core

import os
import wx
import codecs
import sys

def decode(name):
    return name.decode(sys.getfilesystemencoding());

def readFile(fileName):
    f = codecs.open(fileName, "r", "utf-8")
    data = f.read()
    f.close()
    return data

def writeFile(fileName, data):
    f = codecs.open(fileName,'w','utf-8')
    f.write(data)
    f.flush()
    f.close()

def writeBinaryFile(fileName, data):
    f = open(fileName, 'wb')
    f.write(data)
    f.flush()
    f.close()

def camelToLowerUnderscore(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def underscoreToCamelcase(value):
    return re.sub(r'\w+', lambda m:m.group(0).capitalize(), value)

def extension(path):
    name, ext = os.path.splitext(path)
    return ext

def erlstr(pystr):
    if not pystr: return ""
    return pystr.replace(os.sep, "/")

def pystr(erlstr):
    if not erlstr: return ""
    erlstr = os.path.normpath(erlstr)
    return erlstr.replace("/", os.sep)

def GetAllFilesInDir(path, fileExts = None):
    files = []
    for root, _, fileNames in os.walk(path):
        for fileName in fileNames:
            fp = os.path.join(root, fileName)
            if fileExts and not any([fp.endswith(fm) for fm in fileExts]):
                continue
            files.append(fp)
    return files

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
    return os.path.join(core.MainFrame.cwd, "data", "images", image)

def GetImage(image):
    path = GetImagePath(image)
    return wx.Bitmap(path, wx.BITMAP_TYPE_PNG)

def CreateBitmapButton(parent, image, handler, drawBorder = True):
    if drawBorder:
        button = wx.BitmapButton(parent, wx.ID_ANY, bitmap = wx.Bitmap(GetImagePath(image)))
    else:
        button = wx.BitmapButton(parent, wx.ID_ANY, bitmap = wx.Bitmap(GetImagePath(image)), style = wx.BORDER_NONE)

    button.Bind(wx.EVT_BUTTON, handler)
    return button

def CreateLabel(parent, text):
    return wx.StaticText(parent, label = text)

class Menu(wx.Menu):
    def AppendMenuItem(self, text, handlerObject, handler, shortcut = None):
        title = text
        if shortcut:
            title += "\t" + shortcut
        item = self.Append(wx.ID_ANY, title, text)
        handlerObject.Connect(item.GetId(), -1, wx.wxEVT_COMMAND_MENU_SELECTED, handler)

        return item

    def AppendCheckMenuItem(self, text, handlerObject, handler, check = False):
        item = self.Append(wx.ID_ANY, text, text, wx.ITEM_CHECK)
        item.Check(check)
        handlerObject.Bind(wx.EVT_MENU, handler, item)
        return item
