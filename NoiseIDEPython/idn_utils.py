from threading import Thread, Event

__author__ = 'Yaroslav'

import os
import wx

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
            self.finished.wait(self.interval)
            if not self.finished.is_set():
                self.function()

class Menu(wx.Menu):
    def AppendMenuItem(self, text, handlerObject, handler):
        item = self.Append(wx.NewId(), text, text)
        handlerObject.Bind(wx.EVT_MENU, handler, item)
