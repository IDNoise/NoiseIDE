from threading import Thread, Event

__author__ = 'Yaroslav'

import os

def extension(path):
    name, ext = os.path.splitext(path)
    return ext

class Timer(Thread):
    def __init__(self, interval, function):
        Thread.__init__(self)
        self.interval = interval
        self.function = function
        self.finished = Event()

    def Start(self):
        self.start()

    def Cancel(self):
        self.finished.set()

    def run(self):
        while not self.finished.is_set():
            self.finished.wait(self.interval)
            if not self.finished.is_set():
                self.function()