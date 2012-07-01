import io

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import socket
import os
import time
import subprocess as sub
from threading import Thread, Event
from Queue import Queue

class SubprocessReaderThread(Thread):
    def __init__(self, source):
        Thread.__init__(self)
        self.setDaemon(True)
        self.source = source
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            #print "check input"
            output = self.source.readline()
            if output:
                print output
            time.sleep(0.2)

    def Stop(self):
        self.active.clear()
        self.join()

class SubprocessWriterThread(Thread):
    def __init__(self, process, queue):
        Thread.__init__(self)
        self.setDaemon(True)
        self.process = process
        self.queue = queue
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            if not self.queue.empty():
                inCommand = self.queue.get()
                if inCommand:
                    self.process.stdin.write(inCommand + '\n')
                    self.process.stdin.flush()

    def Stop(self):
        self.active.clear()
        self.join()

class ErlangSubprocess:
    def __init__(self, cwd, params):
        self.outputHandler = self.DefaultOutputHandler
        #self.active = Event()
        erlang = ideConfig.LanguageExec("erlang")
        self.cmd = "{} {}".format(erlang, ' '.join(params))
        self.commandQueue = Queue()
        #self.setDaemon(True)
        print erlang

    def Start(self):
        buffer = io.StringIO()
        self.process = sub.Popen(self.cmd, stdin = sub.PIPE, bufsize = 1)#, stdout = sub.PIPE)
        self.process.stdout = buffer
        #self.process.stdin = buffer
        self.reader = SubprocessReaderThread(self.process.stdout)
        self.reader.start()
        self.writer = SubprocessWriterThread(self.process, self.commandQueue)
        self.writer.start()
        #self.process.communicate(u'[io:format("~p~n", [V]) || V <- lists:seq(1, 1000)].')


    def Stop(self):
        self.reader.Stop()
        self.writer.Stop()
        self.process.kill()

    def ExecCommand(self, cmd):
        self.commandQueue.put_nowait(cmd)

    def SetOutputHandler(self, handler):
        self.outputHandler = handler

    def DefaultOutputHandler(self, line):
        print line


class ErlangConnection:
    def __init__(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        pass

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        return 55555

    def Start(self):
        self.socket.connect((self.Host(), self.Port()))

