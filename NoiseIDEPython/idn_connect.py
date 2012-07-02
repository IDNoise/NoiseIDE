import asyncore
import random
import wx

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import io
import socket
import os
import time
import struct
import subprocess as sub
from Queue import Queue
from idn_config import Config
from threading import Thread, Event
from wx import Process, Execute

class AsyncoreThread(Thread):
    def __init__(self):
        Thread.__init__(self)
        self.active = Event()

    def Start(self):
        self.active.set()
        self.start()

    def Stop(self):
        self.active.clear()

    def run(self):
        while self.active.is_set():
            asyncore.poll(0.1)

class ErlangSocketConnection(asyncore.dispatcher):
    def __init__(self):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socketQueue = Queue()
        self.socketHandler = None
        self.port = self.Port()

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        return random.randrange(55555, 55600)

    def Start(self):
        self.connect((self.Host(), self.port))
        self.asyncoreThread = AsyncoreThread()
        self.asyncoreThread.Start()

    def Stop(self):
        self.asyncoreThread.Stop()
        self.close()

    def handle_connect(self):
        #print "connect"
        self.OnConnect()

    def writable(self):
        return not self.socketQueue.empty()

    def handle_close(self):
        #print "close"
        self.close()

    def handle_write(self):
        #print 'handle_write'
        cmd = self.socketQueue.get()
        if cmd:
            msgLen = struct.pack('>H', len(cmd))
            msg = msgLen + cmd
            #print msg
            self.send(msg)

    def handle_read(self):
        #print 'handle_read'
        recv = self.socket.recv(2)
        if recv:
            msgLen = struct.unpack('>H', recv)[0]
            data = self.socket.recv(msgLen)
            print data
            if self.socketHandler:
                self.socketHandler(data)

    def SetSocketHandler(self, handler):
        self.socketHandler = handler

    def _ExecRequest(self, action, data):
        request = '{' + '"action": "{}", "data": {}'.format(action, data) + '}'
        self.socketQueue.put_nowait(request)

    def CompileFile(self, file):
        self._ExecRequest("compile_file", '"{}"'.format(file))

    def Rpc(self, module, fun):
        self._ExecRequest("rpc", '["{0}", "{1}"]'.format(module, fun))

    def SetProp(self, prop, value):
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, erlstr(value)))

    def RemoveProp(self, prop):
        self._ExecRequest("remove_prop", '"{}"'.format(prop))

    def OnConnect(self):
        pass

class ErlangProcess(Process):
    def __init__(self, cwd = os.getcwd(), params = []):
        Process.__init__(self)
        self.Redirect()
        self.cwd = cwd
        erlang = Config.LanguageExec("erlang")
        self.cmd = "{} {}".format(erlang, ' '.join(params + ["-s reloader"]))
        self.pid = None
        self.handler = None
        self.processQueue = Queue()

        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.OnTimer)

    def SendCommandToProcess(self, cmd):
        cmd += '\n'
        #if self.handler:
        #    self.handler(cmd)
        self.outputStream.write(cmd)
        self.outputStream.flush()

    def Start(self):
        cwd = os.getcwd()
        os.chdir(self.cwd)
        self.pid = wx.Execute(self.cmd, wx.EXEC_ASYNC | wx.EXEC_HIDE_CONSOLE, self)
        os.chdir(cwd)
        self.timer.Start(20)
        self.inputStream = self.GetInputStream()
        self.outputStream = self.GetOutputStream()
        #self.SendCommandToProcess("reloader:start().")

    def OnTimer(self, event = None):
        if  self.inputStream.CanRead():
            text =  self.inputStream.read()
            if self.handler:
                self.handler(text)

    def SetOutputHandler(self, handler):
        self.handler = handler

    def Stop(self):
        if self.timer:
            self.timer.Stop()
        result = Process.Kill(self.pid, wx.SIGTERM)
        if result not in [wx.KILL_OK, wx.KILL_NO_PROCESS]:
            Process.Kill(self.pid, wx.SIGKILL)

    def OnTerminate(self, *args, **kwargs):
        if self.timer:
            self.timer.Stop()

class ErlangProcessWithConnection(ErlangProcess, ErlangSocketConnection):
    def __init__(self):
        ErlangProcess.__init__(self, os.path.join(os.getcwd(), 'data', 'erlang', 'modules'))
        ErlangSocketConnection.__init__(self)

    def Start(self):
        ErlangProcess.Start(self)
        self.SendCommandToProcess("eide_connect:start({}).".format(self.port))
        ErlangSocketConnection.Start(self)

    def Stop(self):
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)


def erlstr(str):
    return str.replace("\\", "/")
