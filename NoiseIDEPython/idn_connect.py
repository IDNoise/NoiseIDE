import asyncore
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

class SubprocessReaderThread(Thread):
    def __init__(self, source, handler):
        Thread.__init__(self)
        self.setDaemon(True)
        self.source = source
        self.handler = handler
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            print "check output"
            output = self.source.readline()
            if output and self.handler:
                self.handler(output)
            time.sleep(0.1)

    def Stop(self):
        self.active.clear()
        self.join()

class SubprocessWriterThread(Thread):
    def __init__(self, process, queue, handler):
        Thread.__init__(self)
        self.setDaemon(True)
        self.process = process
        self.queue = queue
        self.handler = handler
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            print "check input"
            if not self.queue.empty():
                inCommand = self.queue.get()
                if inCommand:
                    inCommand = inCommand + '\n'
                    if self.handler:
                        self.handler(inCommand)
                    self.process.stdin.write(unicode(inCommand))
                    self.process.stdin.flush()
            time.sleep(0.1)

    def Stop(self):
        self.active.clear()
        self.join()

class ErlangSubprocess:
    def __init__(self, cwd, params):
        self.outputHandler = self.DefaultProcessHandler
        self.inputHandler = self.DefaultProcessHandler
        self.cwd = cwd
        erlang = Config.LanguageExec("erlang")
        self.cmd = "{} {}".format(erlang, ' '.join(params))
        self.procCommandQueue = Queue()

    def Start(self):
        self.process = sub.Popen(self.cmd, stdin = sub.PIPE, stdout = sub.PIPE, bufsize = 1, cwd = self.cwd)
        #self.process.stdout = io.StringIO()
        #self.process.stdin = io.StringIO()
        self.reader = SubprocessReaderThread(self.process.stdout, self.outputHandler)
        self.reader.start()
        self.writer = SubprocessWriterThread(self.process, self.procCommandQueue, self.inputHandler)
        self.writer.start()

    def Stop(self):
        self.reader.Stop()
        self.writer.Stop()
        self.process.kill()

    def ExecCommand(self, cmd):
        self.procCommandQueue.put_nowait(cmd)

    def SetOutputHandler(self, handler):
        self.outputHandler = handler

    def SetInputHandler(self, handler):
        self.inputHandler = handler

    def DefaultProcessHandler(self, data):
        pass
        #print "process ", data

class ErlangSubprocessWithSocketConnection(ErlangSubprocess):
    def __init__(self):
        params = [
            "-sname eide_shell{}".format(self.Port())
        ]
        erlModulesDir = os.path.join(os.getcwd(), 'data', 'erlang', 'modules')
        ErlangSubprocess.__init__(self, erlModulesDir, params)
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        #self.socket.setblocking(0)
        self.socket.settimeout(1)
        self.socketOutputHandler = self.DefaultSocketHandler
        self.socketInputHandler = self.DefaultSocketHandler
        self.socketCommandQueue = Queue()

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        return 55555

    def Start(self):
        ErlangSubprocess.Start(self)
        self.ExecCommand("eide_connect:start({}).".format(self.Port()))
        self.socket.connect((self.Host(), self.Port()))
        self.socketReader = SocketReaderThread(self.socket, self.socketOutputHandler)
        self.socketReader.start()
        self.socketWriter = SocketWriterThread(self.socket, self.socketCommandQueue)
        self.socketWriter.start()
        cacheDir = os.path.join(os.getcwd(), "cache", "erlang")
        if not os.path.isdir(cacheDir):
            os.makedirs(cacheDir)
        self.SetProp("cache_dir", cacheDir)
        self.CompileFile("eide_cache.erl")
        #self.Rpc("init", "stop")

    def Stop(self):
        self.socketReader.Stop()
        self.socketWriter.Stop()
        ErlangSubprocess.Stop(self)

    def DefaultSocketHandler(self, data):
        pass
        #print "socket ", data

    def SetSocketOutputHandler(self, handler):
        self.socketOutputHandler = handler

    def _ExecRequest(self, action, data):
        request = '{' + '"action": "{}", "data": {}'.format(action, data) + '}'
        self.socketCommandQueue.put_nowait(request)

    def CompileFile(self, file):
        self._ExecRequest("compile_file", '"{}"'.format(file))

    def Rpc(self, module, fun):
        self._ExecRequest("rpc", '["{0}", "{1}"]'.format(module, fun))

    def SetProp(self, prop, value):
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, value))

    def RemoveProp(self, prop):
        self._ExecRequest("remove_prop", '"{}"'.format(prop))

class SocketWriterThread(Thread):
    def __init__(self, socket, queue):
        Thread.__init__(self)
        self.socket = socket
        self.queue = queue
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            if not self.queue.empty():
                inCommand = self.queue.get()
                if inCommand:
                    msgLen = struct.pack('>H', len(inCommand))
                    cmd = msgLen + inCommand
                    self.socket.send(cmd)
                time.sleep(0.1)

    def Stop(self):
        self.active.clear()
        self.join()

class SocketReaderThread(Thread):
    def __init__(self, socket, handler):
        Thread.__init__(self)
        self.handler = handler
        self.socket = socket
        self.active = Event()

    def run(self):
        self.active.set()
        while self.active.is_set():
            try:
                recv = self.socket.recv(2)
                if recv:
                    msgLen = struct.unpack('>H', recv)[0]
                    data = self.socket.recv(msgLen)
                    if self.handler:
                        self.handler(data)
            except Exception, e:
                pass
            time.sleep(0.1)


    def Stop(self):
        self.active.clear()
        self.join()

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

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        return 55555

    def Start(self):
        self.connect((self.Host(), self.Port()))
        self.asyncoreThread = AsyncoreThread()
        self.asyncoreThread.Start()
        self.OnConnect()

    def Stop(self):
        self.asyncoreThread.Stop()
        self.close()

    def handle_connect(self):
        print "connect"
        #self.OnConnect()

    def writable(self):
        return not self.socketQueue.empty()

    def handle_close(self):
        print "close"
        self.close()

    def handle_write(self):
        print 'handle_write'
        cmd = self.socketQueue.get()
        if cmd:
            msgLen = struct.pack('>H', len(cmd))
            msg = msgLen + cmd
            print msg
            self.send(msg)

    def handle_read(self):
        print 'handle_read'
        recv = self.socket.recv(2)
        if recv:
            msgLen = struct.unpack('>H', recv)[0]
            data = self.socket.recv(msgLen)
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
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, value))

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
        self.cmd = "{} {}".format(erlang, ' '.join(params))
        self.pid = None
        self.handler = None
        self.processQueue = Queue()

        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.OnTimer)

    def SendCommandToProcess(self, cmd):
        cmd += '\n'
        if self.handler:
            self.handler(cmd)
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
        self.SendCommandToProcess("eide_connect:start({}).".format(self.Port()))
        ErlangSocketConnection.Start(self)

    def Stop(self):
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)

    def OnConnect(self):
        cacheDir = os.path.join(os.getcwd(), "cache", "erlang")
        if not os.path.isdir(cacheDir):
            os.makedirs(cacheDir)
        self.SetProp("cache_dir", erlstr(cacheDir))
        self.CompileFile("eide_cache.erl")
        #self.CompileFile("d:/Projects/NoiseIDE/NoiseIDEPython/data/erlang/modules/eide_cache.erl")

def erlstr(str):
    return str.replace("\\", "/")
