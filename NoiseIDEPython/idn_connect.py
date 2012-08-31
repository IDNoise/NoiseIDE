__author__ = 'Yaroslav Nikityshev aka IDNoise'

import socket
import os
import time
import struct
from Queue import Queue
from threading import Thread, Event
from wx import Process
import asyncore
import json
import random
import wx
from idn_global import GetProject, Log

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
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        #self.socket.setsockopt( socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
        self.socketQueue = Queue()
        self.socketHandler = None
        self.port = random.randrange(55555, 55600)

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        pass

    def Start(self):
        while True:
            try:
                self.connect((self.Host(), self.port))
                break
            except Exception, e:
                pass
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
            #print data
            if self.socketHandler:
                self.socketHandler(data)

    def SetSocketHandler(self, handler):
        self.socketHandler = handler

    def _ExecRequest(self, action, data):
        request = '{' + '"action": "{}", "data": {}'.format(action, data) + '}'
        #Log("request", request)
        self.socketQueue.put(request)

    def OnConnect(self):
        pass

class CompileErrorInfo:
    WARNING, ERROR = (0, 1)
    def __init__(self, path, type, line, msg):
        self.type = self.StrToType(type)
        self.msg = msg
        self.line = line - 1
        self.path = path
        #print type, line, msg

    def TypeToStr(self):
        if self.type == self.WARNING:
            return "Warning"
        else:
            return "Error"

    @classmethod
    def StrToType(cls, str):
        return cls.WARNING if str == "warning" else cls.ERROR

class ErlangIDEConnectAPI(ErlangSocketConnection):
    TASK_COMPILE, TASK_COMPILE_FLY, TASK_GEN_FILE_CACHE, TASK_GEN_ERLANG_CACHE = range(4)

    def __init__(self):
        ErlangSocketConnection.__init__(self)
        self.SetSocketHandler(self._HandleSocketResponse)

    def CompileFile(self, file):
        GetProject().AddTask((self.TASK_COMPILE, file))
        self._ExecRequest("compile_file", '"{}"'.format(file))

    def CompileFileFly(self, realPath, flyPath):
        GetProject().AddTask((self.TASK_COMPILE_FLY, realPath.lower()))
        self._ExecRequest("compile_file_fly", '["{0}", "{1}"]'.format(erlstr(realPath), erlstr(flyPath)))

    def Rpc(self, module, fun):
        self._ExecRequest("rpc", '["{0}", "{1}"]'.format(module, fun))

    def SetProp(self, prop, value):
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, erlstr(value)))

    def RemoveProp(self, prop):
        self._ExecRequest("remove_prop", '"{}"'.format(prop))

    def CompileProjectFile(self, file, app):
        self._ExecRequest("compile_project_file", '["{0}", "{1}"]'.format(erlstr(file), app))

    def CompileProjectFiles(self, files):
        GetProject().CreateProgressDialog("Compiling project")
        for (file, app) in files:
            GetProject().AddTask((self.TASK_COMPILE, file.lower()))
            self.CompileProjectFile(file, app)

    def GenerateFileCache(self, file):
        GetProject().AddTask((self.TASK_GEN_FILE_CACHE, file.lower()))
        self._ExecRequest("gen_file_cache", '"{}"'.format(erlstr(file)))

    def GenerateFileCaches(self, files):
        for file in files:
            self.GenerateFileCache(file)

    def GenerateErlangCache(self):
        GetProject().AddTask(self.TASK_GEN_ERLANG_CACHE)
        GetProject().CreateProgressDialog("Generating/Checking erlang cache")
        self._ExecRequest("gen_erlang_cache", '[]')

    def GenerateProjectCache(self):
        self._ExecRequest("gen_project_cache", '[]')

    def AddPath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def RemovePath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def _HandleSocketResponse(self, text):
        #Log("response", text)
        self.lastTaskDone = None
        try:
            js = json.loads(text)
            if not "response" in js: return
            res = js["response"]

            if res == "compile" or res == "compile_fly":
                errorsData = js["errors"]
                path = pystr(js["path"])

                if res == "compile":
                    done = (self.TASK_COMPILE, path.lower())
                else:
                    done = (self.TASK_COMPILE_FLY, path.lower())
                GetProject().TaskDone("Compiled {}".format(path), done)

                errors = []
                for error in errorsData:
                    if error["line"] == "none":
                        return
                    errors.append(CompileErrorInfo(path, error["type"], error["line"], error["msg"]))
                #print "compile result: {} = {}".format(path, errors)
                GetProject().AddErrors(path, errors)

            elif res == "gen_file_cache":
                path = pystr(js["path"])
                GetProject().TaskDone("Generated cache for {}".format(path), (self.TASK_GEN_FILE_CACHE, path.lower()))
            elif res == "gen_erlang_cache":
                GetProject().TaskDone("Generated cache for erlang libs", self.TASK_GEN_ERLANG_CACHE)
            elif res == "connect":
                Log("socket connected")
        except Exception, e:
            Log("===== connection exception ", e)

class ErlangProcess(Process):
    def __init__(self, cwd = os.getcwd(), params = []):
        Process.__init__(self)
        self.Redirect()
        self.cwd = cwd
        erlang = GetProject().GetErlangPath()
        self.cmd = "{} {}".format(erlang, ' '.join(params + ["-s reloader"]))
        self.pid = None
        self.handler = None
        self.processQueue = Queue()
        self.stopped = False

        self.timer = wx.Timer(self, wx.NewId())
        self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)

    def SetParams(self, params):
        erlang = GetProject().GetErlangPath()
        self.cmd = "{} {}".format(erlang, ' '.join(params + ["-s reloader"]))

    def SendCommandToProcess(self, cmd):
        cmd += '\n'
        self.outputStream.write(cmd)
        self.outputStream.flush()

    def Start(self):
        cwd = os.getcwd()
        os.chdir(self.cwd)
        self.pid = wx.Execute(self.cmd, wx.EXEC_ASYNC | wx.EXEC_HIDE_CONSOLE, self)
        os.chdir(cwd)
        self.timer.Start(100)
        self.inputStream = self.GetInputStream()
        self.outputStream = self.GetOutputStream()
        self.stopped = False

    def OnTimer(self, event):
        if  self.inputStream.CanRead():
            text =  self.inputStream.read()
            if self.handler:
                self.handler(text)

    def SetOutputHandler(self, handler):
        self.handler = handler

    def Stop(self):
        if self.timer:
            self.timer.Stop()
        if not self.pid or self.stopped: return
        wx.Kill(self.pid, wx.SIGKILL, wx.KILL_CHILDREN)
        self.stopped = True

    def OnTerminate(self, *args, **kwargs):
        if self.timer:
            self.timer.Stop()

class ErlangProcessWithConnection(ErlangProcess, ErlangIDEConnectAPI):
    def __init__(self, cwd):
        ErlangProcess.__init__(self, cwd)
        ErlangIDEConnectAPI.__init__(self)

    def Start(self):
        ErlangProcess.Start(self)
        self.SendCommandToProcess("eide_connect:start({}).".format(self.port))
        ErlangSocketConnection.Start(self)

    def Stop(self):
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)


def erlstr(str):
    return os.path.normcase(str).replace(os.sep, "/")

def pystr(str):
    return os.path.normcase(str).replace("/", os.sep)