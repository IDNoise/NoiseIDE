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
import wx.lib.agw.pyprogress as PP
import asyncore
import json
import random
import wx
from idn_global import GetTabMgr, GetProject, Log

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
        self.port = random.randrange(55555, 55600)

    def Host(self):
        return "127.0.0.1"

    def Port(self):
        pass

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
        self.type = self.WARNING if type == "warning" else self.ERROR
        self.msg = msg
        self.line = line - 1
        self.path = path
        #print type, line, msg

class ErlangIDEConnectAPI(ErlangSocketConnection):
    TASK_COMPILE, TASK_GEN_FILE_CACHE, TASK_GEN_ERLANG_CACHE = range(3)

    def __init__(self):
        ErlangSocketConnection.__init__(self)
        self.tasks = set()
        self.SetSocketHandler(self._HandleSocketResponse)
        self.progressDialog = None
        self.lastTaskDone = None
        self.progressTimer = wx.Timer(self, wx.NewId())
        self.progressTimer.Start(250)
        self.Bind(wx.EVT_TIMER, self.OnProgressTimer, self.progressTimer)
        self.lastTaskTime = time.time()

    def CompileFile(self, file):
        self.tasks.add((self.TASK_COMPILE, file))
        self._ExecRequest("compile_file", '"{}"'.format(file))

    def CompileFileFly(self, realPath, flyPath):
        self.tasks.add((self.TASK_COMPILE, realPath.lower()))
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
        self._CreateProgressDialog("Compiling project")
        for (file, app) in files:
            self.tasks.add((self.TASK_COMPILE, file.lower()))
            self.CompileProjectFile(file, app)

    def GenerateFileCache(self, file):
        self.tasks.add((self.TASK_GEN_FILE_CACHE, file.lower()))
        self._ExecRequest("gen_file_cache", '"{}"'.format(erlstr(file)))

    def GenerateFileCaches(self, files):
        for file in files:
            self.GenerateFileCache(file)

    def GenerateErlangCache(self):
        self.tasks.add(self.TASK_GEN_ERLANG_CACHE)
        self._CreateProgressDialog("Generating/Checking erlang cache")
        self._ExecRequest("gen_erlang_cache", '[]')

    def GenerateProjectCache(self):
        self._ExecRequest("gen_project_cache", '[]')

    def AddPath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def RemovePath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def _CreateProgressDialog(self, text = "IDE Activities"):
        if self.progressDialog:
            pass
        else:
            self.progressDialog = PP.PyProgress(message = text,
                agwStyle= wx.PD_APP_MODAL | wx.PD_ELAPSED_TIME)
            self.progressDialog.SetGaugeProportion(0.2)
            self.progressDialog.SetGaugeSteps(50)
            self.progressDialog.SetGaugeBackground(wx.BLACK)
            self.progressDialog.SetFirstGradientColour(wx.GREEN)
            self.progressDialog.SetSecondGradientColour(wx.BLUE)
            self.progressDialog.SetSize((500, 150))
            self.progressDialog.ShowDialog()

    def _HandleSocketResponse(self, text):
        #Log("response", text)
        try:
            js = json.loads(text)
            if not "response" in js: return
            res = js["response"]

            if res == "compile":
                errorsData = js["errors"]
                path = pystr(js["path"])
                errors = []
                for error in errorsData:
                    errors.append(CompileErrorInfo(path, error["type"], error["line"], error["msg"]))
                #print "compile result: {} = {}".format(path, errors)
                GetProject().AddErrors(path, errors)
                self.lastTaskDone = "Compiled {}".format(path)
                task = (self.TASK_COMPILE, path.lower())
                if task in self.tasks:
                    self.tasks.remove(task)
            elif res == "gen_file_cache":
                path = pystr(js["path"])
                self.lastTaskDone = "Generated cache for {}".format(path)
                self.tasks.remove((self.TASK_GEN_FILE_CACHE, path.lower()))
            elif res == "gen_erlang_cache":
                self.lastTaskDone = "Generated cache for erlang libs"
                self.tasks.remove(self.TASK_GEN_ERLANG_CACHE)
            elif res == "connect":
                Log("socket connected")
        except Exception, e:
            Log("===== exception ", e)

        if len(self.tasks) == 0 and self.progressDialog:
            self.progressDialog.Destroy()
            self.progressDialog = None

    def OnProgressTimer(self, event):
        if self.progressDialog:
            if self.lastTaskDone:
                self.lastTaskTime = time.time()
                self.progressDialog.UpdatePulse(self.lastTaskDone)
                self.lastTaskDone = None
            if time.time() - self.lastTaskTime > 7 and len(self.tasks) > 0:
                Log("####\n 7 seconds from last task done. Tasks left ", len(self.tasks))
                Log("\n\t".join([str(t) for t in self.tasks]))

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

        self.timer = wx.Timer(self, wx.NewId())
        self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)

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
        if not self.pid: return
        result = Process.Kill(self.pid, wx.SIGTERM)
        if result not in [wx.KILL_OK, wx.KILL_NO_PROCESS]:
            Process.Kill(self.pid, wx.SIGABRT)
            Process.Kill(self.pid, wx.SIGKILL)

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
    return str.replace(os.sep, "/")

def pystr(str):
    return str.replace("/", os.sep)