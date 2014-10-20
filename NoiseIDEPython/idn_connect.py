from idn_erlang_constats import *
import idn_events
from idn_utils import erlstr

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import socket
import os
import struct
from Queue import Queue
import asyncore
import json
import wx
import core
import asyncproc

class ErlangSocketConnection(asyncore.dispatcher):
    def __init__(self):
        asyncore.dispatcher.__init__(self)
        self.socketQueue = Queue()
        self.socketHandler = None
        self.port = self.PickUnusedPort()

    def Host(self):
        return "127.0.0.1"

    def PickUnusedPort(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(('localhost', 0))
        addr, port = s.getsockname()
        s.close()
        return port

    def Start(self):
        try:
            #asyncore.dispatcher.__init__(self)
            self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
            self.connect((self.Host(), self.port))
        except Exception ,e:
            core.Log("connect",  e)

    def Stop(self):
        if self.socket:
            self.close()

    def handle_connect(self):
        self.OnConnect()

    def writable(self):
        return not self.socketQueue.empty()

    def handle_close(self):
        self.close()

    def handle_write(self):
        cmd = self.socketQueue.get()
        if cmd:
            msgLen = struct.pack('>L', len(cmd))
            msg = msgLen + cmd
            self.send(msg)

    def handle_read(self):
        recv = self.socket.recv(4)
        if recv:
            msgLen = struct.unpack('>L', recv)[0]
            data = self.socket.recv(msgLen)
            errorCount = 0
            while len(data) != msgLen:
                try:
                    data += self.socket.recv(msgLen - len(data))
                except Exception, e:
                    core.Log("recv error", e)
                    errorCount += 1
                    if errorCount > 10:
                        return
            if self.socketHandler:
                self.socketHandler(data)

    def SetSocketHandler(self, handler):
        self.socketHandler = handler

    def _ExecRequest(self, action, data):
        request = '{' + '"action": "{}", "data": {}'.format(action, data) + '}'
        self.socketQueue.put(request)

    def OnClosed(self):
        pass

    def OnConnect(self):
        pass

class CompileErrorInfo:
    WARNING, ERROR = (0, 1)
    def __init__(self, path, type, line, msg):
        self.type = self.StrToType(type)
        self.msg = msg
        self.line = line - 1
        self.path = path

    def TypeToStr(self):
        if self.type == self.WARNING:
            return "Warning"
        else:
            return "Error"

    @classmethod
    def StrToType(cls, str):
        return cls.WARNING if str == "warning" else cls.ERROR

class ErlangIDEConnectAPI(ErlangSocketConnection):
    def __init__(self):
        ErlangSocketConnection.__init__(self)
        self.SetSocketHandler(self._HandleSocketResponse)

        self.SocketDataReceivedEvent = idn_events.Event()

        self.TaskAddedEvent = idn_events.Event()

    def Compile(self, path):
        #self.TaskAddedEvent((TASK_COMPILE, path.lower()))
        self._ExecRequest("compile", '"{}"'.format(erlstr(path)))

    def CompileTests(self, path):
        self.TaskAddedEvent((TASK_COMPILE_APP, path.lower()))
        self._ExecRequest("compile_tests", '"{}"'.format(erlstr(path)))

    def CompileApp(self, path):
        self.TaskAddedEvent((TASK_COMPILE_APP, path.lower()))
        self._ExecRequest("compile_app", '"{}"'.format(erlstr(path)))

    def CacheApp(self, path):
        self.TaskAddedEvent((TASK_CACHE_APP, path.lower()))
        self._ExecRequest("cache_app", '"{}"'.format(erlstr(path)))

    def CompileFileFly(self, realPath, flyPath):
        #self.TaskAddedEvent((TASK_COMPILE_FLY, realPath.lower()))
        self._ExecRequest("compile_file_fly", '["{0}", "{1}"]'.format(erlstr(realPath), erlstr(flyPath)))

    def Rpc(self, module, fun):
        self._ExecRequest("rpc", '["{0}", "{1}"]'.format(module, fun))

    def SetProp(self, prop, value):
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, erlstr(value)))

    def SetHomeDir(self, path):
        self._ExecRequest("set_home", '"{}"'.format(erlstr(path)))

    def DialyzeModules(self, paths):
        paths = '[' + ', '.join(['"{}"'.format(erlstr(path)) for path in paths]) + ']'
        self._ExecRequest("dialyze_modules", '{}'.format(paths))

    def DialyzeApps(self, apps):
        paths = '[' + ', '.join(['"{}"'.format(erlstr(app)) for app in apps]) + ']'
        self._ExecRequest("dialyze_apps", '{}'.format(paths))

    def RemoveProp(self, prop):
        self._ExecRequest("remove_prop", '"{}"'.format(prop))

    def XRef(self, module):
        self._ExecRequest("xref_module", '"{}"'.format(module))

    def CompileProjectFiles(self, paths):
        for path in paths:
            self.TaskAddedEvent((TASK_COMPILE, path.lower()))
            self.Compile(path)

    def GenerateFileCache(self, path):
        #self.TaskAddedEvent((TASK_GEN_FILE_CACHE, path.lower()))
        self._ExecRequest("gen_file_cache", '"{}"'.format(erlstr(path)))

    def GenerateFileCaches(self, paths):
        for path in paths:
            self.GenerateFileCache(path)

    def GenerateErlangCache(self, runtime):
        self._ExecRequest("gen_erlang_cache", '"{}"'.format( runtime))

    def GenerateProjectCache(self):
        self._ExecRequest("gen_project_cache", '[]')

    def AddPath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def CompileOption(self, path, option):
        self._ExecRequest("compile_option", '["{0}", "{1}"]'.format(erlstr(path), option))

    def RemovePath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def _HandleSocketResponse(self, text):
        self.lastTaskDone = None
        try:
            js = json.loads(text)
            if not "response" in js: return
            res = js["response"]
            wx.CallAfter(self.SocketDataReceivedEvent, res, js)

        except Exception, e:
            core.Log("===== connection exception ", text, e)

class ErlangIDEClientAPI(ErlangSocketConnection):
    def __init__(self):
        ErlangSocketConnection.__init__(self)
        self.SetSocketHandler(self._HandleSocketResponse)
        self.SocketDataReceivedEvent = idn_events.Event()

    def _HandleSocketResponse(self, text):
        try:
            js = json.loads(text)
            if not "action" in js: return
            res = js["action"]

            wx.CallAfter(self.SocketDataReceivedEvent, res, js)

        except Exception, e:
            core.Log("===== connection exception(client api) ", text, e)

class ErlangProcess(wx.EvtHandler):
    def __init__(self, cwd = os.getcwd(), params = None):
        wx.EvtHandler.__init__(self)
        self.cwd = cwd
        self.pid = None
        self.stopped = True
        self.proc = None
        params = [] if not params else params
        self.SetParams(params)

        self.timer = wx.Timer(self, wx.ID_ANY)
        self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)

        self.DataReceivedEvent = idn_events.Event()

    def GetSMPData(self):
        return ""

    def SetParams(self, params):
        erlang = core.Project.GetErlangPath()
        self.cmd = '"{}" {} {}'.format(erlang, self.GetSMPData(), ' '.join(params + ["-run reloader"] + self.GetAdditionalParams()))

    def SetCWD(self, cwd):
        self.cwd = cwd

    def GetAdditionalParams(self):
        return []

    def Start(self):
        self.proc = asyncproc.Process(self.cmd, cwd = self.cwd, creationflags=0x08000000)
        self.timer.Start(100)
        self.stopped = False

    def Stop(self):
        if self.timer:
            self.timer.Stop()
        if not self.proc or self.stopped:
            return
        self.proc.terminate()
        del self.proc
        self.proc = None
        self.stopped = True

    def OnTimer(self, event):
        text = self.proc.read()
        if text:
            self.DataReceivedEvent(text)

    def SendCommandToProcess(self, cmd):
        cmd += '\n'
        self.proc.write(cmd)

    def OnTerminate(self, *args, **kwargs):
        if self.timer:
            self.timer.Stop()

class ErlangProcessWithConnection(ErlangProcess, ErlangIDEConnectAPI):
    def __init__(self, cwd):
        ErlangIDEConnectAPI.__init__(self)
        ErlangProcess.__init__(self, cwd)

        self.ClosedConnectionEvent = idn_events.Event()

    def GetSMPData(self):
        return "-smp enable +sbt db +S4:4"

    def GetAdditionalParams(self):
        return ["-noiseide port {}".format(self.port), "-run noiseide_app"]

    def Start(self):
        ErlangProcess.Start(self)

    def Stop(self):
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)

    def ConnectToSocket(self):
        ErlangSocketConnection.Start(self)

    def OnClosed(self):
        core.Log("connection closed")
        if not self.stopped:
            self.ClosedConnectionEvent()

class ErlangProcessWithClientConnection(ErlangProcess, ErlangIDEClientAPI):
    def __init__(self, cwd, params):
        ErlangIDEClientAPI.__init__(self)
        ErlangProcess.__init__(self, cwd, params)

        self.ClosedConnectionEvent = idn_events.Event()
        self.started = False

    def GetAdditionalParams(self):
        return ["-noiseide client_port {}".format(self.port), "-run noiseide_app"]

    def Start(self):
        self.started = False
        self.DataReceivedEvent += self.OnShellDataReceived
        ErlangProcess.Start(self)

    def Stop(self):
        self.DataReceivedEvent -= self.OnShellDataReceived
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)

    def ConnectToSocket(self):
        ErlangSocketConnection.Start(self)

    def OnClosed(self):
        core.Log("connection closed")
        if not self.stopped:
            self.ClosedConnectionEvent()

    def OnShellDataReceived(self, text):
        if "started on:" in text and not self.started:
            self.started = True
            self.DataReceivedEvent -= self.OnShellDataReceived
            self.ConnectToSocket()