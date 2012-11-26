from idn_erlang_constats import *
import idn_events
from idn_utils import erlstr

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import socket
import os
import struct
from Queue import Queue
from threading import Thread, Event
from wx import Process
import asyncore
import json
import wx
import core


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
        #self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        #self.socket.setsockopt( socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
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
        while True:
            try:
                self.connect((self.Host(), self.port))
                break
            except Exception ,e:
                core.Log("connect",  e)
        self.asyncoreThread = AsyncoreThread()
        self.asyncoreThread.Start()

    def Stop(self):
        self.asyncoreThread.Stop()
        self.close()
        #print "stop connection"

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
            msgLen = struct.pack('>L', len(cmd))
            msg = msgLen + cmd
            #print msg
            self.send(msg)

    def handle_read(self):
        #print 'handle_read'
        recv = self.socket.recv(4)
        if recv:
            msgLen = struct.unpack('>L', recv)[0]
            data = self.socket.recv(msgLen)
            while len(data) != msgLen:
                try:
                    data += self.socket.recv(msgLen - len(data))
                except Exception, e:
                    core.Log("recv error", e)
            if self.socketHandler:
                self.socketHandler(data)

    def SetSocketHandler(self, handler):
        self.socketHandler = handler

    def _ExecRequest(self, action, data):
        request = '{' + '"action": "{}", "data": {}'.format(action, data) + '}'
        #core.Log("request", request)
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
    def __init__(self):
        ErlangSocketConnection.__init__(self)
        self.SetSocketHandler(self._HandleSocketResponse)

        self.SocketDataReceivedEvent = idn_events.Event()

        self.TaskAddedEvent = idn_events.Event()

    def Compile(self, file):
        self.TaskAddedEvent((TASK_COMPILE, file.lower()))
        self._ExecRequest("compile", '"{}"'.format(erlstr(file)))

    def CompileFileFly(self, realPath, flyPath):
        self.TaskAddedEvent((TASK_COMPILE_FLY, realPath.lower()))
        self._ExecRequest("compile_file_fly", '["{0}", "{1}"]'.format(erlstr(realPath), erlstr(flyPath)))

    def Rpc(self, module, fun):
        self._ExecRequest("rpc", '["{0}", "{1}"]'.format(module, fun))

    def SetProp(self, prop, value):
        self._ExecRequest("set_prop", '["{0}", "{1}"]'.format(prop, erlstr(value)))

    def SetHomeDir(self, path):
        self._ExecRequest("set_home", '"{}"'.format(erlstr(path)))

    def DialyzeModules(self, files):
        paths = '[' + ', '.join(['"{}"'.format(erlstr(file)) for file in files]) + ']'
        self._ExecRequest("dialyze_modules", '{}'.format(paths))

    def DialyzeApps(self, apps):
        paths = '[' + ', '.join(['"{}"'.format(erlstr(app)) for app in apps]) + ']'
        self._ExecRequest("dialyze_apps", '{}'.format(paths))

    def RemoveProp(self, prop):
        self._ExecRequest("remove_prop", '"{}"'.format(prop))

    def XRef(self, module):
        self._ExecRequest("xref_module", '"{}"'.format(module))

    def CompileProjectFiles(self, files):
        for file in files:
            self.TaskAddedEvent((TASK_COMPILE, file.lower()))
            self.Compile(file)

    def GenerateFileCache(self, file):
        self.TaskAddedEvent((TASK_GEN_FILE_CACHE, file.lower()))
        self._ExecRequest("gen_file_cache", '"{}"'.format(erlstr(file)))

    def GenerateFileCaches(self, files):
        for file in files:
            self.GenerateFileCache(file)

    def GenerateErlangCache(self, runtime):
        self._ExecRequest("gen_erlang_cache", '"{}"'.format( runtime))

    def GenerateProjectCache(self):
        self._ExecRequest("gen_project_cache", '[]')

    def AddPath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def CompileOption(self, file, option):
        self._ExecRequest("compile_option", '["{0}", "{1}"]'.format(erlstr(file), option))

    def RemovePath(self, path):
        self._ExecRequest("add_path", '"{}"'.format(erlstr(path)))

    def _HandleSocketResponse(self, text):
        #core.Log("response", text)

        self.lastTaskDone = None
        try:
            js = json.loads(text)
            if not "response" in js: return
            res = js["response"]

            wx.CallAfter(self.SocketDataReceivedEvent, res, js)

        except Exception, e:

            core.Log("===== connection exception ", text, e)



#class ErlangNodeProtocol(protocol.ProcessProtocol):
#    def __init__(self, parent):
#        self.parent = parent
#
#    def connectionMade(self):
#        print "connectionMade!"
##        for i in range(self.verses):
##            self.transport.write("Aleph-null bottles of beer on the wall,\n" +
##                                 "Aleph-null bottles of beer,\n" +
##                                 "Take one down and pass it around,\n" +
##                                 "Aleph-null bottles of beer on the wall.\n")
#        #self.transport.closeStdin() # tell them we're done
#    def outReceived(self, data):
#        self.parent.DataReceived(data)
#        print "outReceived! {}".format(data)
#        #self.data = self.data + data
#    def errReceived(self, data):
#        print "errReceived! with %d bytes!" % len(data)
#    def inConnectionLost(self):
#        print "inConnectionLost! stdin is closed! (we probably did it)"
#    def outConnectionLost(self):
#        print "outConnectionLost! The child closed their stdout!"
#    def errConnectionLost(self):
#        print "errConnectionLost! The child closed their stderr."
#    def processExited(self, reason):
#        print "processExited, status %d" % (reason.value.exitCode,)
#    def processEnded(self, reason):
#        print "processEnded, status %d" % (reason.value.exitCode,)
#        print "quitting"
#        reactor.stop()
#    def Write(self, cmd):
#        self.transport.write(cmd)
#        #self.transport.flush()
#
#
#class ErlangProcess():
#    def __init__(self, cwd, params = []):
#        self.cwd = cwd
#
#        self.protocol = ErlangNodeProtocol(self)
#        self.SetParams(params)
#        self.DataReceivedEvent = idn_events.Event()
#        self.stopped = False
#        #print self.DataReceivedEvent
#
#    def SendCommandToProcess(self, cmd):
#        self.protocol.Write(cmd)
#
#    def SetParams(self, params):
#        self.cmd = core.Project.GetErlangPath()
#        self.params = ' '.join(params + ["-s reloader"])
#        self.program = "erlang"
#
#    def Start(self):
#        self.stopped = False
#        reactor.callFromThread(reactor.spawnProcess, self.protocol, self.cmd, [self.program, self.params])
#        #reactor.spawnProcess(self.protocol, self.cmd, [self.program, self.params])
#        print "xx"
#
#    def DataReceived(self, data):
#        self.DataReceivedEvent(data)
#
#    def Stop(self):
#        pass
#        #reactor.stop()


class ErlangProcess(Process):
    def __init__(self, cwd = os.getcwd(), params = []):
        Process.__init__(self)
        self.Redirect()
        self.cwd = cwd
        self.SetParams(params)
        self.pid = None
        self.handler = None
        self.processQueue = Queue()
        self.stopped = False

        self.timer = wx.Timer(self, wx.NewId())
        self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)

        self.DataReceivedEvent = idn_events.Event()

    def GetSMPData(self):
        return ""

    def SetParams(self, params):
        erlang = core.Project.GetErlangPath()
        self.cmd = "{} {} {}".format(erlang, self.GetSMPData(), ' '.join(params + ["-s reloader"]))

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
            self.DataReceivedEvent(text)

    def Stop(self):
        if self.timer:
            self.timer.Stop()
        if not self.pid or self.stopped:
            return
        wx.Kill(self.pid, wx.SIGKILL, wx.KILL_CHILDREN)
        self.stopped = True

    def OnTerminate(self, *args, **kwargs):
        if self.timer:
            self.timer.Stop()

class ErlangProcessWithConnection(ErlangProcess, ErlangIDEConnectAPI):
    def __init__(self, cwd):
        ErlangProcess.__init__(self, cwd)
        ErlangIDEConnectAPI.__init__(self)

        self.ClosedConnectionEvent = idn_events.Event()

    def GetSMPData(self):
        return "-smp enable +sbt db +S3:3"

    def Start(self):
        ErlangProcess.Start(self)
        self.SendCommandToProcess("eide_connect:start({}).".format(self.port))
        #time.sleep(1)

    def Stop(self):
        ErlangSocketConnection.Stop(self)
        ErlangProcess.Stop(self)

    def ConnectToSocket(self):
        ErlangSocketConnection.Start(self)

    def OnClosed(self):
        core.Log("connection closed")
        if not self.stopped:
            self.ClosedConnectionEvent()
