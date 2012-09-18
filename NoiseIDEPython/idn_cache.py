import os
import json
import shutil
from idn_config import Config
from idn_directoryinfo import DirectoryChecker
from idn_global import GetMainFrame, Log, GetProject
from idn_utils import readFile
import wx

FILE = "file"
NAME = "name"
ARITY = "arity"
PARAMS = "params"
TYPES = "types"
RESULT = "result"
DOCREF = "docref"
INCLUDES = "includes"
EXPORTED = "exported"
FUNS = "funs"
LINE = "line"
BIF = "bif"
ERLANG = "erlang"
FIELDS = "fields"
RECORDS_DATA = "records_data"
VALUE = "value"
MACROS = "macros"
COMMENT = "comment"
EXPORTED_TYPES = "exported_types"


class Function:
    def __init__(self, moduleData, module, name, arity, line, params, types, result, docref, exported, bif, comment):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.arity = arity
        self.params = params
        self.line = line
        self.types = types
        self.result = result
        self.docref = docref
        self.exported = exported
        self.bif = bif
        self.comment = comment

class Record:
    def __init__(self, moduleData, module, name, fields, types, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.fields = fields
        self.types = types
        self.line = line

        self.fieldTypes = {}
        for i in range(len(self.fields)):
            self.fieldTypes[self.fields[i]] = self.types[i]

    def FieldsData(self):
        return self.fieldTypes.items()

class Macros:
    def __init__(self, moduleData, module, name, value, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.value = value
        self.line = line

class ExportedType:
    def __init__(self, moduleData, module, name, types, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.types = types
        self.line = line

class ModuleData:
    def __init__(self, module, data):
        self.file = data[FILE]
        self.module = module

        self.functions = []
        for funData in data[FUNS]:
            isBif = funData[BIF] if BIF in funData else False
            comment = funData[COMMENT] if COMMENT in funData else ""
            docref = funData[DOCREF] if DOCREF in funData else None
            fun = Function(self, module, funData[NAME], funData[ARITY],
                funData[LINE], funData[PARAMS],  funData[TYPES], funData[RESULT],
                docref, funData[EXPORTED], isBif, comment)

            self.functions.append(fun)

        self.records = []
        for record in data[RECORDS_DATA]:
            recordData = data[RECORDS_DATA][record]
            self.records.append(Record(self, module, record, recordData[FIELDS], recordData[TYPES], recordData[LINE]))

        self.macroses = []
        for macros in data[MACROS]:
            macData = data[MACROS][macros]
            self.macroses.append(Macros(self, module, macros, macData[VALUE], macData[LINE]))

        self.includes = set(data[INCLUDES])

        self.exportedTypes = []
        for expDype in data[EXPORTED_TYPES]:
            expData = data[EXPORTED_TYPES][expDype]
            self.exportedTypes.append(ExportedType(self, module, expDype, expData[TYPES], expData[LINE]))


    def AllRecords(self):
        records = self.records[:]
        for include in self.includes:
            if include in ErlangCache.moduleData and self.module != include:
                records += ErlangCache.moduleData[include].AllRecords()
        return records

    def AllMacroses(self):
        macroses = self.macroses[:]
        #print "module", self.module, "inc", self.includes
        for include in self.includes:
            if include in ErlangCache.moduleData and self.module != include:
                macroses += ErlangCache.moduleData[include].AllMacroses()
        return macroses

    def Functions(self, exported = True):
        funs = [fun for fun in self.functions if (exported and fun.exported == exported) or not exported]
        return funs + self.exportedTypes


class ErlangCache:

    toLoad = []

    @classmethod
    def Init(cls, project):
        cls.project = project
        cls.CACHE_DIR = os.path.join(GetMainFrame().cwd, "cache", "erlang")

        cls.ERLANG_LIBS_CACHE_DIR =  os.path.join(cls.CACHE_DIR, "erlang")
        otherCacheDir =  os.path.join(cls.CACHE_DIR, "other")
        for dir in [cls.CACHE_DIR, cls.ERLANG_LIBS_CACHE_DIR, otherCacheDir]:
            if not os.path.isdir(dir):
                os.makedirs(dir)

        cls.erlangDir = os.path.dirname(os.path.dirname(project.GetErlangPath()))

        cls.checkers = {}
        cls.modules = set()
        cls.includes = set()
        cls.moduleData = {}

        cls.loadTimer = wx.Timer(GetMainFrame(), wx.NewId())
        cls.loadTimer.Start(100)
        GetMainFrame().Bind(wx.EVT_TIMER, cls.OnProgressTimer, cls.loadTimer)

    @classmethod
    def OnProgressTimer(cls, event):
        for i in range(15):
            try:
                file = cls.toLoad.pop()
                cls.LoadFile_(file)
            except IndexError, e:
                pass

    @classmethod
    def AddToLoad(cls, file):
        cls.toLoad.append(file)

    @classmethod
    def LoadCacheFromDir(cls, dir):
        #Log("loading cache from", dir)
        dir = os.path.join(cls.CACHE_DIR, dir)
        for file in os.listdir(dir):
            file = os.path.join(dir, file)
            cls.AddToLoad(file)
            #cls.LoadFile(file)
        #Log("end loading cache from", dir)

    @classmethod
    def CleanDir(cls, dir):
        try:
            dir = os.path.join(cls.CACHE_DIR, dir)
            shutil.rmtree(dir, ignore_errors=True)
            os.mkdir(dir)
        except Exception, e:
            pass

    @classmethod
    def LoadFile_(cls, file):
        try:
            if not os.path.isfile(file): return
            if not file.endswith(".cache"): return
            data = json.loads(readFile(file))
            if not os.path.isfile(data[FILE]):
                os.remove(file)
                return
            name = os.path.basename(file)[:-6]
            if 'nt' == os.name:
                import win32api
                try:
                    data[FILE] = os.path.normcase(win32api.GetLongPathName(data[FILE]))
                except Exception, e:
                    Log("error ", e, "on get long path name for ", data[FILE])
            file = data[FILE]
            #Log("loading cache for", file)
            if (name in cls.modules and name in cls.moduleData and
                cls.moduleData[name].file.lower().startswith(cls.erlangDir) and
                file != cls.moduleData[name].file):
                Log("Ignoring replace of cache for standard erlang " +
                    "module: {}\n\tPath:{}".format(name, file))
                return

            if name.endswith(".hrl"):
                cls.includes.add(name)
            else:
                cls.modules.add(name)

            cls.moduleData[name] = ModuleData(name, data)

            #Log("Loaded cache for", file)
        except  Exception, e:
            Log("load cache file error", e)
        #Log("Cache:", file)

    @classmethod
    def TryLoad(cls, module):
        if module in cls.moduleData:
            return True
        for dir in ["erlang", GetProject().ProjectName()]:
            file = os.path.join(cls.CACHE_DIR, dir, module + ".cache")
            #print file
            if os.path.isfile(file):
                cls.LoadFile_(file)
                return True
        return False

    @classmethod
    def UnloadFile(cls, file):
        if not os.path.isfile(file): return
        if not fileName.endswith(".cache"): return
        name = os.path.basename(file)[:-6]
        del cls.moduleData[name]
        cls.modules.remove(name)

    @classmethod
    def StartCheckingFolder(cls, folder):
        if folder in cls.checkers:
            cls.checkers[folder].Stop()
        checker = DirectoryChecker(1, os.path.join(cls.CACHE_DIR, folder), False, [".cache"])
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_CREATED, cls.LoadFile_)
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_MODIFIED, cls.LoadFile_)
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_DELETED, cls.UnloadCacheForFile)
        checker.Start()
        cls.checkers[folder] = checker

    @classmethod
    def StopCheckingFolder(cls, folder):
        if folder in cls.checkers:
            cls.checkers[folder].Stop()
            del cls.checkers[folder]

    @classmethod
    def UnloadCacheForFile(cls, file):
        cls.UnloadFile(file)

    @classmethod
    def AllModules(cls):
        return cls.modules

    @classmethod
    def GetDependentModules(cls, include):
        result = []
        if not cls.TryLoad(include): return result
        for module in cls.moduleData:
            data = cls.moduleData[module]
            if include in data.includes and data.file.endswith(".erl"):
                result.append(module)

    @classmethod
    def RecordFields(cls, module, record):
        data = cls.RecordData(module, record)
        if not data: return []
        return data.FieldsData()

    @classmethod
    def RecordData(cls, module, record):
        if not cls.TryLoad(module): return None
        for rec in cls.moduleData[module].AllRecords():
            if rec.name == record:
                return rec
        return None

    @classmethod
    def ModuleFunctions(cls, module, exported = True):
        if not cls.TryLoad(module):return []
        return cls.moduleData[module].Functions(exported)

    @classmethod
    def ModuleFunction(cls, module, funName, arity):
        if not cls.TryLoad(module): return None
        funs = []
        for fun in cls.moduleData[module].functions:
            if fun.name == funName:
                funs.append(fun)
        for fun in funs:
            if fun.arity == arity:
                return fun
        if funs:
            return funs[0]
        return None

    @classmethod
    def ModuleExportedData(cls, module, type):
        if not cls.TryLoad(module): return None
        for typeData in cls.moduleData[module].exportedTypes:
            if typeData.name == type:
                return typeData
        return None

    @classmethod
    def ModuleRecords(cls, module):
        if not cls.TryLoad(module): return []
        return cls.moduleData[module].AllRecords()

    @classmethod
    def Bifs(cls):
        module = "erlang"
        if not cls.TryLoad(module): return []
        return [fun for fun in cls.moduleData[module].Functions() if isinstance(fun, Function) and fun.bif == True]

    @classmethod
    def Macroses(cls, module):
        if not cls.TryLoad(module): return []
        return cls.moduleData[module].AllMacroses()

    @classmethod
    def MacrosData(cls, module, macros):
        if not cls.TryLoad(module): return None
        for mac in cls.moduleData[module].AllMacroses():
            if mac.name == macros or mac.name.startswith(macros + "("):
                return mac
        return None
