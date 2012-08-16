import os
import json
from idn_config import Config
from idn_directoryinfo import DirectoryChecker
from idn_global import GetMainFrame

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

def readFile(file):
    with open(file) as f:
        return f.read()


class Function:
    def __init__(self, moduleData, module, name, arity, line, params, types, result, docref, exported, bif):
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

class Record:
    def __init__(self, moduleData, module, name, fields, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.fields = fields
        self.line = line

class Macros:
    def __init__(self, moduleData, module, name, value, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.value = value
        self.line = line

class ModuleData:
    def __init__(self, module, data):
        self.file = data[FILE]
        self.module = module

        self.functions = []
        for funData in data[FUNS]:
            isBif = funData[BIF] if BIF in funData else False
            fun = Function(self, module, funData[NAME], funData[ARITY],
                funData[LINE], funData[PARAMS],  funData[TYPES], funData[RESULT],
                funData[DOCREF], funData[EXPORTED], isBif)

            self.functions.append(fun)

        self.records = []
        for record in data[RECORDS_DATA]:
            recordData = data[RECORDS_DATA][record]
            self.records.append(Record(self, module, record, recordData[FIELDS], recordData[LINE]))

        self.macroses = []
        for macros in data[MACROS]:
            macData = data[MACROS][macros]
            self.macroses.append(Macros(self, module, macros, macData[VALUE], macData[LINE]))

        self.includes = set(data[INCLUDES])

    def AllRecords(self):
        records = self.records[:]
        for include in self.includes:
            if include in ErlangCache.moduleData:
                records += ErlangCache.moduleData[include].records
        return records

    def AllMacroses(self):
        macroses = self.macroses[:]
        for include in self.includes:
            if include in ErlangCache.moduleData:
                macroses += ErlangCache.moduleData[include].macroses
        return macroses

    def Functions(self, exported = True):
        return [fun for fun in self.functions if (exported and fun.exported == exported) or not exported]

class ErlangCache:
    checkers = {}
    modules = set()
    includes = set()
    moduleData = {}

    @classmethod
    def Init(cls):
        cls.CACHE_DIR = os.path.join(GetMainFrame().cwd, "cache", "erlang")

        cls.ERLANG_LIBS_CACHE_DIR =  os.path.join(cls.CACHE_DIR, "erlang")
        otherCacheDir =  os.path.join(cls.CACHE_DIR, "other")
        for dir in [cls.CACHE_DIR, cls.ERLANG_LIBS_CACHE_DIR, otherCacheDir]:
            if not os.path.isdir(dir):
                os.makedirs(dir)

        cls.erlangDir = os.path.dirname(os.path.dirname(Config.LanguageExec("erlang")))

    @classmethod
    def LoadCacheFromDir(cls, dir):
        print "loading cache from ", dir
        dir = os.path.join(cls.CACHE_DIR, dir)
        for file in os.listdir(dir):
            file = os.path.join(dir, file)
            cls.LoadFile(file)
        print "end loading cache from ", dir

    @classmethod
    def LoadFile(cls, file):
        if not os.path.isfile(file): return
        if not file.endswith(".cache"): return
        data = json.loads(readFile(file))
        name = os.path.basename(file)[:-6]
        if 'nt' == os.name:
            import win32api
            try :
                data[FILE] = os.path.normcase(win32api.GetLongPathName(data[FILE]))
            except Exception, e:
                print("error ", e, "on get long path name for ", data[FILE])
        file = data[FILE]
        if (name in cls.modules and name in cls.moduleData and
            cls.moduleData[name].file.lower().startswith(cls.erlangDir) and
            file != cls.moduleData[name].file):
            print("Ignoring replace of cache for standard erlang " +
                "module: {}\n\tPath:{}".format(name, file))
            return

        if name.endswith(".hrl"):
            cls.includes.add(name)
        else:
            cls.modules.add(name)

        cls.moduleData[name] = ModuleData(name, data)
        #print("Loading cache for: " + name)

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
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_CREATED, cls.LoadCacheForFile)
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_MODIFIED, cls.LoadCacheForFile)
        checker.AddHandler(DirectoryChecker.HANDLER_FILE_DELETED, cls.UnloadCacheForFile)
        checker.Start()
        cls.checkers[folder] = checker

    @classmethod
    def StopCheckingFolder(cls, folder):
        if folder in cls.checkers:
            cls.checkers[folder].Stop()
            del cls.checkers[folder]

    @classmethod
    def LoadCacheForFile(cls, file):
        cls.LoadFile(file)

    @classmethod
    def UnloadCacheForFile(cls, file):
        cls.UnloadFile(file)

    @classmethod
    def AllModules(cls):
        return cls.modules

    @classmethod
    def GetDependentModules(cls, include):
        if not module in cls.moduleData: return []
        result = []
        for module in cls.moduleData:
            data = cls.moduleData[module]
            if include in data.includes and data.file.endswith(".erl"):
                result.append(module)

    @classmethod
    def RecordFields(cls, module, record):
        data = cls.RecordData(module, record)
        if not data: return []
        return data.fields

    @classmethod
    def RecordData(cls, module, record):
        if not module in cls.moduleData: return None
        for rec in cls.moduleData[module].AllRecords():
            if rec.name == record:
                return rec
        return None

    @classmethod
    def ModuleFunctions(cls, module, exported = True):
        if not module in cls.moduleData: return []
        return cls.moduleData[module].Functions(exported)

    @classmethod
    def ModuleFunction(cls, module, funName, arity):
        if not module in cls.moduleData: return None
        for fun in cls.moduleData[module].functions:
            if fun.name == funName and fun.arity == arity:
                return fun
        return None

    @classmethod
    def ModuleRecords(cls, module):
        if not module in cls.moduleData: return []
        return cls.moduleData[module].AllRecords()

    @classmethod
    def Bifs(cls):
        module = "erlang"
        if not module in cls.moduleData: return []
        return [fun for fun in cls.moduleData[module].Functions() if fun.bif == True]

    @classmethod
    def Macroses(cls, module):
        if not module in cls.moduleData: return []
        return cls.moduleData[module].AllMacroses()

    @classmethod
    def MacrosData(cls, module, macros):
        if not module in cls.moduleData: return None
        for mac in cls.moduleData[module].AllMacroses():
            if mac.name == macros or mac.name.startswith(macros + "("):
                return mac
        return None
