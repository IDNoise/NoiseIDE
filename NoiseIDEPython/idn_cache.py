import os
import json
import re
import shutil
from idn_config import Config
from idn_directoryinfo import DirectoryChecker
import core
from idn_utils import readFile
import wx
from idn_erlang_utils import IsInclude, IsModule

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
        self.file = moduleData.file

class Record:
    def __init__(self, moduleData, module, name, fields, types, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.fields = fields
        self.types = types
        self.line = line
        self.file = moduleData.file

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
        self.file = moduleData.file

class ExportedType:
    def __init__(self, moduleData, module, name, types, line):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.types = types
        self.line = line
        if moduleData:
            self.file = moduleData.file

class ModuleData:
    def __init__(self, module, data, srcFile):
        self.srcFile = srcFile
        self.file = data[FILE]
        self.module = module

        self.data = data

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

    def AllIncludes(self):
        includes = list(self.includes)[:]
        for include in self.includes:
            if include in ErlangCache.moduleData and self.module != include:
                includes += ErlangCache.moduleData[include].AllIncludes()
        return set(includes)

    def AllRecords(self):
        records = self.records[:]
        for include in self.includes:
            if include in ErlangCache.moduleData and self.module != include:
                records += ErlangCache.moduleData[include].AllRecords()
        return set(records)

    def AllMacroses(self):
        macroses = self.macroses[:]
        for include in self.includes:
            if include in ErlangCache.moduleData and self.module != include:
                macroses += ErlangCache.moduleData[include].AllMacroses()
        return set(macroses)

    def Functions(self, exported = True):
        funs = [fun for fun in self.functions if (exported and fun.exported == exported) or not exported]
        return funs

    def Types(self):
        return self.exportedTypes

    def Application(self):
        return core.Project.GetApp(self.file)

    def IsGlobalInclude(self):
        return "is_global_include" in self.data and self.data["is_global_include"]

class ErlangCache:

    ERLANG_TYPES = {
        ExportedType(None, None, "integer", "..-1 0 1 ..", 0),
        ExportedType(None, None, "term", "any()", 0),
        ExportedType(None, None, "boolean", "'false' | 'true'", 0),
        ExportedType(None, None, "byte", "0..255", 0),
        ExportedType(None, None, "char", "0..16#10ffff", 0),
        ExportedType(None, None, "non_neg_integer", "0..", 0),
        ExportedType(None, None, "pos_integer", "1..", 0),
        ExportedType(None, None, "neg_integer", "..-1", 0),
        ExportedType(None, None, "number", "integer() | float()", 0),
        ExportedType(None, None, "list", "[any()]", 0),
        ExportedType(None, None, "maybe_improper_list", "maybe_improper_list(any(), any())", 0),
        ExportedType(None, None, "string", "[char()]", 0),
        ExportedType(None, None, "nonempty_string", "[char(),...]", 0),
        ExportedType(None, None, "iolist", "maybe_improper_list(char() | binary() | iolist(), binary() | [])", 0),
        ExportedType(None, None, "module", "atom()", 0),
        ExportedType(None, None, "mfa", "{atom(),atom(),byte()}", 0),
        ExportedType(None, None, "node", "atom()", 0),
        ExportedType(None, None, "timeout", "'infinity' | non_neg_integer()", 0),
        ExportedType(None, None, "no_return", "none()", 0),
    }

    toLoad = []
    moduleData = {}
    modules = set()
    includes = set()
    @classmethod
    def Init(cls, project):
        cls.project = project
        cls.CACHE_DIR = os.path.join(core.MainFrame.cwd, "cache", "erlang")

        cls.ERLANG_LIBS_CACHE_DIR =  os.path.join(cls.CACHE_DIR, "runtimes", project.GetErlangRuntime())
        otherCacheDir =  os.path.join(cls.CACHE_DIR, "other")
        for dir in [cls.CACHE_DIR, cls.ERLANG_LIBS_CACHE_DIR, otherCacheDir]:
            if not os.path.isdir(dir):
                os.makedirs(dir)

        cls.erlangDir = os.path.dirname(os.path.dirname(project.GetErlangPath()))

        cls.checkers = {}

        cls.loadTimer = wx.Timer(core.MainFrame, wx.ID_ANY)
        cls.loadTimer.Start(100)
        core.MainFrame.Bind(wx.EVT_TIMER, cls.OnProgressTimer, cls.loadTimer)

        cls.fileCheckTimer = wx.Timer(core.MainFrame, wx.ID_ANY)
        cls.fileCheckTimer.Start(20000)
        core.MainFrame.Bind(wx.EVT_TIMER, cls.OnFileCheckTimer, cls.fileCheckTimer)

    @classmethod
    def OnProgressTimer(cls, event):
        if len(cls.toLoad) > 0:
            for i in range(40):
                try:
                    file = cls.toLoad.pop()
                    cls.LoadFile_(file)
                except IndexError, e:
                    pass

    @classmethod
    def OnFileCheckTimer(cls, event):
        for m, data in cls.moduleData.items():
            if not os.path.exists(data.srcFile):
                cls.UnloadFile(data.srcFile)

    @classmethod
    def AddToLoad(cls, f):
        cls.toLoad.append(f)

    @classmethod
    def LoadCacheFromDir(cls, d):
        d = os.path.join(cls.CACHE_DIR, d)
        for f in os.listdir(d):
            f = os.path.join(d, f)
            cls.AddToLoad(f)

    @classmethod
    def CleanDir(cls, d):
        try:
            d = os.path.join(cls.CACHE_DIR, d)
            shutil.rmtree(d, ignore_errors=True)
            os.mkdir(d)
        except Exception, e:
            pass

    @classmethod
    def LoadFile_(cls, file):
        try:
            if not os.path.isfile(file): return
            if not file.endswith(".cache"): return
            data = json.loads(readFile(file))
            name = os.path.basename(file)[:-6]
            if 'nt' == os.name:
                import win32api
                try:
                    data[FILE] = os.path.normpath(win32api.GetLongPathName(data[FILE]))
                    data[FILE] = data[FILE][0].upper() + data[FILE][1:]
                except Exception, e:
                    core.Log("error ", e, "on get long path name for ", data[FILE])
            srcFile = data[FILE].replace("\\", "/")
            if not os.path.isfile(srcFile):
                os.remove(file)
                return
            if (name in cls.modules and
                not cls.moduleData[name].file.lower().startswith(cls.erlangDir) and
                srcFile.lower().startswith(cls.erlangDir)):
                return
            if name.endswith(".hrl"):
                cls.includes.add(name)
            else:
                cls.modules.add(name)

            cls.moduleData[name] = ModuleData(name, data, file)
        except  Exception, e:
            core.Log("load cache file error", e)

    @classmethod
    def TryLoad(cls, module):
        if module in cls.moduleData:
            return True
        for dir in [cls.project.ProjectName(), os.path.join("runtimes", cls.project.GetErlangRuntime())]:
            file = os.path.join(cls.CACHE_DIR, dir, module + ".cache")
            if os.path.isfile(file):
                cls.LoadFile_(file)
                return module in cls.moduleData
        return False

    @classmethod
    def UnloadFile(cls, file):
        if not os.path.isfile(file): return
        if not file.endswith(".cache"): return
        name = os.path.basename(file)[:-6]
        if not name in cls.moduleData: return
        del cls.moduleData[name]
        if IsInclude(name):
            cls.includes.remove(name)
        else:
            cls.modules.remove(name)

    @classmethod
    def UnloadModule(cls, name):
        if not name in cls.moduleData: return
        del cls.moduleData[name]
        if IsInclude(name):
            cls.includes.remove(name)
        else:
            cls.modules.remove(name)

    @classmethod
    def StartCheckingFolder(cls, folder):
        if folder in cls.checkers:
            cls.checkers[folder].Stop()
        checker = DirectoryChecker(5, os.path.join(cls.CACHE_DIR, folder), False, [".cache"])
        checker.FilesCreatedEvent += cls.OnFilesCreated
        checker.FilesModifiedEvent += cls.OnFilesCreated
        checker.FilesDeletedEvent += cls.OnFilesDeleted
        checker.Start()
        cls.checkers[folder] = checker

    @classmethod
    def OnFilesCreated(cls, files):
        for file in files:
            cls.LoadFile_(file)

    @classmethod
    def OnFilesDeleted(cls, files):
        for file in files:
            cls.UnloadCacheForFile(file)

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
        cls.TryLoad(include)
        for module in cls.modules:
            data = cls.moduleData[module]
            if not data.file.startswith(cls.project.projectDir): continue
            if include in data.AllIncludes():
                result.append(data.file)
        return result

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
    def AllRecords(cls):
        result = []
        for include in cls.includes:
            result += cls.moduleData[include].AllRecords()
        return result

    @classmethod
    def AllRecordsData(cls, record):
        for include in cls.includes:
            for rec in cls.moduleData[include].AllRecords():
                if rec.name == record:
                    return rec
        return None

    @classmethod
    def AllRecordFields(cls, record):
        data = cls.AllRecordsData(record)
        if not data: return []
        return data.FieldsData()

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
        for typeData in cls.ERLANG_TYPES:
            if typeData.name == type:
                return typeData
        return None

    @classmethod
    def ModuleExportedTypes(cls, module):
        if not cls.TryLoad(module): return None
        return cls.moduleData[module].exportedTypes

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
    def Bif(cls, name, arity):
        funs = []
        for fun in cls.Bifs():
            if fun.name == name:
                funs.append(fun)
        for fun in funs:
            if fun.arity == arity:
                return fun
        if funs:
            return funs[0]
        return None

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

    @classmethod
    def ApplicationIncludes(cls, module):
        if not cls.TryLoad(module): return []
        app = cls.moduleData[module].Application()
        result = []
        for inc in cls.includes:
            if cls.moduleData[inc].Application() == app:
                result.append(inc + "\").")
        return result

    @classmethod
    def GlobalIncludes(cls):
        result = []
        for inc in cls.includes:
            if cls.moduleData[inc].IsGlobalInclude():
                result.append(cls.moduleData[inc].Application() + "/include/" + inc + "\").")
        return result

class IgorEntryData:
    def __init__(self, type, file, line):
        self.type = type
        self.file = file
        self.line = line

class IgorCache:
    @classmethod
    def Init(cls, project):
        cls.project = project
        cls.entries = {}
        cls.files = set()

        cls.entryRegex = re.compile(
            r"""
            ((record|variant)\s+([a-zA-Z0-9_]+\.)?(?P<record>[a-zA-Z0-9_]+)\s*)
            |(enum\s+(?P<enum>[a-zA-Z0-9_]+)\s*)
            |(interface\s+(?P<interface>[a-zA-Z0-9_]+)\s*)
            |(define\s+(?P<define>[a-zA-Z0-9_]+)\s*)
            """,
            re.VERBOSE | re.MULTILINE)

    @classmethod
    def GenerateForFile(cls, file):
        cls.ClearAllEntries(file)
        cls.files.add(file)
        text = readFile(file)
        lines = text.splitlines()
        for lineNumber, line in enumerate(lines):
            m = cls.entryRegex.search(line)
            if not m: continue
            d = m.groupdict()
            for g in d:
                value = d[g]
                if value != None:
                    cls.entries[value] = IgorEntryData(g, file, lineNumber + 1)

    @classmethod
    def ClearAllEntries(cls, file):
        for entry, entryData in cls.entries.copy().items():
            if entryData.file == file:
                del cls.entries[entry]

    @classmethod
    def FindCustomType(cls, customType):
        if customType in cls.entries:
            return (cls.entries[customType].file, cls.entries[customType].line)
        return None

    @classmethod
    def GetTypeOfEntry(cls, customType):
        if customType in cls.entries:
            return cls.entries[customType].type
        return None
