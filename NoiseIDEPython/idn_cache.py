import fnmatch
import os
import json
import re
import shutil
import sys
from idn_config import Config
from idn_directoryinfo import DirectoryChecker
import core
from idn_utils import readFile, decode
import wx
from collections import OrderedDict
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
CALLBACKS = "callbacks"


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

        self.fieldTypes = OrderedDict()
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

class Callback:
    def __init__(self, moduleData, module, name, params):
        self.moduleData = moduleData
        self.module = module
        self.name = name
        self.arity = len(params)
        self.params = params
        self.file = moduleData.file

class ModuleData:
    def __init__(self, module, data, srcFile, app):
        self.srcFile = srcFile
        self.file = data[FILE]
        self.module = module
        self.app = app

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

        self.includes = set([(i[0], i[1]) for i in data[INCLUDES]])

        self.exportedTypes = []
        for expDype in data[EXPORTED_TYPES]:
            expData = data[EXPORTED_TYPES][expDype]
            self.exportedTypes.append(ExportedType(self, module, expDype, expData[TYPES], expData[LINE]))

        self.callbacks = []
        if CALLBACKS in data:
            for cb in data[CALLBACKS]:
                cdData = data[CALLBACKS][cb]
                self.callbacks.append(Callback(self, module, cb, cdData[PARAMS]))

    def Key(self):
        return (self.app, self.module)

    def AllIncludes(self):
        includes = list(self.includes)[:]
        for include in self.includes:
            if include in ErlangCache.includes and self.Key() != include:
                includes += ErlangCache.includes[include].AllIncludes()
        return set(includes)

    def AllRecords(self):
        records = self.records[:]
        for include in self.includes:
            if include in ErlangCache.includes and self.Key() != include:
                records += ErlangCache.includes[include].AllRecords()
        return set(records)

    def AllMacroses(self):
        macroses = self.macroses[:]
        for include in self.includes:
            if include in ErlangCache.includes and self.Key() != include:
                macroses += ErlangCache.includes[include].AllMacroses()
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
    modules = {}
    includes = {}
    project = None
    erlangDir = ""

    @classmethod
    def CacheDir(cls): return os.path.join(decode(core.DataDir()), "cache", "erlang")

    @classmethod
    def Init(cls, project):
        cls.project = project
        cls.ERLANG_LIBS_CACHE_DIR =  os.path.join(cls.CacheDir(), "runtimes", project.GetErlangRuntime())

        for d in [cls.CacheDir(), cls.ERLANG_LIBS_CACHE_DIR]:
            if not os.path.isdir(d):
                os.makedirs(d)

        cls.erlangDir = os.path.dirname(os.path.dirname(project.GetErlangPath()))

        cls.checkers = {}

        cls.loadTimer = wx.Timer(core.MainFrame, wx.ID_ANY)
        cls.loadTimer.Start(10)
        core.MainFrame.Bind(wx.EVT_TIMER, cls.OnProgressTimer, cls.loadTimer)

        cls.fileCheckTimer = wx.Timer(core.MainFrame, wx.ID_ANY)
        cls.fileCheckTimer.Start(20000)
        core.MainFrame.Bind(wx.EVT_TIMER, cls.OnFileCheckTimer, cls.fileCheckTimer)

    @classmethod
    def Stop(cls):
        cls.loadTimer.Stop()
        cls.fileCheckTimer.Stop()

    @classmethod
    def OnProgressTimer(cls, event):
        if len(cls.toLoad) > 0:
            for i in range(60):
                try:
                    f = cls.toLoad.pop()
                    cls.LoadFile_(f)
                except IndexError, e:
                    pass

    @classmethod
    def OnFileCheckTimer(cls, event):
        for data in cls.modules.values() + cls.includes.values():
            if not os.path.exists(data.srcFile):
                cls.UnloadFile(data.srcFile)

    @classmethod
    def AddToLoad(cls, f):
        cls.toLoad.append(f)

    @classmethod
    def LoadCacheFromDir(cls, d):
        d = os.path.join(cls.CacheDir(), d)
        for f in os.listdir(d):
            f = os.path.join(d, f)
            if os.path.isdir(f):
                for f1 in os.listdir(f):
                    cls.AddToLoad(os.path.join(f, f1))
            else:
                cls.AddToLoad(f)

    @classmethod
    def CleanDir(cls, d):
        try:
            d = os.path.join(cls.CacheDir(), d)
            shutil.rmtree(d, True)
            os.mkdir(d)
        except Exception, e:
            pass

    @classmethod
    def LoadFile_(cls, f):
        try:
            if not f.endswith(".cache") or not os.path.isfile(f): return
            data = json.loads(readFile(f))
            if not os.path.isfile(data[FILE]):
                os.remove(f)
                return None
            name = os.path.basename(f)[:-6]
            app = os.path.basename(os.path.dirname(f))
            if 'nt' == os.name:
                import win32api
                try:
                    data[FILE] = os.path.normpath(win32api.GetLongPathName(data[FILE]))
                    data[FILE] = data[FILE][0].upper() + data[FILE][1:]
                except Exception, e:
                    core.Log("error ", e, "on get long path name for ", data[FILE])
            # if (name in cls.modules and
            #     not cls.moduleData[name].file.lower().startswith(cls.erlangDir) and
            #     srcFile.lower().startswith(cls.erlangDir)):
            #     return
            mdata = ModuleData(name, data, f, app)
            if name.endswith(".hrl"):
                cls.includes[(app, name)] = mdata
            else:
                cls.modules[name] = mdata
            return mdata
        except  Exception, e:
            core.Log("load cache file error: ", f, e)
        return None

    @classmethod
    def TryLoad(cls, module):
        if cls.IsModuleLoaded(module):
            return True
        for m in cls.FindCacheFiles(module + ".cache"):
            cls.LoadFile_(m)
        return cls.IsModuleLoaded(module)

    @classmethod
    def FindCacheFiles(cls, fileName):
        matches = []
        for d in [cls.project.ProjectName(), os.path.join("runtimes", cls.project.GetErlangRuntime())]:

            for root, dirnames, filenames in os.walk(d):
                for filename in fnmatch.filter(filenames, fileName):
                    matches.append(os.path.join(root, filename))
        return matches

    @classmethod
    def IsModuleLoaded(cls, module):
        return module in cls.AllModules()

    @classmethod
    def UnloadFile(cls, f):
        if not os.path.isfile(f): return
        if not f.endswith(".cache"): return
        name = os.path.basename(f)[:-6]
        app = os.path.dirname(f)
        cls.Unload(name, app)
        os.remove(f)

    @classmethod
    def Unload(cls, name ,app):
        key = (app, name)
        if name in cls.modules:
            del cls.modules[name]
        elif key in cls.includes:
            del cls.includes[key]

    @classmethod
    def UnloadModule(cls, name, app):
        cls.Unload(name, app)

    @classmethod
    def UnloadCacheForFile(cls, f):
        cls.UnloadFile(f)

    @classmethod
    def AllModules(cls):
        return cls.modules.keys()

    @classmethod
    def AllIncludes(cls):
        return [name for (app, name) in cls.includes.keys()]

    @classmethod
    def GetDependentModules(cls, includePath):
        result = []
        app = core.Project.GetApp(includePath)
        include = (app, os.path.basename(includePath))
        cls.TryLoad(os.path.basename(includePath))
        for module in cls.modules.values():
            if not module.file.startswith(cls.project.projectDir): continue
            if include in module.AllIncludes():
                result.append(module.file)
        return result

    @classmethod
    def RecordFields(cls, module, record):
        data = cls.RecordData(module, record)
        if not data: return []
        return data.FieldsData()

    @classmethod
    def RecordData(cls, module, record):
        if not cls.TryLoad(module): return None
        for rec in cls.modules[module].AllRecords():
            if rec.name == record:
                return rec
        return None

    @classmethod
    def AllRecords(cls):
        result = []
        for include in cls.includes.values():
            result += include.AllRecords()
        return result

    @classmethod
    def AllRecordsData(cls, record):
        for include in cls.includes.values():
            for rec in include.AllRecords():
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
        return cls.modules[module].Functions(exported)

    @classmethod
    def ModuleFunction(cls, module, funName, arity):
        if not cls.TryLoad(module): return None
        funs = []
        for fun in cls.modules[module].functions:
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
        for typeData in cls.modules[module].exportedTypes:
            if typeData.name == type:
                return typeData
        for typeData in cls.ERLANG_TYPES:
            if typeData.name == type:
                return typeData
        return None

    @classmethod
    def ModuleCallbacks(cls, module):
        if not cls.TryLoad(module): return []
        return cls.modules[module].callbacks;

    @classmethod
    def ModuleExportedTypes(cls, module):
        if not cls.TryLoad(module): return []
        return cls.modules[module].Types()

    @classmethod
    def ModuleRecords(cls, module):
        if not cls.TryLoad(module): return []
        return cls.modules[module].AllRecords()

    @classmethod
    def Bifs(cls):
        module = "erlang"
        if not cls.TryLoad(module): return []
        return [fun for fun in cls.modules[module].Functions() if isinstance(fun, Function) and fun.bif == True]

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
        return cls.modules[module].AllMacroses()

    @classmethod
    def MacrosData(cls, module, macros):
        if not cls.TryLoad(module): return None
        for mac in cls.modules[module].AllMacroses():
            if mac.name == macros or mac.name.startswith(macros + "("):
                return mac
        return None

    @classmethod
    def ApplicationIncludes(cls, module):
        if not cls.TryLoad(module): return []
        #print "app inc", module
        app = cls.modules[module].Application()
        #print app
        #print cls.includes.keys()
        result = []
        for (incapp, inc) in cls.includes.keys():
            if incapp == app:
                result.append(inc + "\").")
        return result

    @classmethod
    def GlobalIncludes(cls):
        result = []
        for inc in cls.includes.keys():
            if cls.includes[inc].IsGlobalInclude():
                result.append(inc[0] + "/include/" + inc[1] + "\").")
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
