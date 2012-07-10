import os
import json
from idn_config import Config
from idn_directoryinfo import DirectoryChecker

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
        cls.CACHE_DIR = os.path.join(os.getcwd(), "cache", "erlang")

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
            data[FILE] = os.path.normcase(win32api.GetLongPathName(data[FILE]))
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

class Cache():
    modules = set()
    moduleData = {}
    recordData = {}
    window = None
    
    @classmethod
    def loadCache(cls, subdir = None):
        dir = EditorConfig.cacheDir
        if subdir:
            dir = os.path.join(dir, subdir)
        for file in os.listdir(dir):
            fileName = os.path.join(dir, file)
            if not os.path.isfile(fileName): continue
            cls.loadFile(fileName)  
       
    @classmethod
    def loadFile(cls, fileName):
        if not fileName.endswith(".cache"): return
        if not os.path.exists(fileName): return
        file = os.path.basename(fileName)
        name = file[:-6]
        if (not '.' in name or name.endswith(".hrl")):
            data = json.loads(readFile(fileName))
            if not os.path.exists(data[FILE]): return
            if 'nt' == os.name:
                import win32api
                data[FILE] = os.path.normcase(win32api.GetLongPathName(data[FILE]))
            file = data[FILE]
            if (name in cls.modules and name in cls.moduleData and 
                cls.moduleData[name][FILE].lower().startswith(EditorConfig.erlang.lower()) and 
                file != cls.moduleData[name][FILE]): 
                log("Ignoring replace of cache for standart erlang " + 
                    "module: {}\n\tPath:{}".format(name, file))
                return
            
            if name[-4:] != ".hrl":
                cls.modules.add(name)
                
            cls.moduleData[name] = data
            log("Loading cache for: " + name)
     
    @classmethod
    def unload(cls, dir):
        dir = os.path.join(EditorConfig.cacheDir, dir)
        for file in os.listdir(dir):
            fileName = os.path.join(dir, file)
            if not os.path.isfile(fileName): continue
            if not fileName.endswith(".cache"): continue
            file = os.path.basename(fileName)
            name = file[:-6]
            if name.endswith("erl"):
                cls.modules.remove(name)
            del cls.moduleData[name]
      
    @classmethod
    def unloadCacheForFile(cls, file, dir = None):
        file = os.path.basename(fileName)
        if file.endswith(".erl"):
            cls.modules.remove(file)
        del cls.moduleData[file]
        path = EditorConfig.cacheDir + os.sep
        if dir:
            path = os.path.join(path, dir)
        path = os.path.join(path, file + ".cache")
        os.remove(path)
            
    @classmethod    
    def allModules(cls):
        return cls.modules
        
    @classmethod    
    def moduleFileData(cls, module):
        if module in cls.modules:
            return (cls.moduleData[module][FILE],  0)
        return None
        
    @classmethod
    def getDependentModules(cls, include):
        result = []
        for module in cls.moduleData:
            m = cls.moduleData[module]
            includes = m[INCLUDES]
            for inc in includes:
                if inc == include:
                    mod = m[FILE]
                    if mod.endswith(".erl"): 
                        if os.path.isfile(mod):
                            result.append(mod)
                        else:
                            if cls.window.project:
                                Cache.unloadCacheForFile(mod, 
                                    cls.window.project.projectName())
                            else:
                                Cache.unloadCacheForFile(mod)           
        return result
     
    @classmethod   
    def moduleFunctions(cls, module, exported = True, onlyNames = False):
        result = []
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if (exported and fun[EXPORTED] == exported) or\
                    not exported:
                    if onlyNames:
                        result.append(fun[NAME])
                    else:
                        result.append((fun[NAME], fun[ARITY], module, 
                            fun[PARAMS], fun[TYPES], fun[RESULT], fun[DOCREF])) 
        return result
    
    @classmethod
    def functionDocRef(cls, function, arity = None, module = None):
        variants = []
        if not module:
            module = ERLANG 
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if function == fun[NAME] and fun[DOCREF] != "":
                    if arity and fun[ARITY] == arity:
                        return fun[DOCREF]
                    variants.append(fun[DOCREF])
            if variants: 
                return variants[0]
        return None
        
    @classmethod
    def funData(cls, funTitle, arity = None, module = None):
        variants = []
        if not module:
            module = ERLANG
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if funTitle == fun[NAME]:
                    result = (cls.moduleData[module][FILE], fun[LINE])
                    if arity and fun[ARITY] == arity:
                        return result
                    variants.append(result)
            return variants[0] if variants else None
        return None
     
    @classmethod
    def functionInfo(cls, funTitle, arity = None, module = None):
        variants = []
        if not module:
            module = ERLANG 
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if funTitle == fun[NAME]:
                    result = (module, funTitle, fun[LINE], fun[PARAMS], fun[TYPES], fun[RESULT], fun[DOCREF])
                    if arity and fun[ARITY] == arity:
                        return result
                    variants.append(result)
            return variants[0] if variants else None
        return None
     
    @classmethod
    def funExportData(cls, module, line):
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for (i, fun) in enumerate(funs):
                if (line >= fun[LINE] and 
                    (i + 1 == len(funs) or line < funs[i+1][LINE])):
                    return (fun[NAME], fun[ARITY])
        return None 
     
    @classmethod  
    def functionHelp(cls, docref, window):
        if window.project:
            name = window.project.projectName()
            file = os.path.join(EditorConfig.cacheDir, name, docref)
            if os.path.isfile(file): 
                return readFile(file)
        file = os.path.join(EditorConfig.cacheDir, ERLANG, docref)
        if not os.path.isfile(file): return None
        return readFile(file)
     
    @classmethod
    def moduleExports(cls, module):
        result = []
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if fun[EXPORTED]:
                    result.append((fun[NAME], fun[ARITY]))
        return result
        
    @classmethod
    def bifs(cls):
        module = ERLANG
        result = []
        if module in cls.moduleData:
            funs = cls.moduleData[module][FUNS]
            for fun in funs:
                if fun[BIF] == True:
                    result.append((fun[NAME], fun[ARITY], module, fun[PARAMS], fun[TYPES], fun[RESULT], fun[DOCREF]))
        return result
        
    @classmethod  
    def moduleRecords(cls, module):
        result = []
        if module in cls.moduleData:
            recData = cls.moduleData[module][RECORDS_DATA]
            for record in recData:
                result.append((record, recData[record][FIELDS]))
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    recData = cls.moduleData[inc][RECORDS_DATA]
                    for record in recData:
                        result.append((record, recData[record][FIELDS]))
        return result
        
    @classmethod
    def recData(cls, module, record):
        if module in cls.moduleData:
            recData = cls.moduleData[module][RECORDS_DATA]
            if record in recData:
                return (cls.moduleData[module][FILE], recData[record][LINE])
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    recData = cls.moduleData[inc][RECORDS_DATA]
                    if record in recData:
                        return (cls.moduleData[inc][FILE],  recData[record][LINE])
        return None
        
    @classmethod  
    def recordInfo(cls, module, record):
        if module in cls.moduleData:
            recData = cls.moduleData[module][RECORDS_DATA]
            if record in recData:
                return (module, recData[record][FIELDS])
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    records = cls.moduleData[inc][RECORDS_DATA]
                    if record in records:
                        return (inc, records[record][FIELDS])
        return (None, None)    

    @classmethod  
    def macroses(cls, module):
        result = []
        if module in cls.moduleData:
            macroses = cls.moduleData[module][MACROS]
            for m in macroses:
                result.append((m, macroses[m][VALUE], ""))
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    macroses = cls.moduleData[inc][MACROS]
                    for m in macroses:
                        result.append((m, macroses[m][VALUE], ""))
        return result
        
    @classmethod
    def macData(cls, module, macros):
        if module in cls.moduleData:
            macroses = cls.moduleData[module][MACROS]
            if macros in macroses:
                return (cls.moduleData[module][FILE], macroses[macros][LINE])
            for mac in macroses:
                if mac.startswith(macros + "("):
                    return (cls.moduleData[module][FILE], macroses[mac][LINE])
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    macroses = cls.moduleData[inc][MACROS]
                    if macros in macroses:
                        return (cls.moduleData[inc][FILE],  macroses[macros][LINE])
                    for mac in macroses:
                        if mac.startswith(macros + "("):
                            return (cls.moduleData[inc][FILE],  macroses[mac][LINE])
        return None
        
    @classmethod
    def macInfo(cls, module, macros):
        if module in cls.moduleData:
            macroses = cls.moduleData[module][MACROS]
            if macros in macroses:
                return (module, macros, macroses[macros][VALUE])
            for mac in macroses:
                if mac.startswith(macros + "("):
                    return (module, mac, macroses[mac][VALUE])
            includes = cls.moduleData[module][INCLUDES]
            for inc in includes:
                if inc in cls.moduleData:
                    macroses = cls.moduleData[inc][MACROS]
                    if macros in macroses:
                        return (inc, macros, macroses[macros][VALUE])
                    for mac in macroses:
                        if mac.startswith(macros + "("):
                            return (inc, mac, macroses[mac][VALUE])
        return None    
 