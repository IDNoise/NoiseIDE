import os
import json
from idn_project import ErlangProject


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

class ErlangCache:
    @classmethod
    def LoadCacheFromDir(cls, dir):
        dir = os.path.join(ErlangProject.CACHE_DIR, dir)
        for file in os.listdir(dir):
            file = os.path.join(dir, file)

            cls.LoadFile(file)

    @classmethod
    def LoadFile(cls, file):
        if not os.path.isfile(file): return
        if not fileName.endswith(".cache"): return
        data = data = json.loads(readFile(file))
        name = os.path.basename(file)[:-6]
        #if name.endswith(".hrl.cache"):



class Function:
    pass

class ModuleData:
    @classmethod
    def fromDict(cls, module, data):
        moduleData = ModuleData()
        moduleData.file = data[FILE]
        moduleData.module = module
        moduleData.functions = []
        for funData in data[FUNS]:
            fun = Function()
            fun.name = funData[NAME]
            fun.arity = funData[ARITY]
            fun.params = funData[PARAMS]
            fun.types = funData[TYPES]
            fun.result = funData[RESULT]
            fun.docref = funData[DOCREF]
            fun.exported = funData[EXPORTED]
            fun.module = module
            moduleData.functions.append(fun)

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
 