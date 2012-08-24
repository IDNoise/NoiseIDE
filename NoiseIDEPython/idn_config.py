from idn_global import GetMainFrame

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import __builtin__
import os
import yaml

class Config:
    @classmethod
    def load(cls):
        path = os.path.join(GetMainFrame().cwd, "noiseide.yaml")
        stream = file(path, 'r')
        cls.data = yaml.load(stream)
        cls.languagePaths = cls.data["languages"]

    @classmethod
    def save(cls):
        path = os.path.join(GetMainFrame().cwd, "noiseide.yaml")
        stream = file(path, 'w')
        yaml.dump(cls.data, stream)

    @classmethod
    def LanguageExec(cls, language):
        return cls.languagePaths[language]

    @classmethod
    def GetProp(cls, prop):
        if prop in cls.data:
            return cls.data[prop]
        return None

    @classmethod
    def SetProp(cls, prop, value):
        cls.data[prop] = value

