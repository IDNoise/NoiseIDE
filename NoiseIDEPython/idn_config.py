__author__ = 'Yaroslav Nikityshev aka IDNoise'

import __builtin__
import os
import yaml

class Config:
    @classmethod
    def load(cls):
        path = os.path.join(os.getcwd(), "noiseide.yaml")
        stream = file(path, 'r')
        cls.data = yaml.load(stream)
        cls.languagePaths = cls.data["languages"]

    @classmethod
    def LanguageExec(cls, language):
        return cls.languagePaths[language]

    @classmethod
    def GetProp(cls, prop):
        return cls.data[prop]

    @classmethod
    def SetProp(cls, prop, value):
        cls.data[prop] = value

