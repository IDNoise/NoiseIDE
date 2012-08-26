__author__ = 'Yaroslav Nikityshev aka IDNoise'

import yaml
import os
from idn_global import GetMainFrame

class ColorSchema:
    @classmethod
    def load(cls, theme):
        path = os.path.join(GetMainFrame().cwd, "{}.color.yaml".format(theme))
        stream = file(path, 'r')
        cls.schemaFile = yaml.load(stream)
        cls.codeEditor = cls.schemaFile["code_editor"]
        cls.codeLanguages = cls.codeEditor["languages"]

    @classmethod
    def LanguageFormats(cls, language):
        return cls.codeLanguages[language]