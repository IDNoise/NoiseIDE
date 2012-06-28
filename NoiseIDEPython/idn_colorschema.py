__author__ = 'Yaroslav Nikityshev aka IDNoise'

import yaml
import os

class ColorSchema:
    @classmethod
    def load(cls, theme):
        path = os.path.join(os.getcwd(), "{}.color.yaml".format(theme))
        stream = file(path, 'r')
        cls.schemaFile = yaml.load(stream)
        cls.codeEditor = cls.schemaFile["code_editor"]
        cls.codeFormats = cls.codeEditor["formats"]