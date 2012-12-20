__author__ = 'Yaroslav'

def IsBeam(file):
    return file.endswith(".beam")

def IsModule(file):
    return file.endswith(".erl")

def IsInclude(file):
    return file.endswith(".hrl")

def IsYrl(file):
    return file.endswith(".yrl")

def IsIgor(self, path):
    return path.endswith(".igor")
