__author__ = 'Yaroslav'

def IsBeam(path):
    return path.endswith(".beam")

def IsModule(path):
    return path.endswith(".erl")

def IsInclude(path):
    return path.endswith(".hrl")

def IsYrl(path):
    return path.endswith(".yrl")

def IsIgor(path):
    return path.endswith(".igor")

def IsAppSrc(path):
    return path.endswith(".app.src")
