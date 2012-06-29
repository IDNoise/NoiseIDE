__author__ = 'Yaroslav'

import os

def Extension(path):
    name, ext = os.path.splitext(path)
    return ext