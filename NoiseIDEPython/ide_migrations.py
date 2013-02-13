

__author__ = 'Yaroslav'

import shutil
import wx
import core
from idn_cache import ErlangCache

def migration_65():
    wx.MessageBox("Applying migration to v0.65. Deleting cache. Please regenerate erlang and project(apps\deps) cache later.", "Migration notification")
    shutil.rmtree(ErlangCache.CacheDir(), True)
    core.Log("migration_65 success")

MIGRATIONS = {0.65 : migration_65}