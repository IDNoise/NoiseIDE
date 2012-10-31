from idn_global import Log

__author__ = 'Yaroslav'
import zipfile
import os.path

def Install():
    zfile = zipfile.ZipFile("installer.zip")
    for name in zfile.namelist():
        try:
            (dirname, filename) = os.path.split(name)
            Log("Decompressing " + filename + " on " + dirname)
            if dirname and not os.path.exists(dirname):
                os.mkdir(dirname)
            if filename:
                fd = open(name,"wb")
                fd.write(zfile.read(name))
                fd.close()
        except Exception, e:
            Log(e)


def CreateInstallArchive():
    pass