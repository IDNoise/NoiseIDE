from idn_utils import writeFile

__author__ = 'Yaroslav'
import zipfile
import os
import os.path

def Decompress(installerFile):
    zfile = zipfile.ZipFile(installerFile)

    for name in zfile.namelist():
        newName = os.path.join("installer", name)
        try:
            (dirname, filename) = os.path.split(newName)
            print "Decompressing " + filename + " on " + dirname
            if dirname and not os.path.exists(dirname):
                os.mkdir(dirname)
            if filename:
                fd = open(newName, "wb")
                fd.write(zfile.read(name))
                fd.close()
        except Exception, e:
            print e
    zfile.close()
    os.remove(installerFile)
    #writeFile("C:/install_log.txt", logData)

def CreateInstallArchive():
    pass