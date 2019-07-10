import shutil
from idn_utils import writeFile, readFile

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

def zipdir(path, zipFile):
    filesToZip = []
    for root, dirs, files in os.walk(path):
        for fileToZip in files:
            filesToZip.append(os.path.join(root, fileToZip))
    [zipFile.write(filesToZip) for filesToZip in filesToZip]

def current_version():
    revCfg = os.path.join(os.getcwd(), "rev.cfg")
    version = 0.1
    if os.path.isfile(revCfg):
        data = readFile(revCfg)
        version = float(data.split("\n")[0].split(":")[1].strip())
    return version  
    
def CopyPackages():
    cwd = os.getcwd()
    dist_dir = 'dist'
    noiseide_dir = 'noiseide'

    file = 'NoiseIDE-' + str(current_version()) + '-win32.msi'
    new_file = 'NoiseIDE.msi'
    
    shutil.rmtree(noiseide_dir, ignore_errors = True)
    if not os.path.isdir(noiseide_dir):
        os.mkdir(noiseide_dir)
    shutil.copy('{}/{}'.format(dist_dir, file), '{}/{}'.format(noiseide_dir, new_file) )
    shutil.copy('rev.cfg', '{}/rev.cfg'.format(noiseide_dir) )
    dropboxdirs = ['d:/Downloads/Dropbox/NoiseIDEDist', 'e:/Downloads/Dropbox/NoiseIDEDist', 'c:/Users/IDNoise/Dropbox/NoiseIDEDist']
    for dropboxdir in dropboxdirs:
        if os.path.exists(dropboxdir):
            shutil.copy('{}/{}'.format(dist_dir, file), '{}/{}'.format(dropboxdir, new_file) )
            shutil.copy('rev.cfg', '{}/rev.cfg'.format(dropboxdir) )
    os.chdir(cwd)
    shutil.rmtree('dist', ignore_errors = True)
    shutil.rmtree('build', ignore_errors = True)
    

if __name__ == '__main__':
    CopyPackages()