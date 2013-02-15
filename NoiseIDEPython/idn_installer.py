import shutil
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

def GenerateUpdatePackages():
    cwd = os.getcwd()
    noiseide_dir = 'noiseide'
    noiseide_last = zipfile.ZipFile('noiseide_last.zip', 'w', zipfile.ZIP_DEFLATED)
    noiseide = zipfile.ZipFile('noiseide.zip', 'w', zipfile.ZIP_DEFLATED)

    os.chdir(noiseide_dir)
    zipdir('data', noiseide_last)
    zipdir('data', noiseide)
    for f in os.listdir(os.getcwd()):
        noiseide_last.write(f)
        if f not in ['NoiseIDE.exe', 'rev.cfg', 'python27.dll', 'noiseide_copy.bat']:
            noiseide.write(f)
    noiseide_last.close()
    noiseide.close()

    #zipdir('data', noiseide)
    #for f in ["noiseide_copy.bat", "dark.color.yaml", "noiseide.template.yaml", "white.color.yaml", "library.zip"]:
    #    noiseide.write(f)
    
    os.chdir(cwd)
    shutil.rmtree(noiseide_dir, ignore_errors = True)
    if not os.path.isdir(noiseide_dir):
        os.mkdir(noiseide_dir)
    shutil.move('noiseide_last.zip', '{}/noiseide_last.zip'.format(noiseide_dir))
    shutil.move('noiseide.zip', '{}/noiseide.zip'.format(noiseide_dir))
    shutil.copy('rev.cfg', '{}/rev.cfg'.format(noiseide_dir))

if __name__ == '__main__':
    GenerateUpdatePackages()