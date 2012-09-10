import sys

from cx_Freeze import setup, Executable 

base = None
if sys.platform == "win32":
    base = "Win32GUI"

includefiles = ['dark.color.yaml', 
                'noiseide.template.yaml', 
                'data']
    
build_exe_options = {'include_files':includefiles}
    
setup(
        name = "NoiseIDE",
        version = "0.1",
        description = "NoiseIDE",
        options = {"build_exe": build_exe_options},
        executables = [Executable("idn_mainframe.py", base = base, targetName = "NoiseIDE.exe")])

