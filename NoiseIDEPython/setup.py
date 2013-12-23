import sys
import os
import os.path
from cx_Freeze import setup, Executable 
from idn_utils import readFile

base = None
if sys.platform == "win32":
    base = "Win32GUI"

includefiles = ['dark.color.yaml', 
                'white.color.yaml', 
                'noiseide_copy.bat', 
                'rev.cfg', 
                'noiseide.template.yaml', 
                'data/images',
                'data/erlang/modules/apps/noiseide/include',
                'data/erlang/modules/apps/noiseide/ebin',
                'data/erlang/modules/apps/noiseide/src/noiseide_api.erl',
                'data/erlang/templates',
				'data/erlang/ide_cmds.yaml',
				'data/erlang/ide_macros.yaml'
				]
    
build_exe_options = {'include_files':includefiles} #, 'build_exe' : 'noiseide'
    
company_name = "CoconutShavers"
product_name = "NoiseIDE"
    
bdist_msi_options = {
    'upgrade_code': '{ac7c7151-0f46-4061-a3b8-6321940165c0}',
    'add_to_path': False,
    'initial_target_dir': r'C:\Programming\%s\%s' % (company_name, product_name)#,
    #'build_exe' : 'noiseide'
    }

def current_version():
    revCfg = os.path.join(os.getcwd(), "rev.cfg")
    version = 0.1
    if os.path.isfile(revCfg):
        data = readFile(revCfg)
        version = float(data.split("\n")[0].split(":")[1].strip())
    return version    
    
setup(
        name = "NoiseIDE",
        version = str(current_version()),
        description = "NoiseIDE",
        options = {"bdist_msi": bdist_msi_options, "build_exe": build_exe_options},
        executables = [Executable("idn_mainframe.py",
            base = base,
            targetName = "NoiseIDE.exe",
            icon = "data/images/icon.ico")
        ])

