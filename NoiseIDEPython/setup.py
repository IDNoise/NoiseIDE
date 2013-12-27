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
                'rev.cfg', 
                'noiseide.template.yaml', 
                'data/images',
                'data/erlang/modules/apps/noiseide/include',
                'data/erlang/modules/apps/noiseide/ebin',
                'data/erlang/modules/apps/noiseide/src/noiseide_api.erl',
                'data/erlang/templates',
				'data/erlang/ide_cmds.yaml',
				'data/erlang/ide_snippets.yaml'
				]
    
build_exe_options = {'include_files':includefiles} #, 'build_exe' : 'noiseide'
    
product_name = "NoiseIDE"

# Create a structure for the registry table
# This will create a value 'InstallDir' in the key 'HKEY_LOCAL_MACHINE\SOFTWARE\MyCo\hello'
registry_table = [('NoiseIDEKeyLM', 2, r'SOFTWARE\CoconutShavers\NoiseIDE', '*', None, 'TARGETDIR'),
        ('NoiseIDEInstallDir', 2, r'SOFTWARE\CoconutShavers\NoiseIDE', 'InstallDir', '[TARGETDIR]', 'TARGETDIR'),]
# A RegLocator table to find the install directory registry key when upgrading
reg_locator_table = [('NoiseIDEInstallDirLocate', 2, r'SOFTWARE\CoconutShavers\NoiseIDE', 'InstallDir', 0)]
# An AppSearch entry so that the MSI will search for previous installs
# and update the default install location
app_search_table = [('TARGETDIR', 'NoiseIDEInstallDirLocate')]

shortcut_table = [
    ("DesktopShortcut",        # Shortcut
     "DesktopFolder",          # Directory_
     "Noise IDE",              # Name
     "TARGETDIR",              # Component_
     "[TARGETDIR]NoiseIDE.exe",# Target
     None,                     # Arguments
     None,                     # Description
     None,                     # Hotkey
     None, # Icon
     None,                     # IconIndex
     None,                     # ShowCmd
     'TARGETDIR'               # WkDir
     )
    ]


# Now create the table dictionary
msi_data = {'Registry': registry_table, 'RegLocator': reg_locator_table, 'AppSearch': app_search_table, "Shortcut": shortcut_table}

bdist_msi_options = {
    'upgrade_code': '{ac7c7151-0f46-4061-a3b8-6321940165c0}',
    'add_to_path': False,
    'initial_target_dir': r'C:\Programming\%s' % (product_name),
    'data': msi_data
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

