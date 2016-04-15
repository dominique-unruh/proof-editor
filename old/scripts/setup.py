import sys, glob, os
from cx_Freeze import setup, Executable

main_dir = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)),'..'))
print(main_dir)
os.chdir(main_dir)

# To be used for build_msi as a global application identifier
guid = '{568D16DF-405D-491E-BAD7-5DE914CF8A8E}'

# Suffix for executables (e.g., nothing or .exe)
exe_suffix = '' if sys.platform != 'win32' else '.exe'

# Location of the cmathml executable
cmathml = 'cmathml'+exe_suffix

# Location of the MathEdHaskell executable
MathEdHaskell = 'haskell/dist/build/MathEdHaskell/MathEdHaskell' + exe_suffix


include_files = [
    'resources',
    'abbrevs.txt', 
    cmathml,
    MathEdHaskell,
]


#include_files += [(x,x) for x in [
#    'mathview/mathview.js', 'mathview/mathview.css', 'mathview/mathpicture.css', 
#    'mathview/svg_convert.html',
#    'mathview/mathjax',
#]]

if sys.platform == 'win32':
#    include_files += ['cmathml.exe']
    include_files += glob.glob("*.dll") # Needed?
    include_files += ["C:/OCaml/bin/ocamlrun.exe"] # Needed?


print(include_files)

build_exe_options = {
    "packages": [
        'PyQt5.QtPrintSupport',
    ],
    'includes': [ 
        'lxml._elementpath', 'gzip', # for lxml.etree
    ],
    'include_files': include_files,
    'path': sys.path + [os.path.join(main_dir,'src')]
    }

build_msi_options = build_exe_options.copy().update({
    'upgrade_code' : guid,
    'add_to_path': False,
})

# GUI applications require a different base on Windows (the default is for a
# console application).
base = None
#if sys.platform == "win32":
#    base = "Win32GUI"

setup(  name = "mathed",
        version = "0.1",
        description = "My GUI application!",
        options = {"build_exe": build_exe_options},
        executables = [Executable("src/main.py", targetName="math-ed.exe", base=base)])
