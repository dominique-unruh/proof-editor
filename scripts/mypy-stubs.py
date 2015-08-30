#!/usr/bin/python3

import os, subprocess, sys

#subprocess.check_call("rm -rf .mypy-stubs",shell=True)

def get_lib_file(lib):
    noimport = False
    if lib.find("(noimport)")!=-1:
        noimport = True
        lib = lib.replace("(noimport)","")
    lib = lib.rstrip()
    lib = lib.split(".")
    for i in range(len(lib)):
        dir = ".mypy-stubs/" + "/".join(lib[:i+1])
        os.makedirs(dir,exist_ok=True)
        file = dir+"/__init__.py"
        if not os.path.exists(file):
            #print("Creating {}".format(file))
            with open(file,'w') as f: pass
    with open(file,'w') as f:
        if noimport:
            f.write("import typing\n\n")
        else:
            f.write("from typing import *\n\n")
    return file

f = open(__file__,'rt')
out = open("/dev/null",'wt')

for l in f:
    if l.startswith("### "):
        filename = get_lib_file(l[4:])
        out.close()
        #print("Appending to {}".format(filename))
        out = open(filename,'at')
    else:
        out.write(l)

sys.exit(0)


### lxml.etree

### sip (noimport)

from typing import Any

class voidptr(): pass

def cast(obj:Any, typ:type) -> Any:
    pass

### logging.config

def dictConfig(config:Dict[str,Any]) -> None: pass

### logging

def info(str,*args:Union[str,int]) -> None: pass
def debug(str,*args:Union[str,int]) -> None: pass

### unittest.mock

### traceback

def print_exc(limit:int=None, file:IO[Any]=None, chain:bool=True) -> None: pass
