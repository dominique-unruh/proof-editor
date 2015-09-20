#!/usr/bin/python3

import os, re

rootdir = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir))
haskelldir = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir,'haskell'))
pkg_config = os.path.join(haskelldir,"dist","package.conf.inplace","MathEdHaskell-0.1-inplace.conf")

with open(pkg_config) as f:
    for line in f:
        line = line.rstrip()
        m = re.fullmatch(r"([a-zA-Z0-9-]+): (.*)",line)
        if not m: continue
        if m.group(1) == 'hs-libraries':
            libbasename = m.group(2)
        elif m.group(1) == 'library-dirs':
            libdir = m.group(2)

assert libdir, libdir
assert libbasename, libbasename

numfound = 0
for f in os.listdir(libdir):
    if f.startswith("lib"+libbasename) and \
       f.endswith(".so"):
        numfound += 1
        libname = f

assert numfound == 1

libpath = os.path.normpath(os.path.join(libdir,libname))
assert os.path.exists(libpath)

libpath = os.path.relpath(libpath,rootdir)

print("Symlinking {}".format(libpath))

target = os.path.join(rootdir,'libHSMathEdHaskell.so')
if os.path.exists(target): os.unlink(target)
os.symlink(libpath,target)
