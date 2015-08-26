#!/usr/bin/python3

import sys, os
import subprocess

def call(cmd,dir=None):
    print(cmd)
    subprocess.check_call (cmd,cwd=dir,shell=True)
    

os.chdir(os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir)))

call("cabal install --only-dependencies --enable-tests",'haskell')
call("cabal configure",'haskell')
call("cabal build",'haskell')
call("ocamlfind ocamlopt -package batteries,xml-light -linkpkg cmathml.mli cmathml.ml -g -o cmathml")
call("make resources/mathjax")
call("make resources/icons")
