#!/usr/bin/python3

import os, sys, subprocess, re

haskelldir = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir,'haskell'))
haskelltestdir = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir,'haskell','src'))
print("Scanning",haskelltestdir)

outfile = open(os.path.join(haskelltestdir,"Test.hs"),"wt")

outfile.write("""{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

""")

ghc_decl = "{-# OPTIONS_GHC -F -pgmF htfpp #-}"
test_import = "import Test.Framework"

def isTestFile(p):
    with open(p) as f:
        lines = list(f)
    has_tests = any(l.startswith("prop_") or l.startswith("test_") for l in lines)
    has_decl = any(l.rstrip() == ghc_decl for l in lines)
    has_import = any(l.rstrip() == test_import for l in lines)
    if not has_tests: return False
    if not has_decl:
        print("Missing OPTIONS_GHC line in {}. Add:".format(p))
        print(ghc_decl)
    if not has_import:
        print("Missing import line in {}. Add:".format(p))
        print(test_import)
    if not has_decl or not has_import:
        sys.exit(1)
    return True

def raiseExn(e): raise e
def splitpath(p):
    if p==os.path.curdir: return []
    split = []
    while p != "":
        (p,a) = os.path.split(p)
        split.insert(0,a)
    return split

haskell_ext = set(['.hs'])
ignore_ext = set(['.ods','.xml'])


for (dirpath, _, filenames) in os.walk(haskelltestdir,onerror=raiseExn):
    dirpath_rel = os.path.relpath(dirpath,haskelltestdir)
    for filename in filenames:
        file = os.path.join(dirpath,filename)
        (base,ext) = os.path.splitext(filename)
        if ext.endswith("~") or ext in ignore_ext: continue
        if ext not in haskell_ext: raise RuntimeError("unknown extension "+ext)
        module = ".".join(splitpath(dirpath_rel)+[base])
        if module == "Test": continue
        if not isTestFile(file): 
            if re.match("Test\.hs$",file):
                raise RuntimeError("Module {} has no tests".format(module))
            continue
        outfile.write("import {{-@ HTF_TESTS @-}} {}\n".format(module))
        print("Module",module)
        

outfile.write("""
main :: IO()
main = htfMain htf_importedTests
""")

#subprocess.check_call("cabal test",cwd=haskelldir,shell=True)
