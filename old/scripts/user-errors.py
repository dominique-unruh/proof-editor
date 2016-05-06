#!/usr/bin/python3

import os, re, sys
from lxml import etree

maindir = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.pardir))
errorsdir = os.path.join(maindir,'resources','errors')
errorDBhs = os.path.join(maindir,'haskell','src','UserError','ErrorDB.hs')

errorDBPreamble = """
module UserError.ErrorDB where

import UserError.UserError
import Openmath.Types

userErrorDB' :: String -> UserError
userErrorDB' = userErrorDB "resources/errors"

"""

fundeclTemplate = """{name} :: {paramTypes} -> UserError
{name}   {params} =
    {addargs}userErrorDB' "{nameCap}"

"""

# commutativityNotCommutative :: Arg1 -> Openmath -> Path1 -> Path -> Symbol -> Openmath -> UserError
# commutativityNotCommutative Arg1 arg1 Path1 path1 Symbol symbol =
#     addErrorData "Arg1" arg1 $
#     addErrorData "path1" path1 $
#     addErrorData "Symbol" symbol $
#     userErrorDB' "CommutativityNotCommutative"

# data Arg1 = Arg1
# data Path1 = Path1
# data Symbol = Symbol

# commutativityNotCommutative :: Arg1 -> Openmath -> Path1 -> Path -> Symbol -> Openmath -> UserError
# commutativityNotCommutative Arg1 arg1 Path1 path1 Symbol symbol =
#     addErrorData "Arg1" arg1 $
#     addErrorData "path1" path1 $
#     addErrorData "Symbol" symbol $
#     userErrorDB' "CommutativityNotCommutative"


def firstLower(str):
    return str[0].lower() + str[1:]

types = {'formula':'Openmath',
         'path':'Path'}
         

keywords = set()
functions = []
def parse_files(dir):
    for (path,dirs,files) in os.walk(dir):
        for file in files:
            m = re.fullmatch(r"ue(.*)\.xhtml",file)
            if not m and file!="erroredit.css" \
                     and file.endswith("~") \
                     and file.endswith(".bak"):
                raise RuntimeError("Unexpected file: {}".format(file))
            if not m: continue
            filepath = os.path.join(path,file)
            errorName = m.group(1)
            errorName2 = firstLower(errorName)
            xml = etree.parse(filepath)

            metadata = xml.findall('.//{urn:unruh:proofedit:usererror}metadata')
            if not metadata:
                raise RuntimeError("no metadata")
            if len(metadata)>1:
                raise RuntimeError(">1 metadata")
            metadata = metadata[0]
            if len(metadata):
                raise RuntimeError("metadata contains elements")
            metadata = metadata.text

            decls = {}
            params = []
            for decl in metadata.split(','):
                m = re.fullmatch(r"([a-zA-Z0-9]+)\s+([a-zA-Z0-9]+)",decl.strip())
                t,n = m.groups()
                decls[n] = t
                params.append((n,t))
                keywords.add(n)

            fun = fundeclTemplate.format(
                name=errorName2, nameCap=errorName,
                paramTypes=" -> ".join("{}->{}".format(n,types[t]) for n,t in params),
                params="   ".join("{} {}".format(n,firstLower(n)) for n,t in params),
                addargs=' '.join('addErrorData "{}" {} $ '.format(n,firstLower(n)) for n,t in params),
            )
            functions.append(fun)


def write_file(file):
    with open(file,'wt') as fh:
        fh.write(errorDBPreamble)
        for k in sorted(keywords):
            fh.write("data {} = {}\n".format(k,k))
        fh.write("\n")
        for f in functions:
            fh.write(f)
    

parse_files(errorsdir)
write_file(errorDBhs)
