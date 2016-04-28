#!/usr/bin/python3

# TODO delete me

from typing import List, Tuple
import re

usedPythonNames = dict()

class HaskellType(object):
    @classmethod
    def new(cls, tycon:str, tyarg:List['HaskellType']=[]) -> 'HaskellType':
        if tycon=="String":
            return HaskellTypeString()
        else:
            t = HaskellType()
            t.tycon = tycon
            t.tyarg = tyarg
            return t

    def __init__(self): # Don't call me
        self.tycon = None
        self.tyarg = []

    def wrappedType(self):
        return HaskellType.new('Foreign.StablePtr.StablePtr',[self])

    def haskellWrapper(self):
        return 'newStablePtr'
        
    def haskellUnwrapper(self):
        return 'deRefStablePtr'

    def pythonWrapper(self):
        return '@@@.pointer'

    def pythonWrapperRelease(self):
        return '# Nothing to release for @@@'

    def pythonUnwrapper(self):
        return "{}(@@@)".format(self.pythonName())

    def pythonName(self):
        tycon = re.sub("^.*\.","",self.tycon)
        if self.tyarg: 
            name = tycon+"_"+"_".join(t.pythonName for t in self.pyarg)
        else:
            name = tycon
        
        prev = usedPythonNames.get(name)
        if prev is not None and prev != self:
            raise ValueError("Collision of python type names: {} {}".format(self,prev))
        usedPythonNames[name] = self

        return name


    def __str__(self) -> str:
        return self.toString(parens=False)

    def toString(self,parens:bool) -> str:
        if not self.tyarg: return self.tycon
        ret = self.tycon + " " + " ".join(str(x) for x in self.tyarg)
        if parens: ret = "({})".format(ret)
        return ret

    def __eq__(self,o) -> bool:
        return (self.tycon == o.tycon) and (self.tyarg == o.tyarg)


class HaskellTypeString(HaskellType):
    def __init__(self):
        super().__init__()
        self.tycon = 'String'

    CString = HaskellType.new('Foreign.C.String.CString')

    def wrappedType(self):
        return self.CString

    def haskellWrapper(self):
        return 'newCString utf8'
        
    def haskellUnwrapper(self):
        return 'deRefStablePtr'

    def pythonWrapper(self):
        return '@@@' # TODO?

    def pythonWrapperRelease(self):
        return '# TODO' # TODO?

    def pythonUnwrapper(self) -> str:
        return "HaskellTypeString.toPython(@@@)"

    @classmethod
    def toPython(p:'pointer'):
        s = ctypes.string_at(p)
        haskellLib.free(p)
        return s

    def pythonName(self):
        return "str"




class FFIItem(object):
    def __init__(self, haskellname:str, cname:str, pythonname:str,
                 haskellparams:List[Tuple[str,HaskellType]], 
                 haskellreturn:HaskellType):
        self.haskellname = haskellname
        self.cname = cname
        self.pythonname = pythonname
        self.haskellparams = haskellparams
        self.haskellreturn = haskellreturn

    def haskellStub(self) -> str:
        ret = self.haskellreturn
        if ret.tycon=="IO":
            ret = ret.tyargs[0]
            isIO = True
        else:
            isIO = False
        params = [(n,t.wrappedType()) for (n,t) in self.haskellparams]
        typesig = " -> ".join(str(t) for (_,t) in 
                              params+[(None,HaskellType.new("IO",[ret.wrappedType()]))])
        stub = "{} :: {}\n".format(self.cname,typesig)
        stub += "foreign export ccall {} :: {}\n".format(self.cname,typesig)

        stub += "{} = do\n".format(' '.join([self.cname] +
                                            [n for n,t in params]))
        for (n,t) in self.haskellparams:
            stub += "    unwrapped_{} <- {} {}\n".format(n,t.haskellUnwrapper(),n)
        
        funcall = ' '.join([self.haskellname] +
                           ["unwrapped_"+n for n,t in params])
        if isIO: 
            stub += "    result_value <- {}\n".format(funcall)
        else:
            stub += "    let result_value = {}\n".format(funcall)

        stub += "    {} result_value\n".format(ret.haskellWrapper())

        return stub


    def pythonStub(self):
        params = [(n,t.pythonName()) for n,t in self.haskellparams]
        ret = self.haskellreturn
        if ret.tycon=="IO":
            ret = ret.tyargs[0]
            isIO = True
        else:
            isIO = False
        stub = "def {}({}) -> {}:\n".format(
            self.pythonname,
            ", ".join(n+":"+t for n,t in params),
            self.haskellreturn.pythonName())

        for n,t in self.haskellparams:
            stub += "    wrapped_{} = {}\n".format(
                n, t.pythonWrapper().replace("@@@",n))
        
        stub += "    wrapped_return_value = haskellLib.{}({})\n".format(
            self.cname,
            ", ".join("wrapped_"+n for n,t in params))

        for n,t in self.haskellparams:
            stub += "    {}\n".format(
                t.pythonWrapperRelease().replace("@@@",n))

        stub += "    return {}\n".format(
            ret.pythonUnwrapper().replace("@@@",'wrapped_return_value'))

        return stub
                                            

toPmathml = FFIItem(haskellname="Openmath.Pmathml.toPmathml",
                    cname="toPmathml",
                    pythonname="toPmathml",
                    haskellparams=[('config',HaskellType.new("Openmath.Pmathml.PMMLConfiguration")),
                                   ('math',HaskellType.new("Openmath.Types.Openmath"))],
                    haskellreturn=HaskellType.new("String"))

texToOpenmath = FFIItem(haskellname="Openmath.TeX.texToOpenmath",
                        cname="texToOpenmath",
                        pythonname="texToOpenmath",
                        haskellparams=[('tex',HaskellType.new("String"))],
                        haskellreturn=HaskellType.new("Openmath.Types.Openmath"))


print()
print(toPmathml.haskellStub())
print()
print(toPmathml.pythonStub())

print()
print(texToOpenmath.pythonStub())
