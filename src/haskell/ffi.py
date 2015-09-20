from typing import Callable
from abc import ABCMeta, abstractmethod
import ctypes, json

import logging
import functools

class HaskellTypeWrapper(metaclass=ABCMeta):
    @abstractmethod
    def fromCType(self, x:object) -> object: pass
    @abstractmethod
    def toCType(self, x:object) -> object: pass
    def cleanup(self, y:object) -> None: pass


class HaskellPtr(tuple):
    def __new__(cls, lib:'HaskellLib', pointer):
        return tuple.__new__(cls, (pointer,lib))
        
    def __str__(self):
        return "StablePtr$"+str(self[0])

    def __del__(self):
        self[1]._lib.hs_free_stable_ptr(self[0])

class HaskellStablePtrWrapper(HaskellTypeWrapper):
    def fromCType(self, lib:'HaskellLib', x:object) -> HaskellPtr:
        return HaskellPtr(lib,x)

    def toCType(self, x:HaskellPtr) -> object:
        assert isinstance(x,HaskellPtr), x
        return x[0]

class HaskellBoolWrapper(HaskellTypeWrapper):
    def fromCType(self, lib:'HaskellLib', x:object) -> bool:
        return bool(x)

    def toCType(self, x:bool) -> object:
        return int(x)

class HaskellStringWrapper(HaskellTypeWrapper):
    def fromCType(self, lib:'HaskellLib', x:object) -> HaskellPtr:
        s = ctypes.string_at(x)
        lib._lib.freeCString(x)
        return s.decode("utf-8")

    def toCType(self, x:str) -> object:
        assert isinstance(x,str), x
        return ctypes.create_string_buffer(x.encode())

#     def cleanup(self, lib:'HaskellLib', x:object) -> None:
#         lib._lib.freeCString(x)

typeWrappers = {
    'StablePtr': HaskellStablePtrWrapper(),
    'String': HaskellStringWrapper(),
    'Bool': HaskellBoolWrapper(),
}

class HaskellLib(object):
    def __init__(self, libfile:str):
        self._lib = ctypes.CDLL(libfile)
        self._lib.hs_init(0,0)

    def getTypeWrapper(self, wrapper:str, typ) -> HaskellTypeWrapper:
        if wrapper in ("StablePtr",'String'): return typeWrappers[wrapper]
        if wrapper=="Storable": 
            if typ=='Bool': return typeWrappers[typ]
            elif typ=='GHC.Types.Bool': return typeWrappers['Bool']
        raise NotImplementedError("Unsupported wrapper: {} {}".format(wrapper,typ))

    def __getattr__(self, funName:str) -> Callable:
        logging.debug("Creating stub "+funName)

        stub = self._make_stub(funName)
        self.__setattr__(funName,stub)
        return stub

    def _make_stub(self, funName:str) -> Callable:
        ffiinfo_ptr = self._lib[funName+"_ffiexport_info"]()
        ffiinfo_json_ptr = self._lib.ffiInfoToJSON(ffiinfo_ptr)
        ffiinfo_json = ctypes.string_at(ffiinfo_json_ptr).decode("utf-8")
        self._lib.freeCString(ffiinfo_json_ptr)
        ffiinfo = json.loads(ffiinfo_json)

        fun = self._lib[funName]
        returnWrapper = self.getTypeWrapper(ffiinfo['returnWrapper'],ffiinfo['returnTypeOrig'])
        
        argumentWrappers = ffiinfo['argumentWrappers']
        argTypesOrig = ffiinfo['argTypesOrig']
        argnum = len(argumentWrappers)
    
        def getWrapper(i:int): 
            return self.getTypeWrapper(argumentWrappers[i],argTypesOrig[i])
        
        assert argnum>=0 

        doc = "{} (imported from Haskell)".format(funName)

        if argnum==0: 
            def stub():
                return returnWrapper.fromCType(self,fun())
        elif argnum==1:
            wrap0=getWrapper(0)
            def stub(x0):
                y0 = wrap0.toCType(x0)
                res = fun(y0)
                wrap0.cleanup(y0)
                return returnWrapper.fromCType(self,res)
        elif argnum==2:
            wrap0=getWrapper(0)
            wrap1=getWrapper(1)
            def stub(x0,x1):
                y0 = wrap0.toCType(x0)
                y1 = wrap1.toCType(x1)
                res = fun(y0,y1)
                wrap0.cleanup(y0)
                wrap1.cleanup(y1)
                return returnWrapper.fromCType(self,res)
        elif argnum==3:
            wrap0=getWrapper(0)
            wrap1=getWrapper(1)
            wrap2=getWrapper(2)
            def stub(x0,x1,x2):
                y0 = wrap0.toCType(x0)
                y1 = wrap1.toCType(x1)
                y2 = wrap2.toCType(x2)
                res = fun(y0,y1,y2)
                wrap0.cleanup(y0)
                wrap1.cleanup(y1)
                wrap2.cleanup(y2)
                return returnWrapper.fromCType(self,res)
        else:
            raise NotImplementedError("{}-ary functions".format(argnum))

        stub.__doc__ = doc
        stub.__name__ = funName
        stub.__qualname__ = "(haskell)."+funName
        return stub

