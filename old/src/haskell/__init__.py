import haskell.ffi  # @UnresolvedImport
import glob

# def findlib():
#     name = "libHSMathEdHaskell"
#     locs = glob.glob("haskell/dist/build/{}-*.so".format(name))
#     if len(locs)>1: raise IOError("Ambiguous Haskell library: {}".format(locs))
#     if locs: return locs[0]
#     raise IOError("Library {} not found".format(name))

lib = haskell.ffi.HaskellLib("./libHSMathEdHaskell.so")
