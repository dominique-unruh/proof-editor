from haskell import lib
import glob

#libpaths = glob.glob("haskell/dist/build/libHSMathEdHaskell-0.1-*so")
#assert len(libpaths)==1
#lib = src.haskell_ffi.HaskellLib(libpaths[0])
config = lib.texDefaultConfiguration()
om = lib.texToOpenmath(config,"x+y")
pmmlConf = lib.pmmlDefaultConfiguration()
pmml = lib.toPmathml(pmmlConf,om)
print(pmml)
