{-# LANGUAGE CPP,FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, BangPatterns, ParallelListComp, RecordWildCards #-}
module FFIExports (exportFFI) where

#ifdef __GHCJS__

import Language.Haskell.TH.Syntax
exportFFI :: Name -> Q [Dec]
exportFFI _ = return []

#else
import Language.Haskell.TH.Syntax
import Foreign.StablePtr (deRefStablePtr, newStablePtr, StablePtr)
import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)
import GHC.Foreign (newCString, peekCString)
import GHC.IO.Encoding.UTF8 (utf8)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (typeOf, Typeable)
import Text.JSON (JSValue, JSON(..), encJSDict, encode, JSON)
import Data.Dynamic (Dynamic, fromDyn, toDyn, Dynamic, dynTypeRep)
import Control.Exception (SomeException(..), Exception(..), catch)
import System.IO (stderr, hPutStr)
import Foreign.Marshal.Alloc (free)
--import Data.Monoid (Any(..))
--import Unsafe.Coerce (unsafeCoerce)

data FFITypeWrapper = FFITypeWrapper {
    wrapperName :: String,
    cType :: Q Type,
    toCType :: Exp -> Q Exp,
    fromCType :: Exp -> Q Exp
}


stablePtrWrapper :: Type -> FFITypeWrapper
stablePtrWrapper typ = FFITypeWrapper {
    wrapperName = "StablePtr",
    cType = [t| StablePtr Dynamic |],
    toCType = \n -> [| newStablePtr $ toDyn $(return n) |],
    fromCType = \n -> [| do
           !dyn <- deRefStablePtr $(return n)
           let err = error $ "invalid type inside StablePtr (expected "++
                             show(typeOf(undefined :: $(return typ)))++", got "++
                             show(dynTypeRep(dyn))++")"
           let !val = fromDyn dyn err :: $(return typ)
           return val |]
}
--stablePtrWrapper typ = FFITypeWrapper {
--    wrapperName = "StablePtr",
--    cType = AppT (ConT ''StablePtr) typ,
--    toCType = \n -> [| newStablePtr $(return n) |],
--    fromCType = \n -> [| deRefStablePtr $(return n) |]
--}

stringWrapper :: FFITypeWrapper
stringWrapper = FFITypeWrapper {
    wrapperName = "String",
    cType = [t| CString |],
    toCType = \n -> [| newCString utf8 $(return n) |],
    fromCType = \n -> [| peekCString utf8 $(return n) |]
}

storableWrapper :: Type -> FFITypeWrapper
storableWrapper typ = FFITypeWrapper {
    wrapperName = "Storable",
    cType = return typ,
    toCType = \n -> [| return $(return n) |],
    fromCType = \n -> [| return $(return n) |]
    }

defaultWrapper :: Type -> FFITypeWrapper
defaultWrapper (ConT str) | str == ''String = stringWrapper
defaultWrapper (t @ (ConT tc)) | tc==''Int || tc==''Bool
    = storableWrapper t
defaultWrapper t = stablePtrWrapper t

data FFIInfo = FFIInfo {
    returnTypeWrapped :: Type,
    returnWrapper :: String,
    returnTypeOrig :: Type, -- ^ the original return type, but without the outermost ''IO''
    argTypesOrig :: [Type],
    argTypesWrapped :: [Type],
    argumentWrappers :: [String]
}
    deriving (Typeable, Show, Eq)

instance ThLiteral FFIInfo where
    thLiteral (FFIInfo {..}) = [|| FFIInfo{
        returnTypeWrapped = $$(thLiteral returnTypeWrapped),
        argTypesWrapped = $$(thLiteral argTypesWrapped),
        argumentWrappers = $$(thLiteral argumentWrappers),
        returnWrapper = $$(thLiteral returnWrapper),
        returnTypeOrig = $$(thLiteral returnTypeOrig),
        argTypesOrig = $$(thLiteral argTypesOrig)
        } ||]


instance ThLiteral Int where
    thLiteral i = unsafeTExpCoerce $ return $ LitE $ IntegerL (fromIntegral i)

instance {-# OVERLAPPING #-} ThLiteral String where
    thLiteral s = unsafeTExpCoerce $ return $ LitE $ StringL s

instance ThLiteral Type where
    thLiteral (ConT name) = [|| ConT $$(thLiteral name) ||]
    thLiteral (AppT t1 t2) = [|| AppT $$(thLiteral t1) $$(thLiteral t2) ||]
    thLiteral ArrowT = [|| ArrowT ||]
    thLiteral (TupleT i) = [|| TupleT $$(thLiteral i) ||]
    thLiteral ListT = [|| ListT ||]
    thLiteral (ForallT tyvars ctx typ) = [|| ForallT $$(thLiteral tyvars) $$(thLiteral ctx) $$(thLiteral typ) ||]
    thLiteral (SigT typ kind) = [|| SigT $$(thLiteral typ) $$(thLiteral kind) ||]
    thLiteral (VarT v) = [|| VarT $$(thLiteral v) ||]
    thLiteral (PromotedT n) = [|| PromotedT $$(thLiteral n) ||]
    thLiteral (UnboxedTupleT i) = [|| UnboxedTupleT $$(thLiteral i) ||]
    thLiteral EqualityT = [|| EqualityT ||]
    thLiteral (PromotedTupleT i) = [|| PromotedTupleT $$(thLiteral i) ||]
    thLiteral PromotedNilT = [|| PromotedNilT ||]
    thLiteral StarT = [|| StarT ||]
    thLiteral (LitT l) = [|| LitT $$(thLiteral l) ||]
    thLiteral PromotedConsT = [|| PromotedConsT ||]
    thLiteral ConstraintT = [|| ConstraintT ||]


instance ThLiteral TyLit where
    thLiteral (NumTyLit i) = [|| NumTyLit $$(thLiteral i) ||]
    thLiteral (StrTyLit str) = [|| StrTyLit $$(thLiteral str) ||]

instance ThLiteral TyVarBndr where
    thLiteral (PlainTV name) = [|| PlainTV $$(thLiteral name) ||]
    thLiteral (KindedTV name kind) = [|| KindedTV $$(thLiteral name) $$(thLiteral kind) ||]


instance ThLiteral Integer where
    thLiteral i = [|| $$(unsafeTExpCoerce $ return $ LitE (IntegerL i)) :: Integer ||]


instance ThLiteral a => ThLiteral [a] where
    thLiteral list = unsafeTExpCoerce $ do literals <- mapM (unTypeQ.thLiteral) list; return $ ListE literals

instance ThLiteral Name where
    thLiteral (Name occ flavour) = [|| Name $$(thLiteral occ) $$(thLiteral flavour) ||]

instance ThLiteral OccName where
    thLiteral (OccName str) = [|| OccName $$(thLiteral str) ||]

instance ThLiteral NameFlavour where
    thLiteral NameS = [|| NameS ||]
    thLiteral (NameQ modname) = [|| NameQ $$(thLiteral modname) ||]
    thLiteral (NameU i) = [|| NameU $$(thLiteral i) ||]
    thLiteral (NameL i) = [|| NameL $$(thLiteral i) ||]
    thLiteral (NameG ns pkg m) = [|| NameG $$(thLiteral ns) $$(thLiteral pkg) $$(thLiteral m) ||]

instance ThLiteral ModName where
    thLiteral (ModName str) = [|| ModName $$(thLiteral str) ||]

instance ThLiteral NameSpace where
    thLiteral VarName = [|| VarName ||]
    thLiteral DataName = [|| DataName ||]
    thLiteral TcClsName = [|| TcClsName ||]

instance ThLiteral PkgName where
    thLiteral (PkgName str) = [|| PkgName $$(thLiteral str) ||]

exportFFI :: Name -> Q [Dec]
exportFFI fun = do
    VarI _ typ _ _ <- qReify fun
    let (argTypes,returnType) = parseFunType typ

    -- Processing return
    let (isIO,rawReturnType) = case returnType of
            AppT (ConT io) r | io==''IO -> (True,r)
            r -> (False,r)
    let returnWrapper = defaultWrapper rawReturnType
    wrappedReturnType <- cType returnWrapper
    returnName <- qNewName "return"
    wrappedReturnName <- qNewName "return_wrapped"
    wrapReturn <- toCType returnWrapper (VarE returnName)
    let returnCode = [
            BindS (VarP wrappedReturnName) wrapReturn,
            NoBindS $ AppE (VarE 'return) (VarE wrappedReturnName)
            ]
    -- Processing arguments
    argNames <- mapM (\_ -> qNewName "x") argTypes
    let argWrappers = map defaultWrapper argTypes
    wrappedArgTypes <- mapM cType argWrappers
    wrappedArgNames <- mapM (\_ -> qNewName "wrapped") argTypes
    wrapArgs <- mapM (\(wrapper,x) -> fromCType wrapper (VarE x)) (zip argWrappers argNames)
    let wrapArgCode = [ BindS (VarP y) wr
                          | y <- wrappedArgNames | wr <- wrapArgs ]

    -- Processing function type
    let mappedFun' = mkName (nameBase fun ++ "_wrapped")
    let funType = foldr (\a b -> AppT (AppT ArrowT a) b) (AppT (ConT ''IO) wrappedReturnType) wrappedArgTypes
    let funSig = SigD mappedFun' funType
    let funExport = ForeignD $ ExportF CCall (nameBase fun) mappedFun' funType

    -- "let returnValue = f wrapped1 wrapped2 ..."
    let invokeF =
         if isIO then BindS (VarP returnName) (foldl AppE (VarE fun) (map VarE wrappedArgNames))
         else LetS [ ValD (VarP returnName) (NormalB $ foldl AppE (VarE fun) (map VarE wrappedArgNames)) [] ]

    let ffiInfoName = mkName (nameBase fun ++ "_ffiexport_info")
    let ffiInfoType = AppT (ConT ''StablePtr) (ConT ''FFIInfo)
    ffiInfo <- unTypeQ $ thLiteral FFIInfo {
        returnTypeWrapped = wrappedReturnType,
        argTypesWrapped = wrappedArgTypes,
        argumentWrappers = map wrapperName argWrappers,
        returnWrapper = wrapperName returnWrapper,
        returnTypeOrig = rawReturnType,
        argTypesOrig = argTypes
     }
    let ffiInfoCode = [ SigD ffiInfoName ffiInfoType,
                        PragmaD (InlineP ffiInfoName NoInline FunLike AllPhases),
                        ValD (VarP ffiInfoName) (NormalB $
                            AppE (VarE 'unsafePerformIO) $ AppE (VarE 'newStablePtr) ffiInfo) []
#ifndef NO_FOREIGN_EXPORT
                        , ForeignD $ ExportF CCall (nameBase ffiInfoName) ffiInfoName ffiInfoType
#endif
                      ]

    let doE = DoE $ wrapArgCode ++ [invokeF] ++ returnCode
    let funD = FunD mappedFun' [Clause [VarP x | x <- argNames] (NormalB doE) []]
    let decls = [funSig, funD
#ifndef NO_FOREIGN_EXPORT
                , funExport
#endif
                ] ++ ffiInfoCode

--    runIO (print $ ppr decls)
    return decls

parseFunType :: Type -> ([Type],Type)
parseFunType (AppT (AppT ArrowT a) b) =
    let (args,ret) = parseFunType b in
    (a:args, ret)
parseFunType t = ([],t)

class ThLiteral a where
    thLiteral :: a -> Q (TExp a)


ffiInfoToJSON :: StablePtr FFIInfo -> IO CString
ffiInfoToJSON info = catch (do
    info' <- deRefStablePtr info
    let json = Text.JSON.encode info'
--    putStr $ "JSON "++json++"\n"
    newCString utf8 json
    )
    (\e -> do
        hPutStr stderr $ "Uncaught exception in FFI (ffiInfoToJSON): "++displayException (e::SomeException)
        return nullPtr)


--{- Bad hack. According to http://stackoverflow.com/questions/10889682/how-to-apply-a-polymorphic-function-to-a-dynamic-value#comment52970066_10890414,
--   GHC 8 will have polymorphic typeable. Perhaps that will help. -}
--data UnsafeDynamic = UnsafeDynamic TypeRep Any
--pairDynamic :: StablePtr Dynamic -> StablePtr Dynamic -> IO (StablePtr Dynamic)
--pairDynamic a b = do
--    a' <- deRefStablePtr a
--    b' <- deRefStablePtr b
--    let (UnsafeDynamic aT aV) = unsafeCoerce a'
--    let (UnsafeDynamic bT bV) = unsafeCoerce b'
--    let pairTyCon = typeRepTyCon (typeOf (True,True))
--    let result = UnsafeDynamic (mkTyConApp pairTyCon [aT,bT]) $ unsafeCoerce (aV,bV)
--    newStablePtr $ unsafeCoerce result


#ifndef NO_FOREIGN_EXPORT
--foreign export ccall pairDynamic :: StablePtr Dynamic -> StablePtr Dynamic -> IO (StablePtr Dynamic)
foreign export ccall "freeCString" free :: CString -> IO ()
foreign export ccall ffiInfoToJSON :: StablePtr FFIInfo -> IO CString
#endif

instance JSON FFIInfo where
    readJSON _ = error "readJSON FFIInfo: not implemented"
    showJSON (FFIInfo{..}) = encJSDict [("returnWrapper", showJSON returnWrapper),
                                        ("argumentWrappers", showJSON argumentWrappers),
                                        ("returnTypeOrig", showJSONType returnTypeOrig),
                                        ("argTypesOrig", showJSON $ map showJSONType argTypesOrig)]

showJSONType :: Type -> JSValue
showJSONType (ConT name) = showJSON $ showName name
showJSONType (app @ AppT{}) = showJSON $ appList app []
        where appList (AppT a b) l = appList a ((showJSONType b):l)
              appList a l = (showJSONType a):l
showJSONType ListT = showJSON "List"
showJSONType (TupleT i) = showJSON $ "Tuple-"++show i
showJSONType t = error $ "not implemented: showJSON: "++show t


#endif

{-
foreign export ccall testFun :: Int -> Int -> Int
testFun :: Int -> Int -> Int
testFun a b = a*b

foreign export ccall releaseOpenmath :: StablePtr Openmath -> IO ()
releaseOpenmath :: StablePtr Openmath -> IO ()
releaseOpenmath = freeStablePtr
-}


{-
{- | Return value must be freed by caller -}
texToOpenmath :: CString -> IO (StablePtr Openmath)
texToOpenmath tex = do
    tex' <- peekCString utf8 tex
    config <- Openmath.TeX.texDefaultConfiguration
    let om = Openmath.TeX.texToOpenmath config tex'
    newStablePtr om
foreign export ccall texToOpenmath :: CString -> IO (StablePtr Openmath)


toPmathml :: Foreign.StablePtr.StablePtr Openmath.Pmathml.PMMLConfiguration -> Foreign.StablePtr.StablePtr Openmath.Types.Openmath -> IO Foreign.C.String.CString
foreign export ccall toPmathml :: Foreign.StablePtr.StablePtr Openmath.Pmathml.PMMLConfiguration -> Foreign.StablePtr.StablePtr Openmath.Types.Openmath -> IO Foreign.C.String.CString
toPmathml config math = do
    unwrapped_config <- deRefStablePtr config
    unwrapped_math <- deRefStablePtr math
    let result_value = Openmath.Pmathml.toPmathml unwrapped_config unwrapped_math
    newCString utf8 result_value
-}
