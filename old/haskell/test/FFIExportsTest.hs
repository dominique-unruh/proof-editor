{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell, BangPatterns, ForeignFunctionInterface #-}
module FFIExportsTest where

import Test.Framework
import FFIExports
--import Foreign.StablePtr (deRefStablePtr, newStablePtr)
--import Data.Dynamic (toDyn, fromDyn, typeOf, dynTypeRep)

{-# ANN module "HLint: ignore Use camelCase" #-}

f :: Int -> String -> Bool -> IO [Double]
f = undefined

exportFFI 'f

test_dummy :: IO ()
test_dummy = return ()

----pair_dynamic :: (Typeable a, Typeable b) => a -> b -> IO ()
--test_pair_dynamic :: IO ()
--test_pair_dynamic = do
--    let a = "hello"
--    let b = 1.3::Double
--    a' <- newStablePtr (toDyn a)
--    b' <- newStablePtr (toDyn b)
--    ab' <- pairDynamic a' b'
--    ab <- deRefStablePtr ab'
--    let ab'' = fromDyn ab (error "fromDyn")
--    assertEqual a (fst ab'')
--    assertEqual b (snd ab'')
--    assertEqual (typeOf (a,b)) (dynTypeRep ab)
