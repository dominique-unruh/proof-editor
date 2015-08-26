{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Cmathml.TypesTest where
import Cmathml.Types


import Test.Framework
import Cmathml.Utils (bvarToCI, ciToBvar)
import Cmathml.QuickCheck()

{-# ANN module "HLint: ignore Use camelCase" #-}

-- TODO check whether QuickCheck's builtin handling of lists already splits the size for the subelements

{-
nonleaf :: Int -> Gen Cmathml
nonleaf size = do
    let semSize = size `div` 5
    let sizeHd = size `div` 5
    let sizeBvars = size `div` 5
    let sizeArgs = size-semSize-sizeHd
    let sizeArg = size-semSize-sizeBvars 
    sem <- semanticsGen semSize
    hd <- cmathml sizeHd
    args <- sized_list cmathml sizeArgs
    bvars <- sized_list bvar sizeBvars
    arg <- cmathml sizeArg
    oneof (map return [Apply sem hd args, Bind sem hd bvars arg])

bvar :: Int -> Gen Bvar
bvar size = do
    sem <- semanticsGen size
    name <- arbitrary
    return (sem,name)

cmathml :: Int -> Gen Cmathml
cmathml size = 
    if size<=1 then leaf size
    else oneof [ leaf size, nonleaf size ]

sized_list :: forall a. (Int -> Gen a) -> Int -> Gen [a]
sized_list gen size = do
    len :: Int <- choose (0, size `div` 2) -- TODO log size?
    let size1 = size `div` len
    replicateM len (gen size1)

semanticsGen :: Int -> Gen [(String, String, Annotation)]
semanticsGen = sized_list (\s -> do
    cd <- arbitrary
    name <- arbitrary
    ann <- cmathml s
    return (cd,name,ann))

leaf :: Int -> Gen Cmathml
leaf size = do
    sem <- semanticsGen (size-1)
    cd <- arbitrary
    name <- arbitrary
    str <- arbitrary
--    bytes <- arbitrary
    i <- arbitrary
    oneof (map return [ CN sem (Int i), CI sem name, CSymbol sem cd name, CS sem str]) -- TODO CError, CBytes 

instance Arbitrary Bvar where
    arbitrary = sized bvar
tmp :: Gen Bvar
tmp = arbitrary :: Gen Bvar -- to avoid "orphan instance" warning
-}

prop_ci_bvar :: Bvar -> Bool
prop_ci_bvar b = 
    b == ciToBvar (bvarToCI b)

    
test_plus :: IO ()
test_plus = assertEqual (1+2) (Apply[] (CSymbol [] "arith1" "plus") [CN[] (Int 1), CN[] (Int 2)])

test_minus :: IO ()
test_minus = assertEqual (1-2) (Apply[] (CSymbol [] "arith1" "minus") [CN[] (Int 1), CN[] (Int 2)])
