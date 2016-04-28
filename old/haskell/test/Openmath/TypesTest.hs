{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Openmath.TypesTest where
import Openmath.Types


import Test.Framework
import Openmath.QuickCheck()

{-# ANN module "HLint: ignore Use camelCase" #-}

test_plus :: IO ()
test_plus = assertEqual (1+2) (OMA[] (OMS [] "arith1" "plus") [OMI [] 1, OMI [] 2])

test_minus :: IO ()
test_minus = assertEqual (1-2) (OMA[] (OMS [] "arith1" "minus") [OMI [] 1, OMI [] 2])
