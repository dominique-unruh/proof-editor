{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Transformations.ComputeTest where

import Transformations.Compute
import Test.Framework
import Openmath.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

test_compute :: IO ()
test_compute = do
    let term = apply "arith1" "minus" [apply "arith1" "plus" [1,2], 3]
    let res = compute [(term, Nothing)]
    let expected = 0
    assertEqual (return expected) res

test_compute2 :: IO ()
test_compute2 = do
    let term = apply "arith1" "times" [apply "arith1" "times" [1,2], 3]
    let res = compute [(term, Nothing)]
    let expected = 6
    assertEqual (return expected) res

