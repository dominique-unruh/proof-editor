{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Transformations.ComputeTest where

import Transformations.Compute
import Test.Framework
import Cmathml.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Cmathml] -> Cmathml
apply cd name = Apply [] (CSymbol [] cd name)

int :: Integer -> Cmathml
int i = CN [] (Int i)


test_compute :: IO ()
test_compute = do
    let term = apply "arith1" "minus" [apply "arith1" "plus" [int 1,int 2], int 3]
    let res = compute [(term, Nothing)]
    let expected = 0
    assertEqual (return expected) res

test_compute2 :: IO ()
test_compute2 = do
    let term = apply "arith1" "times" [apply "arith1" "times" [int 1,int 2], int 3]
    let res = compute [(term, Nothing)]
    let expected = 6
    assertEqual (return expected) res

