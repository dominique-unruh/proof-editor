{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Transformations.ComputeTest where

import Transformations.Compute
import Test.Framework
import Openmath.Types
import Control.Monad.Except (runExcept)

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

test_compute :: IO ()
test_compute = do
    let term = apply "arith1" "minus" [apply "arith1" "plus" [1,2], 3]
    res <- assertRight $ runExcept $ compute [(term, Nothing)]
    assertEqual 0 res

test_compute2 :: IO ()
test_compute2 = do
    let term = apply "arith1" "times" [apply "arith1" "times" [1,2], 3]
    res <- assertRight $ runExcept $ compute [(term, Nothing)]
    assertEqual 6 res

