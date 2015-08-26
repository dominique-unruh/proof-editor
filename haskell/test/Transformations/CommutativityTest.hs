{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Transformations.CommutativityTest where
import Transformations.Commutativity
import Test.Framework
import Cmathml.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

test_commutativity :: IO ()
test_commutativity = do
    let term = apply "arith1" "minus" [apply "arith1" "plus" [1,2], 3]
    let path = [1,0]
    let res = commutativity [(term, Just path)]
    let expected = apply "arith1" "minus" [apply "arith1" "plus" [2,1], 3]
    assertEqual res (return expected)

