{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Transformations.CommutativityTest where
import Transformations.Commutativity
import Test.Framework
import Cmathml.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Cmathml] -> Cmathml
apply cd name = Apply [] (CSymbol [] cd name)

int :: Integer -> Cmathml
int i = CN [] (Int i)


test_commutativity :: IO ()
test_commutativity = do
    let term = apply "arith1" "minus" [apply "arith1" "plus" [int 1,int 2], int 3]
    let path = [1,0]
    let res = commutativity [(term, Just path)]
    let expected = apply "arith1" "minus" [apply "arith1" "plus" [int 2,int 1], int 3]
    assertEqual res (return expected)

