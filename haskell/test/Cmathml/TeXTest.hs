{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Cmathml.TeXTest where

import Cmathml.TeX
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_plus :: IO ()
test_plus = 
    assertEqual (texToOpenmath "1+2") (1+2) 
        -- (Apply[] (CSymbol [] "arith1" "plus") [CN[] (Int 1), CN[] (Int 2)])

test_minus :: IO ()
test_minus = 
    assertEqual (texToOpenmath "1-2") (1-2)
       -- (Apply[] (CSymbol [] "arith1" "minus") [CN[] (Int 1), CN[] (Int 2)])
