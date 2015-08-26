{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Openmath.UtilsTest where

import Openmath.Utils
import Openmath.Types
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

apply :: String -> String -> [Openmath] -> Openmath
apply cd name args = OMA [] (OMS [] cd name) args

test_getSubterm :: IO ()
test_getSubterm = do
    let subterm = 2+3
    let term = 1+subterm
    let path = [1,1]
    assertEqual (getSubterm term path) subterm

test_replaceSubterm :: IO ()
test_replaceSubterm = do
    let subterm = 2-3 -- apply "arith1" "minus" [int 2,int 3] in
    let subterm2 = 2+3 -- apply "arith1" "plus" [int 2,int 3] in
    let term = 1+subterm -- apply "arith1" "plus" [int 1, subterm] in
    let newterm = 1+subterm2 -- apply "arith1" "plus" [int 1, subterm2] in
    let path = [1,1]
    assertEqual (replaceSubterm term path subterm2) newterm
    

