{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Openmath.PmathmlTest where

import Test.Framework
import Openmath.Types
import Openmath.Pmathml
import Misc

{-# ANN module "HLint: ignore Use camelCase" #-}


test_int :: IO ()
test_int = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config 1
    let res = xmlToString [] xml
    assertEqual "<mn xmlns=\"http://www.w3.org/1998/Math/MathML\">1</mn>" res

test_unknown_sym :: IO ()
test_unknown_sym = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config (OMS [] "unknown" "xxx")
    let res = xmlToString [] xml
    assertEqual "<mi xmlns=\"http://www.w3.org/1998/Math/MathML\">unknown.xxx</mi>" res

test_unknown_apply :: IO ()
test_unknown_apply = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMA [] (OMS [] "unknown" "xxx") [1,1]
    let res = xmlToString [] xml
    let expect = "<mrow xmlns=\"http://www.w3.org/1998/Math/MathML\"><mi>unknown.xxx</mi><mo form=\"infix\">\x2061</mo><mrow><mo fence=\"true\">(</mo><mn>1</mn><mo separator=\"true\">,</mo><mn>1</mn><mo fence=\"true\">)</mo></mrow></mrow>"
    assertEqual expect res

test_known_sym :: IO ()
test_known_sym = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config (OMS [] "arith1" "plus")
    let res = xmlToString [] xml
    assertEqual "<mo form=\"infix\" xmlns=\"http://www.w3.org/1998/Math/MathML\">+</mo>" res

test_known_apply :: IO ()
test_known_apply = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMA [] (OMS [] "arith1" "times") [1,23]
    let res = xmlToString [] xml
    let expect = "<mrow xmlns=\"http://www.w3.org/1998/Math/MathML\"><mn>1</mn><mo form=\"infix\">\8901</mo><mn>23</mn></mrow>"
    assertEqual expect res

test_forall :: IO ()
test_forall = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMBIND [] (OMS [] "quant1" "forall") [([],"x")] (OMS [] "logic1" "true")
    let res = xmlText xml
    let expect = "\8704x.true"
    assertEqual expect res

test_parens1 :: IO ()
test_parens1 = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ (1+2)*3
    let res = xmlText xml
    let expect = "(1+2)\8901\&3"
    assertEqual expect res


test_parens2 :: IO ()
test_parens2 = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+2+3
    let res = xmlText xml
    let expect = "1+2+3"
    assertEqual expect res

test_parens3 :: IO ()
test_parens3 = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+(2+3)
    let res = xmlText xml
    let expect = "1+(2+3)"
    assertEqual expect res

test_parens4 :: IO ()
test_parens4 = do
    config <- pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+(2*3)
    let res = xmlText xml
    let expect = "1+2\8901\&3"
    assertEqual expect res
