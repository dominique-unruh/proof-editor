{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Openmath.PmathmlTest where

import Test.Framework
import Openmath.Types
import Openmath.Pmathml
import Misc
import qualified Text.XML as X

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


test_int :: IO ()
test_int = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config 1
    let res = xmlToString [] xml
    let expected = "<mn class=\"leaf number integer\" path=\"\" xmlns=\"http://www.w3.org/1998/Math/MathML\">1</mn>"
    assertEqual expected res

test_unknown_sym :: IO ()
test_unknown_sym = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config (OMS [] "unknown" "xxx")
    let res = xmlToString [] xml
    let expected = "<mi class=\"leaf symbol\" path=\"\" xmlns=\"http://www.w3.org/1998/Math/MathML\">unknown.xxx</mi>"
    assertEqual expected res

test_unknown_apply :: IO ()
test_unknown_apply = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMA [] (OMS [] "unknown" "xxx") [1,1]
    let res = xmlText xml
    let expect = "unknown.xxx\8289(1,1)"
    assertEqual expect res

test_known_sym :: IO ()
test_known_sym = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config (OMS [] "arith1" "plus")
    let res = xmlToString [] xml
    let expected = "<mo class=\"leaf symbol\" form=\"infix\" path=\"\" xmlns=\"http://www.w3.org/1998/Math/MathML\">+</mo>"
    assertEqual expected res -- "<mo form=\"infix\" xmlns=\"http://www.w3.org/1998/Math/MathML\">+</mo>" res

test_known_apply :: IO ()
test_known_apply = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMA [] (OMS [] "arith1" "times") [1,23]
    let res = xmlToString [] xml
    let expect = "<mrow class=\"apply\" path=\"\" xmlns=\"http://www.w3.org/1998/Math/MathML\"><mn class=\"leaf number integer\" path=\"1.0\">1</mn><mo class=\"leaf symbol\" form=\"infix\" path=\"0\">\8901</mo><mn class=\"leaf number integer\" path=\"1.1\">23</mn></mrow>"
    assertEqual expect res

test_forall :: IO ()
test_forall = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ OMBIND [] (OMS [] "quant1" "forall") [OMV [] "x"] (OMS [] "logic1" "true")
    let res = xmlText xml
    let expect = "\8704x.true"
    assertEqual expect res

test_parens1 :: IO ()
test_parens1 = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ (1+2)*3
    let res = xmlText xml
    let expect = "(1+2)\8901\&3"
    assertEqual expect res


test_parens2 :: IO ()
test_parens2 = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+2+3
    let res = xmlText xml
    let expect = "1+2+3"
    assertEqual expect res

test_parens3 :: IO ()
test_parens3 = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+(2+3)
    let res = xmlText xml
    let expect = "1+(2+3)"
    assertEqual expect res

test_parens4 :: IO ()
test_parens4 = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1+(2*3)
    let res = xmlText xml
    let expect = "1+2\8901\&3"
    assertEqual expect res

test_frac :: IO ()
test_frac = do
    let config = pmmlDefaultConfiguration
    let xml = pmmlRender config $ 1/2
    let text = xmlText xml
    assertEqual "12" text
    let tag = X.nameLocalName $ X.elementName xml
    assertEqual tag "mfrac"