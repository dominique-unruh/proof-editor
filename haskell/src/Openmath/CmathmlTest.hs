{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Openmath.CmathmlTest where
import Openmath.Cmathml
import Test.Framework
import Openmath.Types
import Openmath.QuickCheck()

{-# ANN module "HLint: ignore Use camelCase" #-}

omv :: String -> Openmath
omv = OMV []

apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

test_trailing_white :: IO ()
test_trailing_white = do
    let xml = "<apply><csymbol cd=\"relation1\">eq</csymbol><ci>u</ci><ci>v</ci></apply>\n"
    putStr $ "["++xml++"]"
    let cmathml = fromCmathml xml
    let expect = apply "relation1" "eq" [omv "u", omv "v"]
    assertEqual expect cmathml


prop_xml_roundtrip :: Openmath -> Bool
prop_xml_roundtrip cmathml = fromCmathml (toCmathml cmathml) == cmathml


prop_double_roundtrip :: Double -> Bool
prop_double_roundtrip r = read (show r) == r
