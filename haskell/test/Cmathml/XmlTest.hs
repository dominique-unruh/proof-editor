{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Cmathml.XmlTest where
import Cmathml.Xml
import Test.Framework
import Cmathml.Types
import Cmathml.QuickCheck()

{-# ANN module "HLint: ignore Use camelCase" #-}

omv :: String -> Openmath
omv = OMV []

apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

test_trailing_white :: IO ()
test_trailing_white = do
    let xml = "<apply><csymbol cd=\"relation1\">eq</csymbol><ci>u</ci><ci>v</ci></apply>\n"
    putStr $ "["++xml++"]"
    let cmathml = cmathmlFromXML xml
    let expect = apply "relation1" "eq" [omv "u", omv "v"]
    assertEqual expect cmathml


prop_xml_roundtrip :: Openmath -> Bool
prop_xml_roundtrip cmathml = (cmathmlFromXML (cmathmlToXML cmathml) == cmathml)


prop_double_roundtrip :: Double -> Bool
prop_double_roundtrip r = (read (show r) == r)

