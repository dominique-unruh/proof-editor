{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Cmathml.XmlTest where
import Cmathml.Xml
import Test.Framework
import Cmathml.Types
import Cmathml.QuickCheck()

{-# ANN module "HLint: ignore Use camelCase" #-}

ci :: String -> Cmathml
ci = CI []

apply :: String -> String -> [Cmathml] -> Cmathml
apply cd name = Apply [] (CSymbol [] cd name)

test_trailing_white :: IO ()
test_trailing_white = do
    let xml = "<apply><csymbol cd=\"relation1\">eq</csymbol><ci>u</ci><ci>v</ci></apply>\n"
    putStr $ "["++xml++"]"
    let cmathml = cmathmlFromXML xml
    let expect = apply "relation1" "eq" [ci "u", ci "v"]
    assertEqual expect cmathml
    
prop_xml_roundtrip :: Cmathml -> Bool
prop_xml_roundtrip cmathml = (cmathmlFromXML (cmathmlToXML cmathml) == cmathml)

prop_double_roundtrip :: Double -> Bool
prop_double_roundtrip r = (read (show r) == r)

