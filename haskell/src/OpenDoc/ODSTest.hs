{-# OPTIONS_GHC -F -pgmF htfpp #-}
module OpenDoc.ODSTest where

import OpenDoc.ODS
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_loadfile :: IO ()
test_loadfile = do
  ods <- odsFromFile "src/OpenDoc/test.ods"
  let sheet1 = head (sheets ods)
  let cells1 = cells sheet1
  let cells1_str = map (map cellText) cells1
  assertEqual [["a","","1"],["c","c","c"]]
              cells1_str
