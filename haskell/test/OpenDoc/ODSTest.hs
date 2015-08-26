{-# OPTIONS_GHC -F -pgmF htfpp #-}
module OpenDoc.ODSTest where

import OpenDoc.ODS
import Test.Framework

test_loadfile :: IO ()
test_loadfile = do
  ods <- odsFromFile "test/OpenDoc/test.ods"
  let sheet1 = (sheets ods) !! 0
  let cells1 = cells sheet1
  let cells1_str = map (map cellText) cells1
  assertEqual [["a","b","1"],["c","c","c"]]
              cells1_str
