{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} OpenDoc.ODSTest
import {-@ HTF_TESTS @-} Transformations.CommutativityTest
import {-@ HTF_TESTS @-} Transformations.ComputeTest
import {-@ HTF_TESTS @-} Cmathml.TypesTest
import {-@ HTF_TESTS @-} Cmathml.XmlTest
import {-@ HTF_TESTS @-} Cmathml.UtilsTest
import {-@ HTF_TESTS @-} Cmathml.TeXTest

main :: IO()
main = htfMain htf_importedTests
