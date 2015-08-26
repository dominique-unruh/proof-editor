{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} OpenDoc.ODSTest
import {-@ HTF_TESTS @-} Transformations.ComputeTest
import {-@ HTF_TESTS @-} Transformations.CommutativityTest
import {-@ HTF_TESTS @-} Openmath.CmathmlTest
import {-@ HTF_TESTS @-} Openmath.UtilsTest
import {-@ HTF_TESTS @-} Openmath.TypesTest
import {-@ HTF_TESTS @-} Openmath.TeXTest

main :: IO()
main = htfMain htf_importedTests
