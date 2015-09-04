{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Openmath.CmathmlTest
import {-@ HTF_TESTS @-} Openmath.PmathmlTest
import {-@ HTF_TESTS @-} Openmath.TeXTest
import {-@ HTF_TESTS @-} Openmath.TypesTest
import {-@ HTF_TESTS @-} Openmath.UtilsTest
import {-@ HTF_TESTS @-} OpenDoc.ODSTest
import {-@ HTF_TESTS @-} UserError.UserErrorTest
import {-@ HTF_TESTS @-} Transformations.CommutativityTest
import {-@ HTF_TESTS @-} Transformations.ComputeTest

main :: IO()
main = htfMain htf_importedTests
