{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Openmath.ZipperTest where

import Test.Framework
import Openmath.Zipper
import Openmath.Utils
import Openmath.Types
import Openmath.QuickCheck(omPathGenHalf)

prop_zipper_replace_twice :: OpenmathZipper -> Openmath -> Openmath -> Bool
prop_zipper_replace_twice zipper om1 om2 =
    getContent (substitute (substitute zipper om1) om2) == getContent (substitute zipper om2)

prop_zipper_getSubterm :: Openmath -> Property
prop_zipper_getSubterm om = forAll (omPathGenHalf om) (\path ->
    getSubterm om path == getFocussed (pathToZipper path om))
