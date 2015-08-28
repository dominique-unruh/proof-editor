{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Openmath.TeXTest where

import Openmath.Types
import Openmath.TeX
import Test.Framework
import Text.Parsec

{-# ANN module "HLint: ignore Use camelCase" #-}


test_tokens :: IO ()
test_tokens =
  assertEqual ["1","+","\\ ","\\test","~"]
                  (map tokenContent $ tokenize " 1+\\  \\test\n~\t")

test_plus :: IO ()
test_plus =
    assertEqual (texToOpenmath "1+2") (1+2)

test_int_parser :: IO ()
test_int_parser =
  case parse intUnsigned "" (tokenize "1") of
    Right i -> assertEqual 1 i
    Left err -> fail $ show err

test_atom_parser :: IO ()
test_atom_parser =
  case parse atom "" (tokenize "-1") of
    Right i -> assertEqual (fromInteger $ -1) i
    Left err -> fail $ show err

test_int1 :: IO ()
test_int1 = do
  let i :: Integer = -1
  let res = texToOpenmath (show i)
  let expect = fromInteger i
  assertEqual expect res

prop_int :: Integer -> Bool
prop_int i = texToOpenmath (show i) == fromInteger i

test_plus_ab :: IO ()
test_plus_ab =
    assertEqual (texToOpenmath "a+b") (OMV [] "a" + OMV [] "b")

test_minus :: IO ()
test_minus =
    assertEqual (texToOpenmath "1-2") (1-2)

test_prec1 :: IO ()
test_prec1 =
    assertEqual (texToOpenmath "1*2+3") (1*2+3)

test_prec2 :: IO ()
test_prec2 =
    assertEqual (texToOpenmath "1+2*3") (1+2*3)
