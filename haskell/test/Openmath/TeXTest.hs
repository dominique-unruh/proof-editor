{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Openmath.TeXTest where

import Openmath.Types
import Openmath.TeX
import Test.Framework
--import Text.Parsec
import Test.QuickCheck.Monadic

--import Text.Parsec.Expr

{-# ANN module "HLint: ignore Use camelCase" #-}



--test_tokens :: IO ()
--test_tokens =
--  assertEqual ["1","+","\\ ","\\test","~"]
--                  (map tokenContent $ tokenize " 1+\\  \\test\n~\t")

test_plus :: IO ()
test_plus = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "1+2") (1+2)

--test_int_parser :: IO ()
--test_int_parser =
--  case parse intUnsigned "" (tokenize "1") of
--    Right i -> assertEqual 1 i
--    Left err -> fail $ show err

--test_atom_parser :: IO ()
--test_atom_parser =
--  config <- texDefaultConfiguration
--  case parse atom "" (tokenize "-1") of
--    Right i -> assertEqual (fromInteger $ -1) i
--    Left err -> fail $ show err

test_int1 :: IO ()
test_int1 = do
  config <- texDefaultConfiguration
  let i = 1 :: Integer
  let res = texToOpenmath config (show (-i))
  let expect = -(fromInteger i)
  assertEqual expect res

prop_int :: Integer -> Property
prop_int i = monadicIO $ do
    config <- run texDefaultConfiguration
    assert $ i<0 || texToOpenmath config (show i) == fromInteger i

test_plus_ab :: IO ()
test_plus_ab = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "a+b") (OMV [] "a" + OMV [] "b")

test_minus :: IO ()
test_minus = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "1-2") (1-2)

test_prec1 :: IO ()
test_prec1 = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "1*2+3") (1*2+3)

test_prec2 :: IO ()
test_prec2 = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "1+2*3") (1+2*3)

test_parens :: IO ()
test_parens = do
    config <- texDefaultConfiguration
    assertEqual (texToOpenmath config "(1+2)*3") ((1+2)*3)

--test_print_table :: IO ()
--test_print_table = print $ (map (map showOp)) $ grammarTableFromConfig testConfiguration
--    where showOp (Infix _ AssocLeft) = "InfixL"
--          showOp (Prefix _) = "Prefix"


test_leftovers :: IO ()
test_leftovers = do
    config <- texDefaultConfiguration
    case texToOpenmath' config "1=2=3" of
        Left _ -> return ()
        Right _ -> assertFailure "expected failure"
