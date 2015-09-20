{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns, PatternGuards, PatternSynonyms #-}
module Transformations.Compute (compute) where

import Transformations.Common
import Openmath.Utils (pattern Int', bindP, getSubterm, replaceSubterm, equivalentTerms, pattern OMASym)
import UserError.UserError (miniUserError)
import Openmath.Types
import Data.Maybe (fromMaybe)
import FFIExports (exportFFI)

intExp :: Integer -> Integer -> Integer
intExp 0 0 = error "0**0"
intExp _ 0 = 1
intExp 1 _ = 1
intExp _ i | i<0 = error "0**negative"
intExp x i = x * intExp x (i-1)

csTrue :: Openmath
csTrue = OMS [] "logic1" "true"
csFalse :: Openmath
csFalse = OMS [] "logic1" "false"

doCompute1 :: Openmath -> Openmath
-- x+0 -> x
doCompute1 (OMASym "arith1" "plus" [x, Int' 0]) = x
-- x-0 -> x
doCompute1 (OMASym "arith1" "minus" [x, Int' 0]) = x
-- 0+x -> x
doCompute1 (OMASym "arith1" "plus" [Int' 0, x]) = x
-- 0*x -> 0
doCompute1 (OMASym "arith1" "times" [Int' 0, _]) = 0
-- x*0 -> 0
doCompute1 (OMASym "arith1" "times" [_, Int' 0]) = 0
-- x*1 -> x
doCompute1 (OMASym "arith1" "times" [x, Int' 1]) = x
-- 1*x -> x
doCompute1 (OMASym "arith1" "times" [Int' 1, x]) = x
-- x*y -> [calc x*y]
doCompute1 (OMASym "arith1" "times" [Int' x, Int' y]) = fromIntegral $ x*y
-- x+y -> [calc x+y]
doCompute1 (OMASym "arith1" "plus" [Int' x, Int' y]) = fromIntegral $ x+y
-- x-y -> [calc x+y]
doCompute1 (OMASym "arith1" "minus" [Int' x, Int' y]) = fromIntegral $ x-y
-- sqrt(x) -> [calc...] if integer
doCompute1 (OMASym "arith1" "root" [Int' x, Int' base])
    | base >= 1, x >= 0, y <- floor((fromIntegral x::Double)**(1/fromIntegral base)), intExp y base==x
    = fromIntegral y
-- 0 / y -> 0
doCompute1 (OMASym "arith1" "divide" [Int' 0,_]) = 0
-- x / 1 -> x
doCompute1 (OMASym "arith1" "divide" [x, Int' 1]) = x
-- x / y -> calc
doCompute1 (OMASym "arith1" "divide" [Int' x, Int' y])
    | (q,0) <- quotRem x y
    = fromIntegral q
-- x**1 -> x
doCompute1 (OMASym "arith1" "power" [x, Int' 1]) = x
-- x**y -> calc
doCompute1 (OMASym "arith1" "power" [Int' x, Int' y])
    | y>=0, (x,y) /= (0,0)
    = fromIntegral $ intExp x y
-- x=x -> true
doCompute1 (OMASym "relation1" "eq"  [x,y])
    | equivalentTerms x y
    = csTrue
-- x = y -> false (for x,y ints and not equal)
doCompute1 (OMASym "relation1" "eq"  [Int' x, Int' y])
    | x /= y = csFalse
-- (true => x) -> x
doCompute1 (OMASym "logic1" "implies"  [OMS _ "logic1" "true", x]) = x
-- (x => true) -> true
doCompute1 (OMASym "logic1" "implies"  [_, OMS _ "logic1" "true"]) = csTrue
-- (false => x) -> true
doCompute1 (OMASym "logic1" "implies"  [OMS _ "logic1" "false", _]) = csTrue
-- (x => x) -> true
doCompute1 (OMASym "logic1" "implies"  [x,y])
    | equivalentTerms x y = csTrue
-- forall x. true  ->  true
doCompute1 (bindP -> Just ("quant1", "forall", _, OMS _ "logic1" "true")) = csTrue
-- exists x. true  ->  true # This assumes nonempty types!
doCompute1 (bindP -> Just ("quant1", "exists", _, OMS _ "logic1" "true")) = csTrue
-- forall x. false  ->  false # This assumes nonempty types!
doCompute1 (bindP -> Just ("quant1", "forall", _, OMS _ "logic1" "false")) = csFalse
-- exists x. false  ->  false
doCompute1 (bindP -> Just ("quant1", "exists", _, OMS _ "logic1" "false")) = csFalse


-- otherwise
doCompute1 math = math


doCompute :: Openmath -> Openmath
doCompute term =
    let term' = (case term of
            OMA sem hd args -> OMA sem (doCompute hd) (map doCompute args)
            OMBIND sem hd bvars arg -> OMBIND sem (doCompute hd) bvars (doCompute arg)
            _ -> term) in
    doCompute1 term'


compute :: Transformation
compute args = do
    assert (length args == 1) $ miniUserError "The transformation needs exactly one argument"
    let [(arg,path')] = args
    let path = fromMaybe [] path'
    let subterm = getSubterm arg path
    let newterm = doCompute subterm
    assert (newterm /= subterm) $ miniUserError "Could not find anything to compute"
    return $ replaceSubterm arg path newterm

exportFFI 'compute
