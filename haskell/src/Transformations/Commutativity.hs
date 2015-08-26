{-# LANGUAGE ScopedTypeVariables #-}
module Transformations.Commutativity where

import Control.Monad.Except
import Cmathml.Types
import Cmathml.Utils
import Data.Maybe (isJust, fromJust)
import Transformations.Common

commutativeOps :: [(String, String)]
commutativeOps = map splitDot [
  "minmax1.min", "minmax1.max", "arith1.plus", "arith1.times", "relation1.eq",
  "logic1.equivalent", "logic1.or", "logic1.and", "relation1.neq"
  ]

commutativity :: Transformation
commutativity args = do
    assert (length args == 1) "The transformation needs exactly one argument"
    let [(arg,path')] = args
    assert (isJust path') "You need to select the subterm to be transformed (e.g., a term like a+b)"
    let path = fromJust path'
    let subterm = getSubterm arg path 
    (sem,lhs,op,rhs) <- case subterm of
        Apply sem op [a,b] -> return (sem,a,op,b)
        _ -> throwError "You should select a binary operation (e.g., a+b)"
    (cd,name) <- case op of
        CSymbol _ cd name -> return (cd,name)
        _ -> throwError "The operation you selected should be a builtin symbol (e.g., a+b, not f(a,b) for your own f)"
    assert ((cd,name) `elem` commutativeOps) "The operation you selected is not commutative (e.g, a+b is OK, a/b is not)"
    let newterm = Apply sem op [rhs,lhs]
    return (replaceSubterm arg path newterm)
