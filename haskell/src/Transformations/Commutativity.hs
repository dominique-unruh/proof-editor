{-# LANGUAGE ScopedTypeVariables #-}
module Transformations.Commutativity (commutativity) where

import Control.Monad.Except
import Openmath.Types
import Openmath.Utils
import Data.Maybe (isJust, fromJust)
import Transformations.Common
import UserError.UserError (miniUserError)
import UserError.ErrorDB as UE

commutativeOps :: [(String, String)]
commutativeOps = map splitDot [
  "minmax1.min", "minmax1.max", "arith1.plus", "arith1.times", "relation1.eq",
  "logic1.equivalent", "logic1.or", "logic1.and", "relation1.neq"
  ]

commutativity :: Transformation
commutativity args = do
    assert (length args == 1) $ miniUserError "The transformation needs exactly one argument"
    let [(arg,path')] = args
    assert (isJust path') $ miniUserError "You need to select the subterm to be transformed (e.g., a term like a+b)"
    let path = fromJust path'
    let subterm = getSubterm arg path
    (sem,lhs,op,rhs) <- case subterm of
        OMA sem op [a,b] -> return (sem,a,op,b)
        _ -> throwError $ miniUserError "You should select a binary operation (e.g., a+b)"
    (cd,name) <- case op of
        OMS _ cd name -> return (cd,name)
        _ -> throwError $ miniUserError "The operation you selected should be a builtin symbol (e.g., a+b, not f(a,b) for your own f)"
    assert ((cd,name) `elem` commutativeOps) $ UE.commutativityNotCommutative Arg1 arg Path1 path Symbol op
            -- miniUserError "The operation you selected is not commutative (e.g, a+b is OK, a/b is not)"
    let newterm = OMA sem op [rhs,lhs]
    return (replaceSubterm arg path newterm)
