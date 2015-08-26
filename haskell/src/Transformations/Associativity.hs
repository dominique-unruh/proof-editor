{-# LANGUAGE ScopedTypeVariables #-}
module Transformations.Associativity where

import Control.Monad.Except
import Cmathml.Types
import Cmathml.Utils
import Data.Maybe (isJust, fromJust)
import Transformations.Common


associativeOps :: [(String, String)]
associativeOps = map splitDot [
  "minmax1.min", "minmax1.max", "logic1.or", "logic1.and", "arith1.plus", "arith1.times"
  ]

checkOp :: Cmathml -> Cmathml -> Error ()
checkOp op1 op2 = do
    assert (op1 == op2) "You cannot apply associativity to an expression with two different operations.\nE.g. (a+b)+c is OK, and a*(b*c) is OK, but a+(b*c) is not OK." 
    cdName <- case op1 of
        CSymbol _ cd name -> return (cd,name)
        _ -> throwError "The operation you selected should be a builtin symbol (e.g., (a+b)+c, not f(a,f(b,c)) for your own f)"
    assert (cdName `elem` associativeOps) "The operation is not associative.\n(Associative operations are for example +, * but not -)"



associativity :: Transformation
associativity args = do
    assert (length args == 1) "The transformation needs exactly one argument"
    let [(arg,path')] = args
    assert (isJust path') "You need to select the subterm to be transformed (e.g., a term like (a+b)+c)"
    let path = fromJust path'
    let subterm = getSubterm arg path 
    newterm <- case subterm of
        Apply sem1 op2 [Apply sem2 op1 [a,b], c] -> do
            checkOp op1 op2
            return $ Apply sem1 op1 [a, Apply sem2 op2 [b,c]]
        Apply sem1 op1 [a, Apply sem2 op2 [b,c]] -> do
            checkOp op1 op2
            return $ Apply sem1 op2 [Apply sem2 op1 [a,b], c]
        _ -> throwError "Select a subexpression of the form (a+b)+c or a+(b+c) where + is some associative operation"
    return (replaceSubterm arg path newterm)
