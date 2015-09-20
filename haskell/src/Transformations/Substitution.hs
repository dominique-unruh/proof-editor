{-# LANGUAGE TemplateHaskell #-}
module Transformations.Substitution (substitution) where

import Transformations.Common
import Openmath.Types
import Control.Monad.Except (throwError)
import Data.Maybe (isNothing, isJust, fromJust)
import Openmath.Utils (equivalentTerms, getSubterm, replaceSubterm)
import UserError.UserError (miniUserError)
import FFIExports (exportFFI)

substitution :: Transformation
substitution args = do
    assert (length args == 2) $ miniUserError "The transformation needs exactly two arguments"
    let [(arg1,path1),(arg2,path2)] = args
    assert (isJust path1) $ miniUserError "Please select the subterm to be replaced in the first formula"
    assert (isNothing path2) $ miniUserError "Do not select any subterms in the second formula"
    (lhs,rhs) <- case arg2 of
        OMA _ (OMS _ "relation1" "eq") [l,r] -> return (l,r)
        _ -> throwError $ miniUserError "The second formula must be an equality (e.g., A=B)"
    let subterm = getSubterm arg1 (fromJust path1)
    assert (equivalentTerms subterm lhs) $ miniUserError "The left hand side of the second formula must match the selected subterm in the first.\nE.g. \"...a...\" and \"a=b\"."
    return $ replaceSubterm arg1 (fromJust path1) rhs

exportFFI 'substitution
