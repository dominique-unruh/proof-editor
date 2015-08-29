{-# LANGUAGE ScopedTypeVariables #-}
module Transformations.ModusPonens (modusPonens) where

import Transformations.Common
import Openmath.Types
import Control.Monad.Except (throwError)
import Data.Maybe (isNothing)
import Openmath.Utils (equivalentTerms)

modusPonens :: Transformation
modusPonens args = do
    assert (length args == 2) "The transformation needs exactly two arguments"
    let [(arg1,path1),(arg2,path2)] = args
    assert (isNothing path1 && isNothing path2) "Do not selected any subterms with this transformation."
    (prem,concl) <- case arg1 of
            OMA _ (OMS _ "logic1" "implies") [p,c] -> return (p,c)
            _ -> throwError "The first formula must be an implication (e.g., A=>B)"
    -- TODO: be more flexible with the equality (e.g., modulo AC)
    assert (equivalentTerms prem arg2) "Second formula must match premise of first formula.\nE.g. A=>B and A,  not A=>B and C."
    return concl
