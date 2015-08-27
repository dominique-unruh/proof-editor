module Transformations.Common where

import Control.Monad.Except (throwError, Except)
import Control.Monad (unless)
import Openmath.Types

type Error = Except String
type Transformation = [(Openmath,Maybe Path)] -> Error Openmath

assert :: Bool -> String -> Error ()
assert condition err = unless condition $ throwError err



