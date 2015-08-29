module Transformations.Common where

import Control.Monad.Except (throwError, Except)
import Control.Monad (unless)
import Openmath.Types
import UserError

type Error = Except UserError
type Transformation = [(Openmath,Maybe Path)] -> Error Openmath

assert :: Bool -> UserError -> Error ()
assert condition err = unless condition $ throwError err



