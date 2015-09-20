module Transformations.Common where

import Control.Monad.Except (throwError, Except)
import Control.Monad (unless)
import Openmath.Types
import UserError.UserError

type Error = Except UserError

-- TODO remove
type Transformation = [(Openmath,Maybe Path)] -> Error Openmath

assert :: Bool -> UserError -> Error ()
assert condition err = unless condition $ throwError err

