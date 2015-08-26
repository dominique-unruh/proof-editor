module Transformations.Common where

import Control.Monad.Except (throwError, Except)
import Control.Monad (unless)
import Cmathml.Types

type Error = Except String
type Transformation = [(Cmathml,Maybe Path)] -> Error Cmathml

assert :: Bool -> String -> Error ()
assert condition err = unless condition $ throwError err

splitDot :: String -> (String, String)
splitDot str = let (pfx,suffix) = break (=='.') str in (pfx,tail suffix)


