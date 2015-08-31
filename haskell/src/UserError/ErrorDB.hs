
module UserError.ErrorDB where

import UserError.UserError
import Openmath.Types

userErrorDB' :: String -> UserError
userErrorDB' = userErrorDB "resources/errors"

data Arg1 = Arg1
data Path1 = Path1
data Symbol = Symbol

commutativityNotCommutative :: Arg1->Openmath -> Path1->Path -> Symbol->Openmath -> UserError
commutativityNotCommutative   Arg1 arg1   Path1 path1   Symbol symbol =
    addErrorData "Arg1" arg1 $  addErrorData "Path1" path1 $  addErrorData "Symbol" symbol $ userErrorDB' "CommutativityNotCommutative"

