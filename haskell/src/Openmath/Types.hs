module Openmath.Types
    (Openmath(..), Attribution, Bvar, Attribute(..),
     Path, PathRange, )
where

import qualified Data.ByteString as B
import qualified Data.XML.Types
--import qualified Data.Text



type Foreign = (String,Data.XML.Types.Node) -- encoding, content
data Error = ErrorOM Openmath | ErrorForeign Foreign
  deriving (Eq, Show)
--data Number = Int Integer | IEEE Double | Real Rational
--    deriving (Eq, Show)
{- | Constraints: cd-name must be non-empty everywhere,
     name of bvars and ci's must be nonempty
     whatever conditions CMML standard imposes on identifiers
-}
data Openmath =
  OMI Attribution Integer -- integer
  | OMF Attribution Double -- floating point number
  | OMV Attribution String -- type can be added using annotations
  | OMS Attribution String String -- cd, name
  | OMSTR Attribution String
  | OMA Attribution Openmath [Openmath]
  | OMBIND Attribution Openmath [Bvar] Openmath
  | OME Attribution String String [Error] -- cd name contents
  | OMB Attribution B.ByteString
    deriving (Eq, Show)
type Attribution = [(String,String,Attribute)] -- cd name value
data Attribute =
    AttributeOM Openmath -- for encoding "MathML-Content"
    | AttributeForeign Foreign
    deriving (Eq, Show)
type Bvar = (Attribution,String)

{- | Describes a path within a formula (i.e., a pointer to a subterm).
     [] denotes the formula itself.
     -i:rest descends into the i-th semantics annotation (counting from 1).
     In OMA, 0:rest descends into the head of the application, 1:i:rest into the i-th argument (counting from 0).
     In OMBIND, 0:rest descends into the head, 1:i:rest into the i-th bound variable, 2:rest into the argument.
-}
type Path = [Int]

{- | Describes a path towards a range of subterms (which must be siblings, i.e.,
     arguments of the same apply, or bound variables of the same binder.
     (path,i) points to the subterm pointed to by path, together with its i following siblings.
     (path,0) thus is equivalent to the path path.
-}
type PathRange = (Path,Int)

{- Local abbreviation -}
apply :: String -> String -> [Openmath] -> Openmath
apply cd name = OMA [] (OMS [] cd name)

instance Num Openmath where
    fromInteger = OMI []
    abs x = apply "arith1" "abs" [x]
    signum _ = error "signum not implemented" -- TODO
    a * b = apply "arith1" "times" [a,b]
    a + b = apply "arith1" "plus" [a,b]
    a - b = apply "arith1" "minus" [a,b]
    negate a = apply "arith1" "unary_minus" [a]

instance Fractional Openmath where
    a / b = apply "arith1" "divide" [a,b]
    recip _ = error "recip not implemented"
    fromRational r = OMF [] (fromRational r) -- TODO: represent as a rational?

instance Floating Openmath where
    pi = OMS [] "nums1" "pi"
    exp x = apply "transc1" "exp" [x]
    sqrt x = apply "arith1" "root" [x,2]
    log _ = error "log" -- TODO
    a ** b = apply "arith1" "power" [a,b]
    logBase _ _ = error "logBase" -- TODO
    sin x = apply "transc1" "sin" [x]
    tan x = apply "transc1" "tan" [x]
    cos x = apply "transc1" "cos" [x]
    asin _ = error "asin"
    atan _ = error "atan"
    acos _ = error "acos"
    sinh x = apply "transc1" "sinh" [x]
    tanh x = apply "transc1" "tanh" [x]
    cosh x = apply "transc1" "cosh" [x]
    asinh _ = error "asinh"
    atanh _ = error "atanh"
    acosh _ = error "acosh"

