module Cmathml.Types 
    (Number(..), Cmathml(..), Semantics, Bvar, Annotation(..),
     Path, PathRange, )
where

import qualified Data.ByteString as B
import qualified Data.XML.Types
import qualified Data.Text

data Number = Int Integer | IEEE Double | Real Rational
    deriving (Eq, Show)
{- | Constraints: cd-name must be non-empty everywhere, 
     name of bvars and ci's must be nonempty
     whatever conditions CMML standard imposes on identifiers
-}
data Cmathml = 
  CN Semantics Number
  | CI Semantics String -- type can be added using annotations
  | CSymbol Semantics String String -- cd, name
  | CS Semantics String
  | Apply Semantics Cmathml [Cmathml]
  | Bind Semantics Cmathml [Bvar] Cmathml
  | CError Semantics String String [Cmathml] -- cd name contents 
  | CBytes Semantics B.ByteString
    deriving (Eq, Show)
type Semantics = [(String,String,Annotation)] -- cd name value
data Annotation = 
    AnnotationCMML Cmathml -- for encoding "MathML-Content"
    | AnnotationXML String [Data.XML.Types.Node] -- AnnotationXML encoding xml
    | AnnotationData String Data.Text.Text
    deriving (Eq, Show)
type Bvar = (Semantics,String)

{- | Describes a path within a formula (i.e., a pointer to a subterm).
     [] denotes the formula itself.
     -i:rest descends into the i-th semantics annotation (counting from 1). 
     In Apply, 0:rest descends into the head of the application, 1:i:rest into the i-th argument (counting from 0).
     In Bind, 0:rest descends into the head, 1:i:rest into the i-th bound variable, 2:rest into the argument.
-}
type Path = [Int]

{- | Describes a path towards a range of subterms (which must be siblings, i.e., 
     arguments of the same apply, or bound variables of the same binder.
     (path,i) points to the subterm pointed to by path, together with its i following siblings.
     (path,0) thus is equivalent to the path path.
-}
type PathRange = (Path,Int)

{- Local abbreviation -}
apply :: String -> String -> [Cmathml] -> Cmathml
apply cd name = Apply [] (CSymbol [] cd name)
        
instance Num Cmathml where
    fromInteger i = CN [] (Int i)
    abs x = apply "arith1" "abs" [x]
    signum _ = error "signum not implemented" -- TODO
    a * b = apply "arith1" "times" [a,b]
    a + b = apply "arith1" "plus" [a,b]
    a - b = apply "arith1" "minus" [a,b]
    negate _ = error "negate not implemented" -- TODO
    
instance Fractional Cmathml where
    a / b = apply "arith1" "divide" [a,b]
    recip _ = error "recip not implemented"
    fromRational r = CN [] (Real r)

instance Floating Cmathml where
    pi = CSymbol [] "nums1" "pi"
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

