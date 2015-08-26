module Cmathml.TeX where

import Cmathml.Types
import Text.TeXMath.Types
import Text.TeXMath.Readers.TeX (readTeX)

apply :: String -> String -> [Cmathml] -> Cmathml
apply cd name = Apply [] (CSymbol [] cd name)

texmathExpToCmml :: Exp -> Cmathml
texmathExpToCmml (ESuper a b) = apply "arith1" "power" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpToCmml (EIdentifier a) = CI [] a
texmathExpToCmml (ENumber s) = CN [] (Int (read s))
texmathExpToCmml (EFraction NormalFrac a b) = apply "arith1" "divide" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpToCmml (EGrouped math) = texmathExpsToCmml math
texmathExpToCmml math = error ("Could not parse math " ++ show math)

texmathExpsToCmml :: [Exp] -> Cmathml
texmathExpsToCmml [e] = texmathExpToCmml e
texmathExpsToCmml [a,ESymbol Bin "+",b] = apply "arith1" "plus" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpsToCmml [a,ESymbol Bin "\8722",b] = apply "arith1" "minus" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpsToCmml [a,ESymbol Bin "\8901",b] = apply "arith1" "times" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpsToCmml [a,ESymbol Rel "\8658",b] = apply "logic1" "implies" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpsToCmml [a,ESymbol Rel "=",b] = apply "relation1" "eq" [texmathExpToCmml a, texmathExpToCmml b]
texmathExpsToCmml [a,ESymbol Ord "/",b] = apply "arith1" "divide" [texmathExpToCmml a, texmathExpToCmml b]

texmathExpsToCmml math = error ("Could not parse math " ++ show math)

texToCmathml :: String -> Cmathml
texToCmathml tex = case readTeX tex of
    Right exps -> texmathExpsToCmml exps
    Left err -> error ("parsing tex: "++err) 
    
