{-| Conversion of PMML (as understood by MathQuill) to OpenMath and back -}
module PMML2Openmath (pmml2Openmath) where

import Openmath.Types
import Text.XML.Light
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Data.Functor.Identity
import Debug.Trace

data Token = Op String | Math Openmath
             deriving (Eq, Show)

strip :: String -> IO String
strip = return . T.unpack . T.strip . T.pack

--satisfy :: (Token -> Bool) -> Parsec [Token] () Token
--satisfy test = tokenPrim show (\pos _ _ -> pos) (\t -> if test t then Just t else Nothing)

operators :: OperatorTable [Token] () Identity Openmath
operators = [
 [prefix "\8722" "arith1" "minus" {-minus sign-}],

 [left "\x2062" {-invisible times-} "arith1" "times",
  left "\183" {-middot-} "arith1" "times", 
  left "/" "arith1" "divide"],

 [left "+" "arith1" "plus", 
  left "\8722" "arith1" "minus" {-minus sign-}]
 ]

prefix op cd name =
    Prefix (opToken op >> return (\t -> apply cd name [t]))
left op cd name =
    Infix (opToken op >> return (\t u -> apply cd name [t,u])) AssocLeft
opToken :: String -> Parsec [Token] () ()
opToken op = tokenPrim show (\pos _ _ -> pos) 
             (\t -> if t == Op op then Just() else Nothing)

mathToken = tokenPrim show (\pos _ _ -> pos) (\t -> case t of Math om -> Just om;  _ -> Nothing)



{- operators :: M.Map String (Int,String,String)
operators = M.fromList [
             ("-",(150,"arith1","minus")),
             ("+",(150,"arith1","plus")),
             ("\x2062",(200,"arith1","times"))
            ] -}

invisibleTimes = Op "\x2062"

-- TODO? Flatten mrow's
preprocess :: [Element] -> [Token]
preprocess els = reverse (pp els [])
    where pp [] toks = toks
          pp (el:els) toks | qName (elName el) == "mrow" =
                               pp ((onlyElems $ elContent el) ++ els) toks
          pp (el:els) toks = 
              let nextTok = pp' el in
              case (nextTok, toks) of
                (Math _, (Math _):_) -> pp els (nextTok:invisibleTimes:toks)
                _ -> pp els (nextTok:toks)
          pp' el = 
              let tag = qName $ elName el in
              if tag=="mo" then Op $ strContent el
              else Math $ pmml2Openmath el

pmml2Openmath :: Element -> Openmath
pmml2Openmath el = p2o (qName $ elName el) (onlyElems $ elContent el)
    where p2o "mi" _ = OMV [] $ strContent el
          p2o "mrow" cont = row cont
          p2o "mfrac" [num,deno] = 
              OMA[] (OMS[] "arith1" "divide") [pmml2Openmath num, pmml2Openmath deno]
          p2o "mfrac" _ = error ("p2o: mfrac has wrong number of operands")
          p2o "mfenced" cont =
              let Just open = attr "open" el in
              let Just close = attr "close" el in
              case (open,close) of
                ("(",")") -> row cont
                ("|","|") -> apply "arith1" "abs" [row cont]
                _ -> error ("Unsupported parentheses: '"++open++"', '"++close++"'")
              
          p2o "msup" [base, exp] = apply "arith1" "power" [pmml2Openmath base, pmml2Openmath exp]
          p2o "msup" _ = error ("p2o: msup has wrong number of operands")

          p2o "mn" _ = fromInteger $ read (strContent el)

          p2o "msqrt" cont = apply "arith1" "root" [row cont, fromInteger 2]

          p2o tag _ = error ("p2o: unsupported tag "++tag)

          attr name el = findAttr (unqual name) el
                      
          row cont = parseLinear $ preprocess $ cont

apply :: String -> String -> [Openmath] -> Openmath
apply cd name args = OMA [] (OMS [] cd name) args

parseLinear :: [Token] -> Openmath
parseLinear toks = 
    case runP exprParser () "" toks of
      Left err -> error ("parsing formula fragment: "++show err)
      Right math -> math
      where
        exprParser = buildExpressionParser operators mathToken <* eof


-- TODO: This ignores priorities!
-- parseLinear (Math m : Op op : toks) = 
--    case M.lookup op operators of
--      Nothing -> error ("Unknown operator '"++op++"'")
--      Just (_,cd,name) -> apply cd name [m, parseLinear toks]
-- parseLinear [Math m] = m
-- parseLinear [] = error "malformed math"


{-
type Op = (String,String)

data FormulaFragment = FF { pri :: Int, -- ^ priority of the operators
                            parts :: [(Openmath,Op)] -- ^ (rev [(a,+),(b,-),(c,*)]) interpreted as "a + b - c *" 
                          }

data ParserState = St {
      hd :: Maybe Openmath,
      frags :: [FormulaFragment]
    }
    {- Interpretation: A formula a+b*c+d^e would be split into
       head=e, frags=[d^, a+b*c+]
       where the fragments have decreasing priority
       -}

pmml2Openmath :: [Element] -> Openmath
pmml2Openmath elems = p2o elems

joinParserState :: ParserState -> Openmath
joinParserState St{hd=Just hd,frags} = join hd frags
    where join hd [] = hd
          join hd (f:fs) = join (joinFrag hd (parts f)) fs
          joinFrag hd [] = hd
          joinFrag hd (((opCd,opName),m):ps) = joinFrag (OMS [] opCd opName [m,hd]) ps
             -- This implicitly assumes right associativity

p2o :: [Element] -> ParserState -> ParserState
p2o [] st = st
p2o (el:els) st | (hd st==Nothing) =
                    let (el
    
-}
