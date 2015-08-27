{-# LANGUAGE PartialTypeSignatures #-}
module Openmath.TeX where

import Openmath.Types
import Openmath.Utils (splitDot)
-- import Text.TeXMath.Types
-- import Text.TeXMath.Readers.TeX (readTeX)
import Text.Parsec hiding (digit)
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)

data TexToken = TexToken { tokenContent::String, tokenSource::String, tokenPos::SourcePos }

macroToken :: Parsec String () TexToken
macroToken = do
  pos <- getPosition
  pre <- many space
  _   <- char '\\'
  str <- many1 letter <|> fmap (\c -> [c]) anyChar
  post <- many space
  return $ TexToken { tokenContent='\\':str, tokenSource=pre++'\\':str++post, tokenPos = pos }

letterToken :: Parsec String () TexToken
letterToken = do
  pos <- getPosition
  pre <- many space
  c <- anyChar
  post <- many space
  return $ TexToken { tokenContent=[c], tokenSource=pre++c:post, tokenPos = pos }

texToken :: Parsec String () TexToken
texToken = macroToken <|> letterToken

tokenize :: String -> [TexToken]
tokenize tex = case parse (spaces >> many texToken) "" tex of
                 Left err -> error $ show err -- should never happen
                 Right toks -> toks



texTok :: String -> Parsec [TexToken] () TexToken
texTok content = token tokenSource tokenPos 
                 (\t -> if tokenContent t == content then Just t else Nothing)
texToks :: String -> Parsec [TexToken] () [TexToken]
texToks content =
    let toks = map (texTok.tokenContent) $ tokenize content in
    sequence toks

type Op = Operator [TexToken] () Identity Openmath
type TexParser = Parsec [TexToken] () Openmath

reservedOp :: String -> Parsec [TexToken] () ()
reservedOp x = try (texToks x) >> return ()

binary :: String -> (Openmath->Openmath->Openmath) -> Assoc -> Op
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix :: String -> (Openmath->Openmath) -> Op
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix :: String -> (Openmath->Openmath) -> Op
postfix name fun       = Postfix (do{ reservedOp name; return fun })

apply1 :: String -> Openmath -> Openmath
apply1 name =
    let (cd,name') = splitDot name in
    \a -> OMA [] (OMS [] cd name') [a]

apply2 :: String -> Openmath -> Openmath -> Openmath
apply2 name =
    let (cd,name') = splitDot name in
    \a b -> OMA [] (OMS [] cd name') [a,b]


negateTerm :: Openmath -> Openmath
negateTerm (OMI [] i) = (OMI [] (-i))
negateTerm m = apply1 "arith1.negate" m

grammar :: [[Op]]
grammar = [
 [prefix "-" negateTerm],
 [binary "*" (apply2 "arith1.times") AssocLeft, 
  binary "/" (apply2 "arith1.divide") AssocLeft],
 [binary "+" (apply2 "arith1.plus") AssocLeft,
  binary "-" (apply2 "arith1.minus") AssocLeft]]

digit :: Parsec [TexToken] () Integer
digit = choice [ texTok "0" >> return 0,
                 texTok "1" >> return 1,
                 texTok "2" >> return 2,
                 texTok "3" >> return 3,
                 texTok "4" >> return 4,
                 texTok "5" >> return 5,
                 texTok "6" >> return 6,
                 texTok "7" >> return 7,
                 texTok "8" >> return 8,
                 texTok "9" >> return 9 ]
        

int_unsigned :: Parsec [TexToken] () Integer
int_unsigned = do
  digits <- many1 digit
  return $ foldl (\i d -> i*10+d) 0 digits
  
int_negative :: Parsec [TexToken] () Integer
int_negative = do
  _ <- texTok "-"
  i <- int_unsigned
  return $ -i

int :: TexParser
int = do
  i <- try int_negative <|> int_unsigned
  return $ OMI [] i

-- TODO: all alphabet
var :: TexParser
var = 
    let v x = texToks x >> return (OMV [] x) in
    choice $ map v ["a","b","c"]
    

atom :: TexParser
atom = int <|> var    

exprParser :: Parsec [TexToken] () Openmath
exprParser = buildExpressionParser grammar atom


texToOpenmath :: String -> Openmath
texToOpenmath tex = 
    let toks = tokenize tex in
    case parse exprParser "" toks of
      Left err -> error (show err) 
      Right math -> math

-- texToOpenmath tex = case readTeX tex of
--     Right exps -> texmathExpsToCmml exps
--     Left err -> error ("parsing tex: "++err) 
    
