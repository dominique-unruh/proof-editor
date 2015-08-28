{-# LANGUAGE PartialTypeSignatures #-}
module Openmath.TeX where

import Openmath.Types
import Openmath.Utils (splitDot)
-- import Text.TeXMath.Types
-- import Text.TeXMath.Readers.TeX (readTeX)
import Text.Parsec hiding (digit)
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)
import Control.Monad (void)
import Data.Char (isAlpha, digitToInt, isDigit)
import Data.List (groupBy, sortOn)
import Data.Function (on)

data TexToken = TexToken { tokenContent :: String, tokenSource :: String, tokenPos :: SourcePos }
    deriving (Eq)
instance Show TexToken where
    show = tokenContent

data TexOperator = TexOperator { texOpPriority :: Int,
                                 texOpSymbol :: String,
                                 texOpTemplate :: String }
    deriving (Eq, Show)

data TexConfiguration = TexConfiguration { texConfigOperators :: [TexOperator] }
    deriving (Eq, Show)

macroToken :: Parsec String () TexToken
macroToken = do
  pos <- getPosition
  pre <- many space
  _   <- char '\\'
  str <- many1 letter <|> fmap (: []) anyChar
  post <- many space
  return TexToken { tokenContent='\\':str, tokenSource=pre++'\\':str++post, tokenPos = pos }

letterToken :: Parsec String () TexToken
letterToken = do
  pos <- getPosition
  pre <- many space
  c <- anyChar
  post <- many space
  return TexToken { tokenContent=[c], tokenSource=pre++c:post, tokenPos = pos }

texToken :: Parsec String () TexToken
texToken = macroToken <|> letterToken

tokenize :: String -> [TexToken]
tokenize tex = case parse (spaces >> many texToken) "" tex of
                 Left err -> error $ show err -- should never happen
                 Right toks -> toks



texSatisfy' :: (String -> Maybe a) -> Parsec [TexToken] () a
texSatisfy' f = token tokenSource tokenPos (f . tokenContent)
texSatisfy :: (String -> Bool) -> Parsec [TexToken] () TexToken
texSatisfy filt = token tokenSource tokenPos
                 (\t -> if filt (tokenContent t) then Just t else Nothing)
texTok :: String -> Parsec [TexToken] () TexToken
texTok content = texSatisfy (==content)
--texTok content = token tokenSource tokenPos
--                 (\t -> if tokenContent t == content then Just t else Nothing)
texToks :: String -> Parsec [TexToken] () [TexToken]
texToks content =
    let toks = map (texTok.tokenContent) $ tokenize content in
    sequence toks

type Op = Operator [TexToken] () Identity Openmath
type TexParser = Parsec [TexToken] () Openmath

reservedOp :: String -> Parsec [TexToken] () ()
reservedOp x = Control.Monad.void (try (texToks x))

binary :: String -> (Openmath->Openmath->Openmath) -> Assoc -> Op
binary  name fun = Infix (do{ reservedOp name; return fun })
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
negateTerm (OMI [] i) = OMI [] (-i)
negateTerm m = apply1 "arith1.negate" m

testConfiguration :: TexConfiguration
testConfiguration = TexConfiguration { texConfigOperators =
    [ op 50 "arith1.times" "infixl:*",
      op 20 "arith1.plus" "infixl:+",
      op 50 "arith1.divide" "infixl:/",
      op 20 "arith1.minus" "infixl:-",
      op 100 "arith1.negate" "prefix:-"
    ]}
    where op pri sym pat = TexOperator { texOpPriority=pri, texOpSymbol=sym, texOpTemplate=pat }

grammarTableFromConfig :: TexConfiguration -> [[Op]]
grammarTableFromConfig (TexConfiguration {texConfigOperators=ops}) =
--    let config' = sortOn texOpPriority (texConfigOperators ops in
    let ops' = groupBy ((==) `on` texOpPriority) $ reverse $ sortOn texOpPriority ops in
    map (map opToTableEntry) ops'
  where
    opToTableEntry (TexOperator { texOpSymbol=sym, texOpTemplate=templ }) =
--        let sym' = case splitDot sym of (cd,name) -> OMS [] cd name in
        let (kind,_:templ') = break (==':') templ in
        case kind of
            "infixl" -> Infix (do reservedOp templ'; return $ apply2 sym) AssocLeft
            "prefix" -> Prefix (do reservedOp templ'; return $ apply1 sym)
            _ -> error $ "unknown prefix "++kind++" for template "++templ


grammar :: [[Op]]
grammar = grammarTableFromConfig testConfiguration

--grammar :: [[Op]]
--grammar = [
-- [prefix "-" negateTerm],
-- [binary "*" (apply2 "arith1.times") AssocLeft,
--  binary "/" (apply2 "arith1.divide") AssocLeft],
-- [binary "+" (apply2 "arith1.plus") AssocLeft,
--  binary "-" (apply2 "arith1.minus") AssocLeft]]

digit :: Parsec [TexToken] () Integer
digit = texSatisfy' digitToNum
    where
        digitToNum [c] | isDigit c = Just $ fromIntegral $ digitToInt c
        digitToNum _ = Nothing
--digit = choice [ texTok "0" >> return 0,
--                 texTok "1" >> return 1,
--                 texTok "2" >> return 2,
--                 texTok "3" >> return 3,
--                 texTok "4" >> return 4,
--                 texTok "5" >> return 5,
--                 texTok "6" >> return 6,
--                 texTok "7" >> return 7,
--                 texTok "8" >> return 8,
--                 texTok "9" >> return 9 ]


intUnsigned :: Parsec [TexToken] () Integer
intUnsigned = do
  digits <- many1 digit
  return $ foldl (\i d -> i*10+d) 0 digits

intNegative :: Parsec [TexToken] () Integer
intNegative = do
  _ <- texTok "-"
  i <- intUnsigned
  return $ -i

int :: TexParser
int = do
  i <- try intNegative <|> intUnsigned
  return $ OMI [] i

var :: TexParser
var = texSatisfy' letterToVar
    where letterToVar [c] | isAlpha c = Just $ OMV [] [c]
          letterToVar _ = Nothing
--    let v x = texToks x >> return (OMV [] x) in
--    choice $ map v ["a","b","c"]

parens :: String -> String -> TexParser
parens open close = do
    _ <- texTok open
    e <- exprParser
    _ <- texTok close
    return e

atom :: TexParser
atom = int <|> var <|> parens "(" ")" <|> parens "{" "}"

exprParser :: Parsec [TexToken] () Openmath
exprParser = buildExpressionParser grammar atom

texToOpenmath :: String -> Openmath
texToOpenmath tex =
    let toks = tokenize tex in
    case parse exprParser "" toks of
      Left err -> error (show err)
      Right math -> math

