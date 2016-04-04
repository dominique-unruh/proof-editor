{-# LANGUAGE DeriveDataTypeable #-}

module ParseMathQuillLatex (parseMathQuillLatex) where

import Text.Parsec
import Text.Parsec.Token
import Data.Text
import Text.Parsec.Language
import Data.Data
import Data.Char (digitToInt)

type MathQuill = [MathQuillElement]

data MathQuillElement =
    MQCommand String [MathQuill] |
    MQVariable String |
    MQNumber Integer |
    MQError String
    deriving (Show, Eq, Data, Typeable)

type Parser a = Parsec String () a

mqVar :: Parser MathQuillElement
mqVar = do 
  x <- letter
  return (MQVariable [x])

macroname :: Parser String
macroname = do
  char '\\'
  name <- many1 letter
  return $ '\\' : name

mqArg :: Parser MathQuill
mqArg = do
  char '{'
  content <- many mathQuillElement
  char '}'
  return content

mqMacroCall :: Parser MathQuillElement
mqMacroCall = do
  macroname <- macroname <?> "LaTeX command"
  args <- many mqArg
  return $ MQCommand macroname args

mqNum :: Parser MathQuillElement
mqNum = do
  num <- decimal haskell
  return (MQNumber num)

mqOp :: Parser MathQuillElement
mqOp = do
  c <- oneOf "+-"
  return $ MQCommand [c] []

mqSuper = do
  char '^'
  content <- mqSuperSubArg
  return $ MQCommand "^" [content]

mqSuperSubArg = (mqArg <?> "block") <|> (mqDigitBlock <?> "integer") <|> (mqVarBlock <?> "letter")
    where
      mqDigitBlock = do
        d <- digit
        return [MQNumber (fromIntegral $ digitToInt d)]
      mqVarBlock = do
        v <- mqVar
        return [v]
                    
mqSub = do
  char '_'
  content <- mqSuperSubArg
  return $ MQCommand "_" [content]

mqDigitBlock :: Parser MathQuill
mqDigitBlock = do
  d <- digit
  return [MQNumber (fromIntegral $ digitToInt d)]

mqVarBlock :: Parser MathQuill
mqVarBlock = do
  v <- mqVar
  return [v]

mathQuillElement :: Parser MathQuillElement
mathQuillElement = 
    (mqVar <?> "variable") 
    <|> (mqNum <?> "integer")
    <|> mqMacroCall
    <|> (mqOp <?> "operator")
    <|> (mqSuper <?> "superscript")
    <|> (mqSub <?> "subscript")

mainParser :: Parser MathQuill
mainParser = do
  mq <- many mathQuillElement 
  eof
  return mq

parseMathQuillLatex :: String -> MathQuill
parseMathQuillLatex ltx = 
    case parse mainParser "" ltx of
      Left err -> [MQError (show err)]
      Right res -> res
