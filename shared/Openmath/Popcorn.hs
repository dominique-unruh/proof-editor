{-# LANGUAGE OverloadedStrings #-}

module Openmath.Popcorn (openmathToPopcorn) where

import Openmath.Types
import Openmath.Utils
import Data.Text (Text, pack, append, cons, intercalate, snoc)
import qualified Data.Map.Strict as M

abbrev_syms = M.fromList $ map (\(x,y) -> (x, pack y)) [
               ("transc1.cos","cos"),
               ("transc1.cosh","cosh"),
               ("transc1.cot","cot"),
               ("transc1.coth","coth"),
               ("transc1.csc","csc"),
               ("transc1.csch","csch"),
               ("transc1.exp","exp"),
               ("transc1.sec","sec"),
               ("transc1.sech","sech"),
               ("transc1.sin","sin"),
               ("transc1.sinh","sinh"),
               ("transc1.tan","tan"),
               ("transc1.tanh","tanh"),
               ("arith1.abs","abs"),
               ("arith1.root","root"),
               ("arith1.sum","sum"),
               ("arith1.product","product"),
               ("calculus1.diff","diff"),
               ("calculus1.int","int"),
               ("calculus1.defint","defint"),
               ("nums1.pi","pi"),
               ("nums1.e","e"),
               ("nums1.i","i"),
               ("nums1.infinity","infinity"),
               ("minmax1.min","min"),
               ("minmax1.max","max"),
               ("fns1.lambda","lambda"),
               ("logic1.true","true"),
               ("logic1.false","false"),
               ("combinat1.binomial","binomial"),
               ("integer1.factorial","factorial")
              ];


openmathToPopcorn :: Openmath -> Text
openmathToPopcorn om = snd $ openmathToPopcorn' om 

openmathToPopcorn' :: Openmath -> (Int,Text) -- priority, text
openmathToPopcorn' (OMA [] (OMS [] "prog1" "block") args)                 = infixn 1 "; " args
openmathToPopcorn' (OMA [] (OMS [] "prog1" "assign") [a,b])               = infix2 2 " := " a b
openmathToPopcorn' (OMA [] (OMS [] "logic1" "implies") [a,b])             = infix2 3 " ==> " a b
openmathToPopcorn' (OMA [] (OMS [] "logic1" "equivalent") [a,b])          = infix2 4 " <=> " a b
openmathToPopcorn' (OMA [] (OMS [] "logic1" "or") args)                   = infixn 5 " or " args
openmathToPopcorn' (OMA [] (OMS [] "logic1" "and") args)                  = infixn 6 " and " args
openmathToPopcorn' (OMA [] (OMS [] "relation1" "lt") [a,b])               = infix2 7 "<" a b
openmathToPopcorn' (OMA [] (OMS [] "relation1" "leq") [a,b])              = infix2 8 "<=" a b
openmathToPopcorn' (OMA [] (OMS [] "relation1" "gt") [a,b])               = infix2 9 ">" a b
openmathToPopcorn' (OMA [] (OMS [] "relation1" "eq") [a,b])               = infix2 10 "=" a b
openmathToPopcorn' (OMA [] (OMS [] "relation1" "geq") [a,b])              = infix2 11 ">=" a b
-- openmathToPopcorn' (OMA [] (OMS [] "relation1" "neq") [a,b])           = infix2 12 "<>" a b
openmathToPopcorn' (OMA [] (OMS [] "relation1" "neq") [a,b])              = infix2 13 "!=" a b
openmathToPopcorn' (OMA [] (OMS [] "interval1" "interval") args)          = infixn 14 ".." args
openmathToPopcorn' (OMA [] (OMS [] "arith1" "plus") args)                 = infixn 15 "+" args
openmathToPopcorn' (OMA [] (OMS [] "arith1" "minus") [a,b])               = infix2 16 "-" a b
openmathToPopcorn' (OMA [] (OMS [] "arith1" "times") args)                = infixn 17 "*" args
openmathToPopcorn' (OMA [] (OMS [] "arith1" "divide") [a,b])              = infix2 18 "/" a b
openmathToPopcorn' (OMA [] (OMS [] "arith1" "power") [a,b])               = infix2 19 "^" a b
openmathToPopcorn' (OMA [] (OMS [] "complex1" "complex_cartesian") [a,b]) = infix2 20 "|" a b
openmathToPopcorn' (OMA [] (OMS [] "nums1" "rational") [a,b])             = infix2 21 "//" a b
openmathToPopcorn' (OMA [] (OMS [] "list1" "list") args)          = 
    let (_,commalist) = infixn 0 ", " args in
    (100, '[' `cons` commalist `snoc` ']')
openmathToPopcorn' (OMA [] (OMS [] "set1" "set") args)          = 
    let (_,commalist) = infixn 0 ", " args in
    (100, '{' `cons` commalist `snoc` '}')

openmathToPopcorn' (OMA [] (OMS [] "prog1" "if") [a,b,c]) = 
    let a' = openmathToPopcorn a; b' = openmathToPopcorn b; c' = openmathToPopcorn c in
    (100, "if" `append` a' `append` "then" `append` b' `append` "else" `append` c' `append` "endif")
    

openmathToPopcorn' (OMA [] (OMS [] "prog1" "while") [a,b]) = 
    let a' = openmathToPopcorn a; b' = openmathToPopcorn b in
    (100, "while" `append` a' `append` "do" `append` b' `append` "endwhile")


openmathToPopcorn' (OMI [] i) = (100, pack $ show i)
openmathToPopcorn' (OMF [] f) = (100, pack $ show f)

openmathToPopcorn' (OMS [] cd name) =
    let cdname = cd ++ '.' : name in
    case M.lookup cdname abbrev_syms of
      Just abbr -> (100, abbr)
      Nothing -> (100, pack cdname)

openmathToPopcorn' (OMV [] name) = (100, pack $ '$' : name)
openmathToPopcorn' (OMSTR [] name) = (100, '"' `cons` escape name `snoc` '"')
    where escape str = pack str -- TODO escape \n \r \t \\ \"
openmathToPopcorn' (OMA [] hd args) =
    (100, openmathToPopcorn hd `append` "(" `append`
                          (intercalate ", " $ map openmathToPopcorn args) `snoc` ')')
openmathToPopcorn' (OMBIND [] hd vars body) =
    (100, '[' `cons` (intercalate "," $ map openmathToPopcorn vars) `append`
        " -> " `append` (openmathToPopcorn body) `snoc` ']')

openmathToPopcorn' (OME _ _ _ _) = error ("openmathToPopcorn: OME not implemented")
openmathToPopcorn' (OMB _ _) = error ("openmathToPopcorn: OMB not implemented")
openmathToPopcorn' om  |  semantics' om /= []  =
                           error ("openmathToPopcorn: attributes not implemented")
openmathToPopcorn' om = error ("Impossible to match this")


openmathToPopcorn'' pri om =
    let (pri',pop) = openmathToPopcorn' om in
    if pri >= pri' then '(' `cons` pop `snoc` ')'
    else pop

infixn :: Int -> Text -> [Openmath] -> (Int,Text)
infixn pri op args =
    let args' = map (openmathToPopcorn'' pri) args in
    (pri, intercalate op args')

infix2 :: Int -> Text -> Openmath -> Openmath -> (Int,Text)
infix2 pri op a b = infixn pri op [a,b]
    
