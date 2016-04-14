{-# LANGUAGE OverloadedStrings #-}

module Openmath.Popcorn (openmathToPopcorn) where

import Openmath.Types
import Data.Text (Text, pack, append, cons, intercalate, snoc)

openmathToPopcorn :: Openmath -> Text
openmathToPopcorn om = snd $ openmathToPopcorn' om 

openmathToPopcorn' :: Openmath -> (Int,Text) -- priority, text
-- ;	prog1.block	n
-- :=	prog1.assign	2
-- ==>	logic1.implies	2
-- <=>	logic1.equivalent	2
-- or	logic1.or	n
-- and	logic1.and	n
-- <	relation1.lt	2
-- <=	relation1.leq	2
-- >	relation1.gt	2
-- =	relation1.eq	2
-- >=	relation1.geq	2
-- <>	relation1.neq	2
-- !=	relation1.neq	2
-- ..	interval1.interval	n
openmathToPopcorn' (OMA [] (OMS [] "arith1" "plus") args) = infixn 15 "+" args
openmathToPopcorn' (OMA [] (OMS [] "arith1" "minus") [a,b]) = infix2 16 "-" a b
openmathToPopcorn' (OMA [] (OMS [] "arith1" "times") args) = infixn 17 "*" args
-- /	arith1.divide	2
-- ^	arith1.power	2
-- |	complex1.complex_cartesian	2
-- //	nums1.rational	2
-- [...]	list1.list	n	Not really a infix operator
-- {...}	set1.set	n	Not really a infix operator
-- if...then...else...endif	prog1.if	n	Not really a infix operator
-- while...do...endwhile	prog1.while	n	Not really a infix operator
openmathToPopcorn' (OMI [] i) = (100, pack $ show i)
openmathToPopcorn' (OMF [] f) = (100, pack $ show f)
openmathToPopcorn' (OMS [] cd name) = (100, pack $ cd ++ '.' : name)
openmathToPopcorn' (OMV [] name) = (100, pack $ '$' : name)
openmathToPopcorn' (OMSTR [] name) = (100, '"' `cons` escape name `snoc` '"')
    where escape str = pack str -- TODO escape \n \r \t \\ \"
openmathToPopcorn' (OMA [] hd args) =
    (100, openmathToPopcorn hd `append` "(" `append`
                          (intercalate ", " $ map openmathToPopcorn args) `snoc` ')')
openmathToPopcorn' (OMBIND [] hd vars body) =
    (100, '[' `cons` (intercalate "," $ map openmathToPopcorn vars) `append`
        " -> " `append` (openmathToPopcorn body) `snoc` ']')

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
    
