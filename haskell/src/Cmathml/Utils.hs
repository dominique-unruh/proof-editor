{-# LANGUAGE RankNTypes, ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}
module Cmathml.Utils
 (getSubterm, replaceSubterm, equivalentTerms, isAtom, pattern Semantics, pattern Semantics',
  bvarToCI, ciToBvar, pattern Int', pattern ApplySym, bindP, removeSemantics)
where

import Cmathml.Types

pattern ApplySym cd name args <- (Apply _ (CSymbol _ cd name) args)
pattern Int' i <- CN _ (Int i)

-- TODO replace by PatternSynonym
bindP :: Cmathml -> Maybe (String, String, [Bvar], Cmathml)
bindP (Bind _ (CSymbol _ cd name) bvars arg) = Just (cd,name,bvars,arg)
bindP _ = Nothing

-- intP :: Cmathml -> Maybe Integer
-- intP (CN _ (Int i)) = Just i
-- intP _ = Nothing

-- DEPRECATED
--applyP :: Cmathml -> Maybe (String, String, [Cmathml])
--applyP (Apply _ (CSymbol _ cd name) args) = Just (cd,name,args)
--applyP _ = Nothing

--intP :: Cmathml -> Maybe Integer
--intP (CN _ (Int i)) = Just i
--intP _ = Nothing

mapSemantics :: (Semantics -> Semantics) -> Cmathml -> Cmathml
mapSemantics f (CSymbol s cd name) = CSymbol (f s) cd name
mapSemantics f (CI s name) = CI (f s) name
mapSemantics f (CS s str) = CS (f s) str
mapSemantics f (Apply s hd args) = Apply (f s) hd args
mapSemantics f (Bind s hd bvars arg) = Bind (f s) hd bvars arg
mapSemantics f (CError s cd name content) = CError (f s) cd name content
mapSemantics f (CBytes s bytes) = CBytes (f s) bytes
mapSemantics f (CN s num) = CN (f s) num

removeSemantics :: Cmathml -> Cmathml
removeSemantics = mapSemantics (\_ -> [])

semantics' :: Cmathml -> Semantics
semantics' (CSymbol s _ _) = s
semantics' (CI s _) = s
semantics' (CS s _) = s
semantics' (Apply s _ _) = s
semantics' (Bind s _ _ _) = s
semantics' (CError s _ _ _) = s
semantics' (CBytes s _) = s
semantics' (CN s _) = s
-- | Matches all Cmathml values with given semantics
pattern Semantics' s <- (semantics' -> s)


semantics :: Cmathml -> Maybe Semantics
semantics c = case semantics' c of [] -> Nothing; s -> Just s
-- | Matches all Cmathml values with given semantics, unless the semantics is empty
pattern Semantics s <- (semantics -> Just s)

{- | Extracts the semantics of a Cmathml element.
     If the semantics is empty ([]), returns Nothing.
     Useful for pattern matching. -}

-- | Converts a bound variable (Bvar) to a Cmathml identifier (CI)
bvarToCI :: Bvar -> Cmathml
bvarToCI (sem,name) = CI sem name

-- | Converts a Cmathml identifier (CI) to a bound variable (Bvar)
ciToBvar :: Cmathml -> (Semantics, String)
ciToBvar (CI sem name) = (sem,name)
ciToBvar _ = error "expecting CI"


{- | Returns True the formula is an atom, i.e., contains no further subterms.
   | CError is considered an atom.
-}
isAtom :: Cmathml -> Bool
isAtom CSymbol{} = True
isAtom CI{} = True
isAtom CN{} = True
isAtom CS{} = True
isAtom Bind{} = False
isAtom Apply{} = False
isAtom CError{} = True
isAtom CBytes{} = True

getSubterm :: Cmathml -> Path -> Cmathml
getSubterm _ (i:_) | i<0 = error "descending into semantics not implemented"
getSubterm term [] = term
getSubterm (Apply _ hd _) (0:path) = getSubterm hd path
getSubterm (Apply _ _ args) (1:i:path) = getSubterm (args!!i) path
getSubterm (Apply{}) (i:_) = error $ "accessing subelement "++show i ++ " of Apply"
getSubterm (Bind _ hd _ _) (0:path) = getSubterm hd path
getSubterm (Bind _ _ bvars _) (1:i:path) =
    case bvars!!i of (sem,name) -> getSubterm (CI sem name) path
getSubterm (Bind _ _ _ arg) (2:path) = getSubterm arg path
getSubterm (Bind{}) (i:_) = error $ "accessing subelement "++show i ++ " of Bind"
getSubterm (CError{}) _ = error "CError subterms not supported"
getSubterm (CS{}) _ = error "cannot descend into CS"
getSubterm (CI{}) _ = error "cannot descend into CI"
getSubterm (CBytes{}) _ = error "cannot descend into CBytes"
getSubterm (CSymbol{}) _ = error "cannot descend into CSymbol"
getSubterm (CN{}) _ = error "cannot descend into CN"


replaceIth :: [a] -> Int -> a -> [a]
replaceIth xs i x = take i xs ++ x : drop (i+1) xs

replaceSubterm :: Cmathml -> Path -> Cmathml -> Cmathml
replaceSubterm _ (i:_) _ | i<0 = error "descending into semantics not implemented"
replaceSubterm _ [] subterm = subterm
replaceSubterm (Apply sem hd args) (0:path) subterm = 
    Apply sem (replaceSubterm hd path subterm) args
replaceSubterm (Apply sem hd args) (1:i:path) subterm =
    Apply sem hd (replaceIth args i (replaceSubterm (args!!i) path subterm))
replaceSubterm (Apply{}) (i:_) _ = error $ "accessing subelement "++show i ++ " of Apply"
replaceSubterm (Bind sem hd bvars arg) (0:path) subterm = Bind sem (replaceSubterm hd path subterm) bvars arg
replaceSubterm (Bind sem hd bvars arg) (1:i:path) subterm =
    let newbvar =  ciToBvar (replaceSubterm (bvarToCI (bvars!!i)) path subterm) in
    Bind sem hd (replaceIth bvars i newbvar) arg
replaceSubterm (Bind sem hd bvars arg) (2:path) subterm = Bind sem hd bvars (replaceSubterm arg path subterm)
replaceSubterm (Bind{}) (i:_) _ = error $ "accessing subelement "++show i++" of Bind"
replaceSubterm (CError{}) _ _ = error "CError subterms not supported"
replaceSubterm (CS{}) _ _ = error "cannot descend into CS"
replaceSubterm (CI{}) _ _ = error "cannot descend into CI"
replaceSubterm (CBytes{}) _ _ = error "cannot descend into CBytes"
replaceSubterm (CSymbol{}) _ _ = error "cannot descend into CSymbol"
replaceSubterm (CN{}) _ _ = error "cannot descend into CN"


-- TODO alpha-equivalence
-- TODO commutativity
-- TODO ignore semantics
-- TODO associativity
-- TODO commutativity of binder args
-- TODO nested allquantifiers/exquantifiers
equivalentTerms :: Cmathml -> Cmathml -> Bool
equivalentTerms a b = a==b

