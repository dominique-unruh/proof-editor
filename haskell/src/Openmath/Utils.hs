{-# LANGUAGE RankNTypes, ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}
module Openmath.Utils
 (getSubterm, replaceSubterm, equivalentTerms, isAtom, pattern Attribution, pattern Attribution',
  bvarToOMV, omvToBvar, pattern Int', pattern OMASym, bindP, removeAttribution, mapAttribution,
  splitDot)
where

import Openmath.Types

pattern OMASym cd name args <- (OMA _ (OMS _ cd name) args)
pattern Int' i <- OMI _ i

-- TODO replace by PatternSynonym
bindP :: Openmath -> Maybe (String, String, [Bvar], Openmath)
bindP (OMBIND _ (OMS _ cd name) bvars arg) = Just (cd,name,bvars,arg)
bindP _ = Nothing

mapAttribution :: (Attribution -> Attribution) -> Openmath -> Openmath
mapAttribution f (OMS s cd name) = OMS (f s) cd name
mapAttribution f (OMV s name) = OMV (f s) name
mapAttribution f (OMSTR s str) = OMSTR (f s) str
mapAttribution f (OMA s hd args) = OMA (f s) hd args
mapAttribution f (OMBIND s hd bvars arg) = OMBIND (f s) hd bvars arg
mapAttribution f (OME s cd name content) = OME (f s) cd name content
mapAttribution f (OMB s bytes) = OMB (f s) bytes
mapAttribution f (OMI s num) = OMI (f s) num
mapAttribution f (OMF s num) = OMF (f s) num

removeAttribution :: Openmath -> Openmath
removeAttribution = mapAttribution (\_ -> [])

semantics' :: Openmath -> Attribution
semantics' (OMS s _ _) = s
semantics' (OMV s _) = s
semantics' (OMSTR s _) = s
semantics' (OMA s _ _) = s
semantics' (OMBIND s _ _ _) = s
semantics' (OME s _ _ _) = s
semantics' (OMB s _) = s
semantics' (OMI s _) = s
semantics' (OMF s _) = s
-- | Matches all Openmath values with given semantics
pattern Attribution' s <- (semantics' -> s)


semantics :: Openmath -> Maybe Attribution
semantics c = case semantics' c of [] -> Nothing; s -> Just s
-- | Matches all Openmath values with given semantics, unless the semantics is empty
pattern Attribution s <- (semantics -> Just s)

{- | Extracts the semantics of a Openmath element.
     If the semantics is empty ([]), returns Nothing.
     Useful for pattern matching. -}

-- | Converts a bound variable (Bvar) to a Openmath identifier (CI)
bvarToOMV :: Bvar -> Openmath
bvarToOMV (sem,name) = OMV sem name

-- | Converts a Openmath identifier (CI) to a bound variable (Bvar)
omvToBvar :: Openmath -> (Attribution, String)
omvToBvar (OMV sem name) = (sem,name)
omvToBvar _ = error "expecting CI"


{- | Returns True the formula is an atom, i.e., contains no further subterms.
   | OME is considered an atom.
-}
isAtom :: Openmath -> Bool
isAtom OMS{} = True
isAtom OMV{} = True
isAtom OMI{} = True
isAtom OMF{} = True
isAtom OMSTR{} = True
isAtom OMBIND{} = False
isAtom OMA{} = False
isAtom OME{} = True
isAtom OMB{} = True

getSubterm :: Openmath -> Path -> Openmath
getSubterm _ (i:_) | i<0 = error "descending into semantics not implemented"
getSubterm term [] = term
getSubterm (OMA _ hd _) (0:path) = getSubterm hd path
getSubterm (OMA _ _ args) (1:i:path) = getSubterm (args!!i) path
getSubterm (OMA{}) (i:_) = error $ "accessing subelement "++show i ++ " of OMA"
getSubterm (OMBIND _ hd _ _) (0:path) = getSubterm hd path
getSubterm (OMBIND _ _ bvars _) (1:i:path) =
    case bvars!!i of (sem,name) -> getSubterm (OMV sem name) path
getSubterm (OMBIND _ _ _ arg) (2:path) = getSubterm arg path
getSubterm (OMBIND{}) (i:_) = error $ "accessing subelement "++show i ++ " of OMBIND"
getSubterm (OME{}) _ = error "OME subterms not supported"
getSubterm (OMSTR{}) _ = error "cannot descend into OMSTR"
getSubterm (OMV{}) _ = error "cannot descend into OMV"
getSubterm (OMB{}) _ = error "cannot descend into OMB"
getSubterm (OMS{}) _ = error "cannot descend into OMS"
getSubterm (OMI{}) _ = error "cannot descend into OMF"
getSubterm (OMF{}) _ = error "cannot descend into OMI"


replaceIth :: [a] -> Int -> a -> [a]
replaceIth xs i x = take i xs ++ x : drop (i+1) xs

replaceSubterm :: Openmath -> Path -> Openmath -> Openmath
replaceSubterm _ (i:_) _ | i<0 = error "descending into semantics not implemented"
replaceSubterm _ [] subterm = subterm
replaceSubterm (OMA sem hd args) (0:path) subterm =
    OMA sem (replaceSubterm hd path subterm) args
replaceSubterm (OMA sem hd args) (1:i:path) subterm =
    OMA sem hd (replaceIth args i (replaceSubterm (args!!i) path subterm))
replaceSubterm (OMA{}) (i:_) _ = error $ "accessing subelement "++show i ++ " of OMA"
replaceSubterm (OMBIND sem hd bvars arg) (0:path) subterm = OMBIND sem (replaceSubterm hd path subterm) bvars arg
replaceSubterm (OMBIND sem hd bvars arg) (1:i:path) subterm =
    let newbvar =  omvToBvar (replaceSubterm (bvarToOMV (bvars!!i)) path subterm) in
    OMBIND sem hd (replaceIth bvars i newbvar) arg
replaceSubterm (OMBIND sem hd bvars arg) (2:path) subterm = OMBIND sem hd bvars (replaceSubterm arg path subterm)
replaceSubterm (OMBIND{}) (i:_) _ = error $ "accessing subelement "++show i++" of OMBIND"
replaceSubterm (OME{}) _ _ = error "OME subterms not supported"
replaceSubterm (OMSTR{}) _ _ = error "cannot descend into OMSTR"
replaceSubterm (OMV{}) _ _ = error "cannot descend into OMV"
replaceSubterm (OMB{}) _ _ = error "cannot descend into OMB"
replaceSubterm (OMS{}) _ _ = error "cannot descend into OMS"
replaceSubterm (OMI{}) _ _ = error "cannot descend into OMI"
replaceSubterm (OMF{}) _ _ = error "cannot descend into OMF"


-- TODO alpha-equivalence
-- TODO commutativity
-- TODO ignore semantics
-- TODO associativity
-- TODO commutativity of binder args
-- TODO nested allquantifiers/exquantifiers
equivalentTerms :: Openmath -> Openmath -> Bool
equivalentTerms a b = a==b

splitDot :: String -> (String, String)
splitDot str = let (pfx,suffix) = break (=='.') str in (pfx,tail suffix)
