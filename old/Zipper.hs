{-# LANGUAGE PatternGuards #-}

module Openmath.Zipper (
    OpenmathZipper, substitute, followPath, pathToZipper, upwards, getContent,
    root, getFocussed
) where

import Data.Generics.Zipper

import Openmath.Types
--import Openmath.Utils (semantics')
import Data.Data
import Data.Maybe (fromJust)

{- | In contrast to 'Zipper Openmath', this type will always focus on an Openmath Value -}
newtype OpenmathZipper = OpenmathZipper (Zipper Openmath)
    deriving (Typeable)

instance Show OpenmathZipper where
    show z = "[Zipper in "++show (getContent z)++" ]"

{- | Given a zipper, and a path, returns a new zipper that focuses on a descendent of the first zipper -}
followPath :: Path -> OpenmathZipper -> OpenmathZipper
followPath [] zipper = zipper
followPath (i:path) zipper | i<0 = followPath path (descendAttribution (-i-1) zipper)
followPath path zipper =
    case (getContent zipper, path) of
    (OMS{}, _) -> error "invalid path (descending into OMS)"
    (OMB{}, _) -> error "invalid path (descending into OMB)"
    (OMF{}, _) -> error "invalid path (descending into OMF)"
    (OMV{}, _) -> error "invalid path (descending into OMV)"
    (OMI{}, _) -> error "invalid path (descending into OMI)"
    (OMA{}, 0:p) -> followPath p $ descendOMAHead zipper
    (OMA{}, 1:i:p) -> followPath p $ descendOMAArg i zipper
    (OMA{}, [1]) -> error "invalid path (into OMA-arg, no index)"
    (OMA{}, _) -> error "invalid path (into OMA, not 0 or 1)"
    (OMSTR{}, _) -> error "invalid path (descending into OMSTR)"
    (OMBIND{}, 0:p) -> followPath p $ descendOMBINDHead zipper
    (OMBIND{}, 1:i:p) -> followPath p $ descendOMBINDBvar i zipper
    (OMBIND{}, 2:p) -> followPath p $ descendOMBINDArg zipper
    (OMBIND{}, [1]) -> error "invalid path (into OMBIND-bvars, no index)"
    (OMBIND{}, _) -> error "invalid path (into OMBIND, not 0, 1, or 2)"
    (OME{}, i:p) -> followPath p $ descendOME i zipper
    (OME{}, _) -> error "unreachable"


descendOMAHead :: OpenmathZipper -> OpenmathZipper
descendOMAHead (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OMA{}) -> OpenmathZipper $ fromJust $ right $ fromJust $ down' zipper
        _ -> error "not an OMA"

descendOMAArg :: Int -> OpenmathZipper -> OpenmathZipper
descendOMAArg i (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OMA{}) -> OpenmathZipper $ descendList i $ fromJust $ down zipper
        _ -> error "not an OMA"

descendOME :: Int -> OpenmathZipper -> OpenmathZipper
descendOME i (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OME{}) -> OpenmathZipper $ descendList i $ fromJust $ down zipper
        _ -> error "not an OME"


descendOMBINDHead :: OpenmathZipper -> OpenmathZipper
descendOMBINDHead (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OMBIND{}) -> OpenmathZipper $ fromJust $ right $ fromJust $ down' zipper
        _ -> error "not an OMBIND"

descendOMBINDArg :: OpenmathZipper -> OpenmathZipper
descendOMBINDArg (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OMBIND{}) -> OpenmathZipper $ fromJust $ down zipper
        _ -> error "not an OMBIND"

descendOMBINDBvar :: Int -> OpenmathZipper -> OpenmathZipper
descendOMBINDBvar i (OpenmathZipper zipper) =
    case getHole zipper of
        Just (OMBIND{}) -> OpenmathZipper $ descendList i $ fromJust $ left $ fromJust $ down zipper
        _ -> error "not an OMBIND"

power :: Int -> (a->a) -> a -> a
power 0 _ x = x
power i f x | i>0 = power (i-1) f $ f x
power _ _ _ = error "power with negative argument"

descendList :: Int -> Zipper a -> Zipper a
descendList i zipper = fromJust $ down' $ power i (fromJust.down) zipper
--    case getHole zipper of
--        Nothing -> error "not a list"
--        Just l -> if length l <= i then error "list too short"
--                  else fromJust $ down' $ power i (fromJust.down) zipper



descendAttribution :: Int -> OpenmathZipper -> OpenmathZipper
descendAttribution i (OpenmathZipper zipper) =
    OpenmathZipper $ fromJust $ down $ descendList i $ fromJust $ down' zipper
--    let attr = semantics' $ fromJust $ getHole zipper in
--    if length attr <= i then error "invalid path: no i-th attribution"
--    else let targetAttr = descendList i $ fromJust $ down' zipper in -- Points to the (String,String,Bool,Attribute) tuple
--    let targetOm = fromJust $ down targetAttr in -- Points to the Openmath
--    OpenmathZipper targetOm

pathToZipper :: Path -> Openmath -> OpenmathZipper
pathToZipper path om = followPath path $ OpenmathZipper $ toZipper om

upwards :: OpenmathZipper -> Maybe OpenmathZipper
upwards = error "undefined: upwards"

substitute :: OpenmathZipper -> Openmath -> OpenmathZipper
substitute (OpenmathZipper z) new = OpenmathZipper $ fromJust $ setHole' new z

root :: OpenmathZipper -> OpenmathZipper
root (OpenmathZipper zz) = OpenmathZipper $ root' zz
    where root' z | Just parent <- up z = parent
          root' z = z

getContent :: OpenmathZipper -> Openmath
getContent (OpenmathZipper z) = fromZipper z

getFocussed :: OpenmathZipper -> Openmath
getFocussed (OpenmathZipper z) = fromJust $ getHole z
