{-# LANGUAGE PatternGuards #-}
module Openmath.Pmathml where

import qualified Data.Map as Map
import qualified Text.XML as X
-- import qualified Text.XML.Cursor as X
import Openmath.Types
import Data.Typeable
import OpenDoc.ODS
import Control.Monad (when)
import Openmath.Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Arrow ((***))
import Data.List (intersperse)
import Misc
--import Debug.Trace (trace)
import Control.Exception (throw)

data PMMLOperator = PMMLOperator {
    pmmlOpPriority :: Int,
    pmmlOpStandalone :: Maybe X.Element,
    pmmlOpRender :: PMMLConfiguration
                 -> Int -- ^ upper level priority
                 -> [Openmath] -- ^ symbol arguments
                 -> Maybe X.Element
} deriving (Typeable)

data PMMLConfiguration = PMMLConfiguration {
    pmmlConfigOperators :: Map.Map (String,String) PMMLOperator
} deriving (Typeable)

mathmlRead :: String -> X.Element
mathmlRead str =
    let str' = "<x xmlns=\"http://www.w3.org/1998/Math/MathML\">"++str++"</x>" in
    let xml = X.parseText X.def (TL.pack str') in
    case xml of
        Right X.Document {X.documentRoot=X.Element{X.elementNodes=[X.NodeElement xml']}} -> xml'
        Right _ -> error "unexpected shape"
        Left err -> throw err

triplicate :: a -> a -> a -> [b] -> [(a,b)]
triplicate _ _ _ [] = []
triplicate a b c (x:xs) = (a,x):trip xs
    where trip [] = []
          trip [y] = [(c,y)]
          trip (y:ys) = (b,y):trip ys


templateToOp :: Int -> String -> PMMLOperator
templateToOp prio templ =
    let (kind,_:templ') = break (==':') templ in
    let xml = mathmlRead templ' in
--    let Right X.Document {X.documentRoot=xml} = X.parseText X.def (TL.pack $ showTrace templ') in
    let priosInfix = triplicate (prio+1) (prio+1) (prio+1) in
    let priosInfixL = triplicate prio (prio+1) (prio+1) in
    let priosInfixR = triplicate (prio+1) (prio+1) prio in
    let infixRender _ _ _ [] = Nothing
        infixRender _ _ _ [_] = Nothing
        infixRender prios config _ args =
            Just $ mrow (intersperse xml $
                         map (uncurry (pmmlRender' config)) $ prios args) in
    let prefixRender config _ [a] = Just $ mrow [xml, pmmlRender' config (prio+1) a]
        prefixRender _ _ _ = Nothing in
    let postfixRender config _ [a] = Just $ mrow [pmmlRender' config (prio+1) a, xml]
        postfixRender _ _ _ = Nothing in
    let renderfun = case kind of "infix" -> infixRender priosInfix
                                 "infixl" -> infixRender priosInfixL
                                 "infixr" -> infixRender priosInfixR
                                 "prefix" -> prefixRender
                                 "postfix" -> postfixRender
                                 _ -> error $ "unsupported kind "++kind in
    PMMLOperator { pmmlOpPriority=prio,
                   pmmlOpStandalone=Just xml,
                   pmmlOpRender=renderfun }

pmmlDefaultConfiguration :: IO PMMLConfiguration
pmmlDefaultConfiguration = do
    let readCols (sym:pri:_:pmml:_) = (sym,pri,pmml)
        readCols _ = error "Row with too few columns in symbols.ods"
    doc <- odsFromFile "resources/symbols.ods"
    let (row1:table) = map (map cellText) $ cells $ head $ sheets doc
    when (readCols row1 /= ("CMML symbol","Priority","PMML")) $ error "Bad columns"
    let table' = filter (\(_,_,t) -> t/="") $ map readCols table

    let opMap = Map.fromList $ map rowToOp table'
                where rowToOp (sym,pri,templ) = (splitDot sym, templateToOp (read pri) templ)
    return PMMLConfiguration { pmmlConfigOperators=opMap }

pmmlName :: String -> X.Name
pmmlName name = X.Name { X.nameLocalName=T.pack name,
                         X.nameNamespace=Just $ T.pack "http://www.w3.org/1998/Math/MathML",
                         X.namePrefix=Nothing }
unqName :: String -> X.Name
unqName name = X.Name { X.nameLocalName=T.pack name,
                        X.nameNamespace=Nothing,
                        X.namePrefix=Nothing }

pmmlElem :: String -> [(String,String)] -> [X.Node] -> X.Element
pmmlElem name attr childr = X.Element {
    X.elementName=pmmlName name,
    X.elementAttributes=Map.fromList $ map (unqName *** T.pack) attr,
    X.elementNodes=childr }

txt :: String -> X.Node
txt str = X.NodeContent $ T.pack str

merror :: String -> X.Element
merror str = pmmlElem "merror" [] [X.NodeElement $ pmmlElem "mtext" [] [txt str]]

mi :: String -> X.Element
mi name = pmmlElem "mi" [] [txt name]

mrow :: [X.Element] -> X.Element
mrow ms = pmmlElem "mrow" [] $ map X.NodeElement ms

moSep :: String -> X.Element
moSep name = pmmlElem "mo" [("separator","true")] [txt name]

moFence :: String -> X.Element
moFence name = pmmlElem "mo" [("fence","true")] [txt name]

moInfix:: String -> X.Element
moInfix name = pmmlElem "mo" [("form","infix")] [txt name]

mfencedSep :: [X.Element] -> X.Element
mfencedSep ms = mrow $
    moFence "(" :
    intersperse (moSep ",") ms ++
    [moFence ")"]
mfenced :: [X.Element] -> X.Element
mfenced ms = mrow $ moFence "(" : ms ++ [moFence ")"]

applyFunction :: X.Element
applyFunction = moInfix "\x2061" -- &ApplyFunction;

maxPri :: Int
maxPri = 1000

pmmlRender' :: PMMLConfiguration -> Int -> Openmath -> X.Element
pmmlRender' _ _ (OMI _ i) = pmmlElem "mn" [] [txt $ show i]
pmmlRender' _ _ (OMF _ f) = pmmlElem "mn" [] [txt $ show f]
pmmlRender' _ _ (OMV _ v) = pmmlElem "mi" [] [txt v]
pmmlRender' _ _ (OMSTR _ s) = pmmlElem "mtext" [] [txt $ show s] -- TODO should be enclosed in quotes?
pmmlRender' _ _ (OMB _ _) = merror "rendering of OMB not implemented" -- TODO

pmmlRender' config _ (OMS _ cd name)
    | Just op <- Map.lookup (cd,name) (pmmlConfigOperators config),
      Just xml <- pmmlOpStandalone op
    = xml

pmmlRender' _ _ (OMS _ cd name) = mi $ cd++"."++name

pmmlRender' config pri (OMA [] (OMS _ cd name) args)
    | Just op <- Map.lookup (cd,name) (pmmlConfigOperators config),
      Just xml <- pmmlOpRender op config pri args
    = -- trace ("PRI "++show pri++" X "++show (pmmlOpPriority op))
            if pri > pmmlOpPriority op then mfenced [xml] else xml

pmmlRender' config _ (OMA _ hd args) = mrow [
    pmmlRender' config maxPri hd,
    applyFunction,
    mfencedSep $ map (pmmlRender' config 0) args]


pmmlRender' config _ (OMBIND _ hd bvars arg) = mrow $
    pmmlRender' config maxPri hd
    : map (pmmlRender' config maxPri . bvarToOMV) bvars
    ++ [moInfix ".", pmmlRender' config 0 arg]

pmmlRender' _ _ (OME{}) = merror "rendering of OME not implemented" -- TODO
--pmmlRender' _ _ _ = merror "pmmlRender: not implemented" -- TODO


pmmlRender :: PMMLConfiguration -> Openmath -> X.Element
pmmlRender config = pmmlRender' config 0

toPmathml :: PMMLConfiguration -> Openmath -> String
toPmathml config math =
    let pmml = pmmlRender config math in
    xmlToString [] $ pmmlElem "math" [] [X.NodeElement pmml]

