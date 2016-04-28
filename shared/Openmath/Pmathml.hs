{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Openmath.Pmathml (
    toPmathml, pmmlRender, PMMLConfiguration(..), pmmlDefaultConfiguration
    ) where

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
import Data.List (intersperse, intercalate)
import Misc
import Debug.Trace (trace)
import Control.Exception (throw)
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafePerformIO)
import FFIExports (exportFFI)

type RenderFunction = PMMLConfiguration
                   -> Int -- ^ upper level priority
                   -> Path -- ^ Path (in reverse) of the current element (does not need to be added to the element itself, but may be needed for children)
                   -> [Openmath] -- ^ symbol arguments
                   -> Maybe X.Element

data PMMLOperator = PMMLOperator {
    pmmlOpPriority :: Int,
    pmmlOpStandalone :: Maybe X.Element,
    pmmlOpRender :: RenderFunction
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

triplicate :: a -> a -> a -> [b] -> [(a,Int,b)]
triplicate _ _ _ [] = []
triplicate a b c (x:xs) = (a,0,x):trip 1 xs
    where trip _ [] = []
          trip i [y] = [(c,i,y)]
          trip i (y:ys) = (b,i,y):trip (i+1) ys


mkTemplateRender :: X.Element -> RenderFunction
mkTemplateRender xml =
    let len = f xml + 1 -- number of arguments in template
            where f X.Element{X.elementName=X.Name{X.nameLocalName=tagName}}
                    | Just i <- readMaybe $ T.unpack tagName = i
                  f e = maximum $ map f' $ X.elementNodes e
                  f' (X.NodeElement e) = f e
                  f' _ = 0 in
    \config _ path args ->
    let render X.Element{X.elementName=X.Name{X.nameLocalName=tagName}}
               | Just i <- readMaybe $ T.unpack tagName
               = pmmlRender' config argPrio (i:1:path) $ args!!i
                    where argPrio = 0
        render elm = elm { X.elementNodes=map render' $ X.elementNodes elm }
        render' (X.NodeElement elm) = X.NodeElement $ render elm
        render' node = node
    in
    if length args == len then Just $ render xml else Nothing


templateToOp :: Int -> String -> PMMLOperator
templateToOp prio templ =
    let (kind,_:templ') = break (==':') templ in
    let xml = mathmlRead templ' in
--    let Right X.Document {X.documentRoot=xml} = X.parseText X.def (TL.pack $ showTrace templ') in
    let priosInfix = triplicate (prio+1) (prio+1) (prio+1) in
    let priosInfixL = triplicate prio (prio+1) (prio+1) in
    let priosInfixR = triplicate (prio+1) (prio+1) prio in
    let infixRender _ _ _ _ [] = Nothing
        infixRender _ _ _ _ [_] = Nothing
        infixRender prios config _ path args =
            Just $ mrow (intersperse (addPath (0:path) $ addClasses ["leaf","symbol"] xml) $
                         map (\(pri,i,a) -> pmmlRender' config pri (i:1:path) a) $ prios args) in
    let prefixRender config _ path [a] =
            Just $ mrow [addPath (0:path) $ addClasses ["leaf","symbol"] xml,
                         pmmlRender' config (prio+1) (0:1:path) a]
        prefixRender _ _ _ _ = Nothing in
    let postfixRender config _ path [a] =
            Just $ mrow [pmmlRender' config (prio+1) (0:1:path) a,
                         addPath (0:path) $ addClasses ["leaf","symbol"] xml]
        postfixRender _ _ _ _ = Nothing in
    let renderfun = case kind of "infix" -> infixRender priosInfix
                                 "infixl" -> infixRender priosInfixL
                                 "infixr" -> infixRender priosInfixR
                                 "prefix" -> prefixRender
                                 "postfix" -> postfixRender
                                 "template" -> mkTemplateRender xml
                                 _ -> error $ "unsupported kind "++kind in
    let standalone = case kind of "template" -> Nothing
                                  _ -> Just xml in
    PMMLOperator { pmmlOpPriority=prio,
                   pmmlOpStandalone=standalone,
                   pmmlOpRender=renderfun }

pmmlDefaultConfiguration :: PMMLConfiguration
{-# NOINLINE pmmlDefaultConfiguration #-}
pmmlDefaultConfiguration
  = unsafePerformIO $
      do let readCols (sym : (pri : (_ : (pmml : _)))) = (sym, pri, pmml)
             readCols _ = error "Row with too few columns in symbols.ods"
         doc <- odsFromFile "resources/symbols.ods"
         let (row1 : table) = map (map cellText) $ cells $ head $ sheets doc
         when (readCols row1 /= ("CMML symbol", "Priority", "PMML")) $
           error "Bad columns"
         let table' = filter (\ (_, _, t) -> t /= "") $ map readCols table
         let opMap = Map.fromList $ map rowToOp table'
               where rowToOp (sym, pri, templ)
                       = (splitDot sym, templateToOp (read pri) templ)
         return PMMLConfiguration{pmmlConfigOperators = opMap}

pmmlName :: String -> X.Name
pmmlName name = X.Name { X.nameLocalName=T.pack name,
                         X.nameNamespace=Just $ T.pack "http://www.w3.org/1998/Math/MathML",
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

addPath :: Path -- ^ path (in reverse)
           -> X.Element -> X.Element
addPath path xml =
    xml { X.elementAttributes = Map.insert pathattr pathstr $ X.elementAttributes xml }
        where pathattr = trace "SHOULD BE TRACED ONLY ONCE" $ unqName "path"
              pathstr = T.pack $ intercalate "." $ map show $ reverse path
addClasses :: [String] -> X.Element -> X.Element
addClasses classes =
    let classtxt = T.pack (unwords classes) in
    let classattr = unqName "class" in
    \xml ->
    xml { X.elementAttributes = Map.insert classattr classtxt $ X.elementAttributes xml }

maxPri :: Int
maxPri = 1000

pmmlRender' :: PMMLConfiguration
            -> Int -- ^ priority
            -> Path -- ^ (in reverse)
            -> Openmath -> X.Element
pmmlRender' _ _ path (OMI _ i) = addPath path $ addClasses ["leaf","number","integer"] $ pmmlElem "mn" [] [txt $ show i]
pmmlRender' _ _ path (OMF _ f) = addPath path $ addClasses ["leaf","number","float"] $ pmmlElem "mn" [] [txt $ show f]
pmmlRender' _ _ path (OMV _ v) = addPath path $ addClasses ["leaf","variable"] $ pmmlElem "mi" [] [txt v]
pmmlRender' _ _ path (OMSTR _ s) = addPath path $ addClasses ["leaf","string"] $ pmmlElem "mtext" [] [txt $ show s] -- TODO should be enclosed in quotes?
pmmlRender' _ _ _ (OMB _ _) = merror "rendering of OMB not implemented" -- TODO

pmmlRender' config _ path (OMS _ cd name)
    | Just op <- Map.lookup (cd,name) (pmmlConfigOperators config),
      Just xml <- pmmlOpStandalone op
    = addPath path $ addClasses ["leaf","symbol"] xml

pmmlRender' _ _ path (OMS _ cd name) = addPath path $ addClasses ["leaf","symbol"] $ mi $ cd++"."++name

pmmlRender' config pri path (OMA [] (OMS _ cd name) args)
    | Just op <- Map.lookup (cd,name) (pmmlConfigOperators config),
      Just xml <- pmmlOpRender op config pri path args
    = -- trace ("PRI "++show pri++" X "++show (pmmlOpPriority op))
            addPath path $ addClasses ["apply"] $
                if pri > pmmlOpPriority op then mfenced [xml] else xml

pmmlRender' config _ path (OMA _ hd args) = addPath path $ addClasses ["apply"] $ mrow [
    pmmlRender' config maxPri (0:path) hd,
    applyFunction,
    mfencedSep $ zipWith (\i a -> pmmlRender' config 0 (i:1:path) a) [0..] args]


pmmlRender' config _ path (OMBIND _ hd bvars arg) = addPath path $ addClasses ["bind"] $ mrow $
    pmmlRender' config maxPri (0:path) hd
    : zipWith (\i -> pmmlRender' config maxPri (i:1:path)) [0..] bvars
    ++ [moInfix ".", pmmlRender' config 0 (2:path) arg]

pmmlRender' _ _ _ (OME{}) = merror "rendering of OME not implemented" -- TODO
--pmmlRender' _ _ _ = merror "pmmlRender: not implemented" -- TODO


pmmlRender :: PMMLConfiguration -> Openmath -> X.Element
pmmlRender config math =
    let pmml = pmmlRender' config 0 [] math in
    pmmlElem "math" [] [X.NodeElement pmml]

toPmathml :: PMMLConfiguration -> Openmath -> String
toPmathml config math =
    let pmml = pmmlRender config math in
    xmlToString [] $ pmmlElem "math" [] [X.NodeElement pmml]


exportFFI 'toPmathml
exportFFI 'pmmlDefaultConfiguration
