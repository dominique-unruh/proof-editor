{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Openmath.Cmathml (fromCmathml, toCmathml) where

import Text.XML.Light.Types
import Openmath.Types
import Text.XML.Light.Output
import Text.XML.Light.Proc
import Text.XML.Light.Input (parseXML)
import Data.Char (isSpace)
import Openmath.Utils (pattern Attribution, removeAttribution, mapAttribution)
--import Numeric (readSigned, readFloat)
import Data.Ratio (denominator, numerator)
import FFIExports (exportFFI)

--elCSymbol :: QName
--elCSymbol = blank_name {qName="csymbol"}
--atCD :: QName
--atCD = blank_name {qName="cd"}

text :: String -> Content
text t = Text CData{cdVerbatim = CDataText, cdData = t, cdLine = Nothing}

elm :: String -> [Attr] -> [Content] -> Content
elm tag attrib content = Elem Element
    {elName=blank_name {qName=tag},
     elAttribs=attrib,
     elContent=content,
     elLine=Nothing}

attr :: String -> String -> Attr
attr name val = Attr {attrKey=blank_name {qName=name}, attrVal=val}

toElement :: Openmath -> Content
toElement (math @ (Attribution sem)) =
    let math' = removeAttribution math in
    let attribs = map attrToXML sem
            where attrToXML (cd,name,False,ann) = elm "annotation-xml" [attr "cd" cd, attr "name" name, attr "encoding" "MathML-Content"] [toElement ann]
                  attrToXML (_,_,True,_) = error "nyi foreign attribute" in
    elm "semantics" [] (toElement math' : attribs)
toElement (OMS _ cd name) = elm "csymbol" [attr "cd" cd] [text name]
toElement (OMA _ hd body) = elm "apply" [] (map toElement (hd:body))
toElement (OMV _ name) = elm "ci" [] [text name]
toElement (OMBIND _ hd vars arg) =
    elm "bind" []
        (toElement hd :
         map (\v -> elm "bvar" [] [toElement v]) vars ++
         [toElement arg])
toElement (OMI _ i) =
    elm "cn" [attr "type" "integer"] [text (show i)]
toElement (OMF _ r) =
    elm "cn" [attr "type" "double"] [text (show r)]
-- toElement (CN _ (Real r)) =
--     -- TODO remove or make infinite precision!
--     let str = show ((fromRational r)::Double) in
--     elm "cn" [attr "type" "real"] [text str]
toElement (OMSTR _ str) = elm "cs" [] [text str]
toElement m = error ("NYI: toElement: " ++ show m)

attrP :: String -> [Attr] -> Maybe String
attrP n = lookupAttrBy (\a -> qName a == n)

pattern Elm name att content <- (Elem Element {elName=QName {qName=name},elAttribs=att,elContent=content})

--elmP :: Content -> Maybe (String,[Attr],[Content])
--elmP (Elem Element {elName=QName {qName=name},elAttribs=att,elContent=content}) = Just (name,att,content)
--elmP _ = Nothing

textP :: Content -> Maybe String
textP (Text str) = Just (cdData str)
textP _ = Nothing

dropPlus :: String -> String
dropPlus ('+':str) = str
dropPlus str = str

-- | Read a rational given as a decimal fraction
--readRational :: String -> Rational
--readRational = fst . head . readSigned readFloat . dropPlus

pattern Txt t <- (textP -> Just t)

pattern TypeAttr t <- (lookupAttrBy (\a -> qName a == "type") -> Just t)
pattern NoTypeAttr <- (lookupAttrBy (\a -> qName a == "type") -> Nothing)
pattern EncodingAttr t <- (lookupAttrBy (\a -> qName a == "encoding") -> Just t)
--pattern NoEncodingAttr <- (lookupAttrBy (\a -> qName a == "encoding") -> Nothing)
pattern CDAttr t <- (attrP "cd" -> Just t)

rationalToOpenmath :: Rational -> Openmath
rationalToOpenmath r =
    OMA [] (OMS [] "nums1" "rational") [OMI [] (numerator r), OMI [] (denominator r)]


fromElement :: Content -> Openmath
fromElement (Elm "csymbol" (CDAttr cd) [Txt name]) = OMS [] cd name
fromElement (Elm "apply" _ (hd:args)) = OMA [] (fromElement hd) (map fromElement args)
fromElement (Elm "bind" _ []) = error "bind element must have at least arguments (had zero)"
fromElement (Elm "bind" _ [_]) = error "bind element must have at least arguments (had one)"
fromElement (Elm "bind" _ (hd:bvars_arg)) =
    let bvars = init bvars_arg; arg = last bvars_arg in
    OMBIND [] (fromElement hd) (map bvarFromElement bvars) (fromElement arg)
fromElement (Elm "cn" (TypeAttr "integer") [Txt int]) = OMI [] $ read (dropPlus int)
fromElement (Elm "cn" (TypeAttr "hexdouble") [Txt _]) = error "cn type=hexdouble not implemented"
fromElement (Elm "cn" (TypeAttr "double") [Txt r]) = OMF [] $ read r
fromElement (Elm "cn" (TypeAttr "real") [Txt real]) = rationalToOpenmath (read real)
fromElement (Elm "cn" (TypeAttr typ) _) = error $ "cn with type="++typ
fromElement (Elm "cn" NoTypeAttr _) = error "cn without type"
fromElement (Elm "ci" NoTypeAttr [Txt name]) = OMV [] name
fromElement (Elm "ci" (TypeAttr _) [Txt _]) = error "CI with type not implemented" -- produces semantics, http://www.w3.org/TR/MathML3/chapter4.html#contm.ci.strict
fromElement (Elm "ci" _ _) = error "CI must have single text child"
fromElement (Elm "cerror" _  _) = error "cerror not implemented" -- TODO
fromElement (Elm "cbytes" _ _) = error "cbytes not implemented" -- TODO
fromElement (Elm "share" _ _) = error "sharing not supported"
fromElement (Elm "cs" _ [Txt str]) = OMSTR [] str
fromElement (Elm "cs" _ [_]) = error "cs with non-text content"
fromElement (Elm "cs" _ []) = OMSTR [] ""
fromElement (Elm "cs" _ (_:_)) = error "cs with more than one content element"
fromElement (Elm "semantics" _ (math:annots)) =
   let math' = fromElement math in
   let annots' = map fromElementAttr annots in
   mapAttribution (\sem -> sem++annots') math'
fromElement (Elm "semantics" _ []) = error "empty semantics tag"
fromElement (Elm tag _ _) = error $ "unexpected tag "++tag++" in Content MathML"
fromElement (Text _) = error "text in Content MathML"
fromElement (CRef _) = error "CRef in Content MathML"
fromElement (Elem _) = error "unreachable code"


fromElementAttr :: Content -> Attribute
fromElementAttr (Elm "annotation" _ _) = error "not implemented: annotation-tag"
fromElementAttr (Elm "annotation-xml" (attrs @ (EncodingAttr "MathML-Content")) [body]) =
  case (attrP "cd" attrs, attrP "name" attrs) of
    (Nothing,_) -> error "annotation-xml lacking cd-attribute"
    (_,Nothing) -> error "annotation-xml lacking name-attribute"
    (Just cd, Just name) -> (cd,name,False,fromElement body)
fromElementAttr (Elm "annotation-xml" (EncodingAttr "MathML-Content") _) =
    error "annotation-xml with encoding MathML3-Content must have exactly one argument"
fromElementAttr (Elm "annotation-xml" _ _) =
    error "annotation-xml only supported with encoding MathML3-Content"
fromElementAttr (Elm tag _ _) =
    error $ "expecting annotation or annotation-xml in semantics, not "++tag
fromElementAttr _ =
    error $ "expecting annotation or annotation-xml in semantics, not a non-element"


bvarFromElement :: Content -> Openmath
bvarFromElement (Elm "bvar" _ [v]) =
    case fromElement v of
        e @ (OMV _ _) -> e
        _ -> error "below bvar, not ci or semantics-ci"
bvarFromElement (Elm "bvar" _ _) = error "bvar must have exactly one child"
bvarFromElement (Elm tag _ _) = error $ "unexpected tag "++tag++" instead of bvar"
bvarFromElement (Text _) = error "expecting a bvar tag, got text"
bvarFromElement (CRef _) = error "expecting a bvar tag, got CRef"
bvarFromElement (Elem _) = error "unreachable code"


{- | Converts an Openmath value into Content MathML XML.
  (see http://www.w3.org/TR/MathML3/chapter4.html) -}
toCmathml:: Openmath -> String
toCmathml = showContent . toElement

stripWhiteText :: [Content] -> [Content]
stripWhiteText = filter isNonWhite
    where isNonWhite (Text txt) = not (all isSpace (cdData txt))
          isNonWhite (Elem _) = True
          isNonWhite (CRef _) = True

fromCmathml:: String -> Openmath
fromCmathml xml =
    case stripWhiteText $ parseXML xml of
        [root] -> fromElement root
        [] -> error "empty XML"
        _ -> error "more than one root element in XML"

exportFFI 'toCmathml
exportFFI 'fromCmathml
