{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Cmathml.Xml where

import Text.XML.Light.Types
import Cmathml.Types
import Text.XML.Light.Output
import Text.XML.Light.Proc
import Text.XML.Light.Input (parseXML)
import Data.Char (isSpace)
import Cmathml.Utils (bvarToCI, pattern Semantics, removeSemantics)
import Numeric (readSigned, readFloat)

elCSymbol :: QName
elCSymbol = blank_name {qName="csymbol"}
atCD :: QName
atCD = blank_name {qName="cd"}

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

toElement :: Cmathml -> Content
toElement (math @ (Semantics sem)) = 
    let math' = removeSemantics math in
    let attribs = map attrToXML sem
            where attrToXML (cd,name,AnnotationCMML ann) = elm "annotation-xml" [attr "cd" cd, attr "name" name, attr "encoding" "MathML-Content"] [toElement ann]
                  attrToXML (_,_,AnnotationXML{}) = error "nyi annotation-xml with non-cmathm content"
                  attrToXML (_,_,AnnotationData{}) = error "nyi annotation with text/data content" in
    elm "semantics" [] (toElement math' : attribs)
toElement (CSymbol _ cd name) = elm "csymbol" [attr "cd" cd] [text name]
toElement (Apply _ hd body) = elm "apply" [] (map toElement (hd:body))
toElement (CI _ name) = elm "ci" [] [text name]
toElement (Bind _ hd vars arg) =
    elm "bind" []
        (toElement hd :
         map (\v -> elm "bvar" [] [toElement (bvarToCI v)]) vars ++
         [toElement arg])
toElement (CN _ (Int i)) = 
    elm "cn" [attr "type" "integer"] [text (show i)]
toElement (CN _ (IEEE r)) = 
    elm "cn" [attr "type" "double"] [text (show r)]
toElement (CN _ (Real _)) = error "CN Real" -- how to show "r" in decimal fraction notation? 
--    elm "cn" [attr "type" "integer"] [text (show r)]
toElement (CS _ str) = elm "cs" [] [text str]
toElement m = error ("NYI: toElement: " ++ show m)

--attrP :: String -> [Attr] -> Maybe String
--attrP n = lookupAttrBy (\a -> qName a == n)

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
readRational :: String -> Rational
readRational = fst . head . readSigned readFloat . dropPlus

pattern Txt t <- (textP -> Just t)
          
pattern TypeAttr t <- (lookupAttrBy (\a -> qName a == "type") -> Just t)
pattern NoTypeAttr <- (lookupAttrBy (\a -> qName a == "type") -> Nothing)
pattern CDAttr t <- (lookupAttrBy (\a -> qName a == "cd") -> Just t)


fromElement :: Content -> Cmathml
fromElement (Elm "csymbol" (CDAttr cd) [Txt name]) = CSymbol [] cd name
fromElement (Elm "apply" _ (hd:args)) = Apply [] (fromElement hd) (map fromElement args)
fromElement (Elm "bind" _ []) = error "bind element must have at least arguments (had zero)"
fromElement (Elm "bind" _ [_]) = error "bind element must have at least arguments (had one)"
fromElement (Elm "bind" _ (hd:bvars_arg)) =
    let bvars = init bvars_arg; arg = last bvars_arg in
    Bind [] (fromElement hd) (map bvarFromElement bvars) (fromElement arg)
fromElement (Elm "cn" (TypeAttr "integer") [Txt int]) = CN [] (Int $ read (dropPlus int))
fromElement (Elm "cn" (TypeAttr "hexdouble") [Txt _]) = error "cn type=hexdouble not implemented"
fromElement (Elm "cn" (TypeAttr "double") [Txt _]) = error "cn type=double not implemented"
fromElement (Elm "cn" (TypeAttr "real") [Txt real]) = CN [] (Real $ readRational real)
fromElement (Elm "cn" (TypeAttr typ) _) = error $ "cn with type="++typ
fromElement (Elm "cn" NoTypeAttr _) = error "cn without type"
fromElement (Elm "ci" NoTypeAttr [Txt name]) = CI [] name
fromElement (Elm "ci" (TypeAttr _) [Txt _]) = error "CI with type not implemented" -- produces semantics, http://www.w3.org/TR/MathML3/chapter4.html#contm.ci.strict
fromElement (Elm "ci" _ _) = error "CI must have single text child"
fromElement (Elm "cerror" _  _) = error "cerror not implemented" -- TODO
fromElement (Elm "cbytes" _ _) = error "cerror not implemented" -- TODO
fromElement (Elm "share" _ _) = error "sharing not supported"
fromElement (Elm "cs" _ [Txt str]) = CS [] str
fromElement (Elm "cs" _ [_]) = error "cs with non-text content"
fromElement (Elm "cs" _ []) = CS [] ""
fromElement (Elm "cs" _ (_:_)) = error "cs with more than one content element"
fromElement (Elm "semantics" _ _) = error "semantics not implemented" -- TODO
fromElement (Elm tag _ _) = error $ "unexpected tag "++tag++" in Content MathML"
fromElement (Text _) = error "text in Content MathML"
fromElement (CRef _) = error "CRef in Content MathML"
fromElement (Elem _) = error "unreachable code"


bvarFromElement :: Content -> Bvar
bvarFromElement (Elm "bvar" _ [v]) =
    case fromElement v of
        CI sem name -> (sem,name)
        _ -> error "below bvar, not ci or semantics-ci" 
bvarFromElement (Elm "bvar" _ _) = error "bvar must have exactly one child"
bvarFromElement (Elm tag _ _) = error $ "unexpected tag "++tag++" instead of bvar"
bvarFromElement (Text _) = error "expecting a bvar tag, got text"
bvarFromElement (CRef _) = error "expecting a bvar tag, got CRef"
bvarFromElement (Elem _) = error "unreachable code"


cmathmlToXML :: Cmathml -> String
cmathmlToXML = showContent . toElement

stripWhiteText :: [Content] -> [Content]
stripWhiteText = filter isNonWhite
    where isNonWhite (Text txt) = not (all isSpace (cdData txt))
          isNonWhite (Elem _) = True
          isNonWhite (CRef _) = True

cmathmlFromXML :: String -> Cmathml
cmathmlFromXML xml = 
    case stripWhiteText $ parseXML xml of
        [root] -> fromElement root
        [] -> error "empty XML"
        _ -> error "more than one root element in XML"
        
