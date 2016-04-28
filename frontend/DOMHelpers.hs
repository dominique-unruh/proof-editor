module DOMHelpers where

import GHCJS.DOM.Types (Attr, Node, Element, ToJSString, FromJSString, toJSString, fromJSString, castToElement, castToText, castToAttr)
import GHCJS.DOM.Element (getTagName, getAttributes)
import qualified GHCJS.DOM.Node
import qualified GHCJS.DOM.Text
import qualified GHCJS.DOM.Attr
import qualified GHCJS.DOM.NodeList
import qualified Text.XML.Light as XML
import qualified GHCJS.DOM.NamedNodeMap
import qualified Data.Char

domGetChildren :: Element -> IO [Node]
domGetChildren el = do
  Just nodeList <- GHCJS.DOM.Node.getChildNodes el
  len <- GHCJS.DOM.NodeList.getLength nodeList
  mapM (\i -> do Just n <- GHCJS.DOM.NodeList.item nodeList (i-1); return n) [1 .. len]
  
-- console_log s v = do
--     v' <- toJSVal v
--     js_log (toJSString s) v'
-- foreign import javascript unsafe "console.log($1,$2)"
--     js_log :: JSString -> JSVal -> IO ()

domGetAttributes :: Element -> IO [Attr]
domGetAttributes el = do
  Just attr <- getAttributes el
  len <- GHCJS.DOM.NamedNodeMap.getLength attr
  mapM (\i -> do Just n <- GHCJS.DOM.NamedNodeMap.item attr (i-1); return $ castToAttr n) [1 .. len]
                 
                           

domToXML :: Element -> IO XML.Element
domToXML elem = do
  Just tagName <- getTagName elem
  let tag = XML.QName {XML.qName=map Data.Char.toLower tagName,
                       XML.qURI=Nothing, -- TODO
                       XML.qPrefix=Nothing}
  attr <- domGetAttributes elem
  attr' <- mapM attrToXML attr
  children <- domGetChildren elem
  children' <- mapM nodeToXML children
  return $ XML.Element {XML.elName=tag,
                        XML.elAttribs=attr',
                        XML.elContent=children',
                        XML.elLine=Nothing}
      where attrToXML attr = do
                  Just name <- GHCJS.DOM.Attr.getName attr
                  Just val <- GHCJS.DOM.Attr.getValue attr
                  return XML.Attr {XML.attrKey=XML.QName {XML.qName=name,
                                                          XML.qURI=Nothing, -- TODO
                                                          XML.qPrefix=Nothing},
                                   XML.attrVal=val}

nodeToXML :: Node -> IO XML.Content
nodeToXML node = do
  typ <- GHCJS.DOM.Node.getNodeType node
  case typ of
    GHCJS.DOM.Node.ELEMENT_NODE -> do
                   n <- domToXML $ castToElement node
                   return $ XML.Elem n
    GHCJS.DOM.Node.TEXT_NODE -> do
                   str <- GHCJS.DOM.Text.getWholeText $ castToText node
                   return $ XML.Text $ XML.CData {XML.cdVerbatim=XML.CDataText,
                                                  XML.cdData=str,
                                                  XML.cdLine=Nothing}
    i -> fail ("Unknown node type "++show i)

