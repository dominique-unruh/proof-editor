{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI, CPP #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement, createTextNode, click)
import GHCJS.DOM.Element (setInnerHTML, focus)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Types (Element, ToJSString, FromJSString, toJSString, fromJSString)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.Types (JSVal, JSString)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Foreign.Callback (Callback, asyncCallback)
import Control.Monad (void)
-- import qualified Text.XML as XML
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Arrow ((***))
import qualified Text.XML.Light.Output
import qualified Text.XML.Light.Types as X

-- import ParseMathQuillLatex (parseMathQuillLatex)
import MathQuill


foreign import javascript unsafe "$1.latex($2)"
    js_setLatex :: JSVal -> JSString -> IO ()
foreign import javascript "$r = $1.latex()"
    js_getLatex :: JSVal -> IO JSString
foreign import javascript unsafe "console.log($1,$2)"
    js_log :: JSString -> JSVal -> IO ()

rawJS :: ToJSString a => ToJSVal b => a -> b -> IO JSVal
rawJS code x = do
  x' <- toJSVal x
  js_rawJS (toJSString code) x'
foreign import javascript unsafe "$r = eval('(function fun(_) { '+$1+' })')($2); if (! ($r===undefined)) console.log('rawJS',$1,$2,$r)"
  js_rawJS :: JSString -> JSVal -> IO JSVal

publish :: ToJSString a => ToJSVal b => a -> b -> IO ()
publish name val = do
    val' <- toJSVal val
    js_publish (toJSString name) val'
foreign import javascript unsafe "eval('_=function(_){'+$1+'=_}')($2)"
    js_publish :: JSString -> JSVal -> IO ()

console_log s v = do
    v' <- toJSVal v
    js_log (toJSString s) v'


-- xmlToString :: [(String,String)] -> XML.Element -> String
-- xmlToString ns =
--     let settings = XML.def { XML.rsNamespaces=map (T.pack *** T.pack) ns } in \xml ->
--     let doc = XML.Document {XML.documentPrologue=noPrologue, XML.documentEpilogue=[], XML.documentRoot=xml}
--               where noPrologue = XML.Prologue { XML.prologueBefore=[], XML.prologueDoctype=Nothing, XML.prologueAfter=[] } in
--     let text = XML.renderText settings doc in
--     let text' = TL.drop 38 text in
--     TL.unpack text'

main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    Just span <- createElement doc (Just "span")
    appendChild body $ Just span
    math <- mathQuill span
    publish "m" math

    addEditHandler math $ do
      void $ rawJS "m_to_pmml()" (1::Int)

    setLatex math ("\\left[x^2\\right]")

    mqFocus math

    pmml <- getPMathML math
    putStrLn $ Text.XML.Light.Output.ppElement pmml

    return ()


