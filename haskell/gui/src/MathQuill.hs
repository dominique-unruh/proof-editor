{-# LANGUAGE ScopedTypeVariables #-}

module MathQuill (MathQuill, mathQuill, setLatex, getLatex, addEditHandler, getPMathMLDom, getPMathML) where

import GHCJS.DOM.Types (Element, ToJSString, FromJSString, toJSString, fromJSString)
import GHCJS.Types (JSVal, JSString)
import GHCJS.Foreign.Callback (Callback, asyncCallback)
import qualified GHCJS.DOM.Types
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import Data.Maybe (fromJust)
import qualified Text.XML.Light as XML

import DOMHelpers

newtype MathQuill = MathQuill JSVal

instance ToJSVal MathQuill where
    toJSVal (MathQuill m) = return m

{-| Creates a MathQuill editor field. Expects an empty <div> element
that will be transformed into the editor -}
mathQuill :: Element -> IO MathQuill
mathQuill e = do
    m <- js_mathQuill e
    return $ MathQuill m
foreign import javascript unsafe "$r = MathQuillInterface2.MathField($1, {handlers: {}});"
    js_mathQuill :: Element -> IO JSVal

{-| Sets the content of the editor to the result of parsing a LaTeX string. -}
setLatex :: ToJSString a => MathQuill  -> a -> IO ()
setLatex (MathQuill m) s = js_setLatex m (toJSString s)
foreign import javascript unsafe "$1.latex($2)"
    js_setLatex :: JSVal -> JSString -> IO ()

{-| Returns the content of the editor as a LaTeX string. -}
getLatex :: FromJSString a => MathQuill -> IO a
getLatex (MathQuill m) = do
    ltx <- js_getLatex m
    return $ fromJSString ltx
foreign import javascript "$r = $1.latex()"
    js_getLatex :: JSVal -> IO JSString

{-| Adds a handler that is invoked each time the math editor's content is edited. -}
addEditHandler :: MathQuill -> IO() -> IO ()
addEditHandler (MathQuill m) cb = do
    cb' <- asyncCallback cb
    js_addEditHandler m cb'
foreign import javascript unsafe
    "$1.__options.handlers.fns.edit = $2"
    js_addEditHandler :: JSVal -> Callback (IO ()) -> IO ()

{-| Returns the Presentation MathML representation of the editor's content. (As a DOM tree.) -}
getPMathMLDom :: MathQuill -> IO Element
getPMathMLDom (MathQuill m) = do
  mml <- js_getPMathML m
  Just mml' <- fromJSVal mml
  return mml'
foreign import javascript unsafe "$r = $1.pmathml()"
  js_getPMathML :: JSVal -> IO JSVal


getPMathML :: MathQuill -> IO XML.Element
getPMathML m = do
  dom <- getPMathMLDom m
  domToXML dom
