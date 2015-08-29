{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module UserError where

import qualified Data.Map as Map
import Data.Dynamic
import qualified Text.XML as X
import qualified Text.XML.Cursor as X
import Text.XML.Cursor ((>=>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad (when)
import Data.List (intercalate)

{- TODO: Explain: HTML-templates, simple-HTML -}

-- | Contains an object of arbitrary type a, together with its string representation
data Dyn = Dyn Dynamic String
instance Show Dyn where
    show (Dyn _ str) = str

{- | Represents an error for presenting to the user. -}
data UserError = UserError {
    {- | A short description of the error.
       Should be suitable for presentation in, e.g., a dialog box.
       Must be an HTML-template that results in simple HTML. -}
    shortDescription :: [X.Node],
    {- | A long description of the error.
       This should be a thorough user-manual/tutorial-style explanation,
       explaining the reasons for the problem and how to fix it. -}
    longDescription :: [X.Node],
    errorData :: Map.Map String Dyn
}

type UserErrorRenderer = ()


noPrologue :: X.Prologue
noPrologue = X.Prologue { X.prologueBefore=[], X.prologueDoctype=Nothing, X.prologueAfter=[] }
xmlToString :: [X.Node] -> String
xmlToString xml =
    let root = X.Element{X.elementName="r", X.elementAttributes=Map.empty, X.elementNodes=xml} in
    let doc = X.Document {X.documentPrologue=noPrologue, X.documentEpilogue=[], X.documentRoot=root} in
    let text = X.renderText X.def doc in
    let text' = TL.dropEnd 4 $ TL.drop 41 text in
    TL.unpack text'

userErrorFromFile :: FilePath -> IO UserError
userErrorFromFile file = do
    doc <- X.readFile X.def file
    let root = X.fromDocument doc
    let body = X.child >=> X.laxElement (T.pack "body") >=> X.child >=> X.anyElement $ root
    when (null body) $ error "no elements in body"
    let short' = head body
    print $ X.node $ head body
    let short = map X.node $ X.child short'
    when (null short) $ error "short description empty"
    let long = map X.node $ tail body
    when (null short) $ error "long description empty"
    return UserError { shortDescription=short, longDescription=long, errorData=Map.empty }

addErrorData :: (Typeable a, Show a) => String -> a -> UserError -> UserError
addErrorData name dat err =
    let newErrorData = Map.insert name (Dyn (toDyn dat) (show dat)) $ errorData err in
    err { errorData = newErrorData }

-- TODO: proper rendering
renderDescription :: UserErrorRenderer -> UserError -> [X.Node] -> String
renderDescription _ err descr =
     intercalate "\n" (xmlToString descr:datas)
     where
         datas = map (\(k,v) -> "    <p>"++k++" = "++show v++"</p>") $ Map.assocs $ errorData err

renderShortDescription :: UserErrorRenderer -> UserError -> String
renderShortDescription renderer err = renderDescription renderer err $ shortDescription err

renderLongDescription :: UserErrorRenderer -> UserError -> String
renderLongDescription renderer err = renderDescription renderer err $ longDescription err

instance Show UserError where
    show err = intercalate "\n" (shortDescr:datas)
        where shortDescr :: String = xmlToString $ shortDescription err
              datas :: [String] = map (\(k,v) -> "    "++k++" = "++show v) $ Map.assocs $ errorData err
