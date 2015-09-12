{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module UserError.UserError where

import qualified Data.Map as Map
import Data.Dynamic
import qualified Text.XML as X
import qualified Text.XML.Cursor as X
import Text.XML.Cursor ((>=>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad (when)
import Data.List (intercalate)
import System.FilePath (combine)
import qualified System.IO.Unsafe
import Misc (unqName)

{- TODO: Explain: HTML-templates, simple-HTML -}

-- | Contains an object of arbitrary type a, together with its string representation
data Dyn = Dyn Dynamic String
    deriving (Typeable)
instance Show Dyn where
    show (Dyn _ str) = str

{- | Represents an error for presenting to the user. -}
data UserError = UserError {
    {- | Unique ID, used for disambiguation, e.g., in bugreports -}
    userErrorID :: String,
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
    deriving (Typeable)

data UserErrorRenderer = UserErrorRenderer {
    rendererTransform :: UserError -> X.Element -> [X.Node] }


noPrologue :: X.Prologue
noPrologue = X.Prologue { X.prologueBefore=[], X.prologueDoctype=Nothing, X.prologueAfter=[] }
-- TODO: create function in Utils
xmlToString :: [X.Node] -> String
xmlToString xml =
    let root = X.Element{X.elementName="{http://www.w3.org/1999/xhtml}r", X.elementAttributes=Map.empty, X.elementNodes=xml} in
    let doc = X.Document {X.documentPrologue=noPrologue, X.documentEpilogue=[], X.documentRoot=root} in
    let text = X.renderText settings doc
               where settings = X.def { X.rsNamespaces=[("ue","urn:unruh:proofedit:usererror")] } in
    let text' = TL.drop 119 $ TL.dropEnd 4 text in
    TL.unpack text'

ueXMLName :: String -> X.Name
ueXMLName name = X.Name { X.nameLocalName=T.pack name,
                          X.nameNamespace=Just "urn:unruh:proofedit:usererror",
                          X.namePrefix=Just "ue" }

userErrorFromFile :: String -> FilePath -> IO UserError
userErrorFromFile ueId file = do
    doc <- X.readFile X.def file
    let root = X.fromDocument doc
    let body = (X.child >=> X.laxElement (T.pack "body") >=> X.child) root
    let long = map X.node $ X.checkName (\n -> n /= ueXMLName "shortdescription" &&
                                               n /= ueXMLName "metadata") =<< body
    let short = map X.node $ (X.checkName (==ueXMLName "shortdescription") >=> X.child) =<< body
    when (null short) $ error "short description empty"
    when (null long) $ error "long description empty"
    return UserError { userErrorID=ueId, shortDescription=short, longDescription=long, errorData=Map.empty }

addErrorData :: (Typeable a, Show a) => String -> a -> UserError -> UserError
addErrorData name dat err =
    let newErrorData = Map.insert name (Dyn (toDyn dat) (show dat)) $ errorData err in
    err { errorData = newErrorData }

-- TODO: proper rendering
renderDescription :: UserErrorRenderer -> UserError -> [X.Node] -> String
renderDescription renderer err descr =
    xmlToString $ concatMap render descr
    where render (X.NodeElement X.Element{..}) =
            let elem' = X.Element{elementName=elementName,elementAttributes=elementAttributes,
                                  elementNodes=concatMap render elementNodes} in
            if X.nameNamespace elementName == (Just $ T.pack "urn:unruh:proofedit:usererror")
            then rendererTransform renderer err elem'
            else [X.NodeElement elem']
          render node = [node]

renderShortDescription :: UserErrorRenderer -> UserError -> String
renderShortDescription renderer err = renderDescription renderer err $ shortDescription err

renderLongDescription :: UserErrorRenderer -> UserError -> String
renderLongDescription renderer err = renderDescription renderer err $ longDescription err

instance Show UserError where
    show err = intercalate "\n" (shortDescr:datas)
        where shortDescr :: String = xmlToString $ shortDescription err
              datas :: [String] = map (\(k,v) -> "    "++k++" = "++show v) $ Map.assocs $ errorData err


-- TODO: remove this
miniUserError :: String -> UserError
miniUserError err = UserError { userErrorID="minierror",
                                shortDescription=[X.NodeContent $ T.pack err],
                                longDescription=[X.NodeContent $ T.pack long],
                                errorData=Map.empty }
    where long = "No long description available yet.\nSo, instead, some dummy text.\n"++err

{- | Warning: directory must have immutable content -}
userErrorDB :: String -> String -> UserError
userErrorDB dir errName =
    let path = combine dir ("ue"++errName++".xhtml") in
    System.IO.Unsafe.unsafePerformIO $ userErrorFromFile errName path


htmlXMLName :: String -> X.Name
htmlXMLName name = X.Name { X.nameLocalName=T.pack name,
                            X.nameNamespace=Just "http://www.w3.org/1999/xhtml",
                            X.namePrefix=Nothing }

internalError :: UserError -> String -> [X.Node]
internalError err msg =
    let fullMsg = T.concat ["Internal error, please report: ",
                            T.pack (userErrorID err), ", ",
                            T.pack msg] in
    [X.NodeElement X.Element {X.elementName=htmlXMLName "span",
                              X.elementAttributes=Map.singleton (unqName "class") "internalerror",
                              X.elementNodes=[X.NodeContent fullMsg]}]



















