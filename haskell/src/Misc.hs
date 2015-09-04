
module Misc where

import qualified Text.XML as X
import qualified Text.XML.Cursor as X
import Text.XML.Cursor ((>=>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Arrow ((***))


xmlToString :: [(String,String)] -> X.Element -> String
xmlToString ns =
    let settings = X.def { X.rsNamespaces=map (T.pack *** T.pack) ns } in \xml ->
    let doc = X.Document {X.documentPrologue=noPrologue, X.documentEpilogue=[], X.documentRoot=xml}
              where noPrologue = X.Prologue { X.prologueBefore=[], X.prologueDoctype=Nothing, X.prologueAfter=[] } in
    let text = X.renderText settings doc in
    let text' = TL.drop 38 text in
    TL.unpack text'

xmlText :: X.Element -> String
xmlText xml =
    concatMap T.unpack $ (X.descendant >=> X.content) $ X.fromNode $ X.NodeElement xml
