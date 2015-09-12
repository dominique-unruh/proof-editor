{-# LANGUAGE RecordWildCards, RankNTypes #-}
module UserError.Renderer (renderer) where

import UserError.UserError
import qualified Text.XML as X
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Dynamic
import Misc (unqName)
--import Openmath.Types
import Openmath.Pmathml
import Openmath.Utils (getSubterm)

{-# ANN module "Hlint: ignore Warning: Use fromMaybe" #-}

lookupData :: Typeable a => UserError -> Map.Map X.Name T.Text -> String -> a -> (Map.Map X.Name T.Text,a)
lookupData err attrs attrName def =
    let attrName' = unqName attrName in
    case Map.lookup attrName' attrs of
        Nothing -> (attrs,def)
        Just dataName ->
            case Map.lookup (T.unpack dataName) $ errorData err of
                Nothing -> error $ "No error-data '"++T.unpack dataName++"' in user error '"
                                    ++userErrorID err++"'"
                Just (Dyn dat _) -> case fromDynamic dat of
                    Nothing -> error $ "Error-data '"++T.unpack dataName++"' has type '"
                                       ++show (dynTypeRep dat)++"', not '"
                                       ++show(typeOf def)++
                                       "' (in user error '"++userErrorID err++"')"
                    Just val -> (Map.delete attrName' attrs, val)

transform :: PMMLConfiguration -> String -> UserError -> Map.Map X.Name T.Text -> [X.Node] -> [X.Node]
transform pmmlConfig "formula" err attr _ =
    let (attr1,path) = lookupData err attr "path" [] in
    let (attr2,formula) = lookupData err attr1 "name" (error "no formula specified") in
    let subterm = getSubterm formula path in
    let pmml = pmmlRender pmmlConfig subterm in
    -- TODO: surrounding <math> tag missing
    [X.NodeElement pmml { X.elementAttributes=Map.union attr2 $ X.elementAttributes pmml }]

transform _ tag err _ _ = internalError err $ "unknown usererror tag: "++tag

renderer :: PMMLConfiguration -> UserErrorRenderer
renderer pmmlConfig = UserErrorRenderer {
    rendererTransform = \err X.Element{..} ->
        transform pmmlConfig (T.unpack $ X.nameLocalName elementName) err elementAttributes elementNodes
}

