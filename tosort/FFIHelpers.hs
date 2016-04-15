{-# LANGUAGE TemplateHaskell #-}

module FFIHelpers () where

import Openmath.Types (Path, Openmath)
import FFIExports (exportFFI)
import Transformations.Common (Error)
import UserError.UserError
       (renderLongDescription, renderShortDescription,
        UserErrorRenderer(..), UserError)
import Control.Monad.Except (runExcept)
import Openmath.Pmathml (pmmlDefaultConfiguration)
import qualified UserError.Renderer

consOpenmathMaybePath :: (Openmath,Maybe Path) -> [(Openmath,Maybe Path)] -> [(Openmath,Maybe Path)]
consOpenmathMaybePath = (:)
exportFFI 'consOpenmathMaybePath

nilOpenmathMaybePath :: [(Openmath,Maybe Path)]
nilOpenmathMaybePath = []
exportFFI 'nilOpenmathMaybePath

pairOpenmathMaybePath :: Openmath -> Maybe Path -> (Openmath,Maybe Path)
pairOpenmathMaybePath a b = (a,b)
exportFFI 'pairOpenmathMaybePath

runExceptTrafo :: Error Openmath -> Either UserError Openmath
runExceptTrafo = runExcept
exportFFI 'runExceptTrafo

isError :: Either UserError Openmath -> Bool
isError (Left _) = True
isError (Right _) = False
exportFFI 'isError

ueRender :: UserErrorRenderer
ueRender = UserError.Renderer.renderer pmmlDefaultConfiguration

userErrorShortDescr :: Either UserError Openmath -> String
userErrorShortDescr (Left err) = renderShortDescription ueRender err
userErrorShortDescr (Right _) = "no error"
exportFFI 'userErrorShortDescr


userErrorLongDescr :: Either UserError Openmath -> String
userErrorLongDescr (Left err) = renderLongDescription ueRender err
userErrorLongDescr (Right _) = "no error"
exportFFI 'userErrorLongDescr

errorRight :: Either UserError Openmath -> Openmath
errorRight (Left _) = error "left"
errorRight (Right om) = om
exportFFI 'errorRight
