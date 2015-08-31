module Main where

import UserError.UserError
       (UserErrorRenderer, renderShortDescription, renderLongDescription)
import System.Environment (getArgs)
import Openmath.TeX
import Openmath.Cmathml
import Data.List.Split (splitOn)
import Openmath.Types
import Transformations.Commutativity
--import System.IO (stderr, hPutStr)
--import System.Exit (exitFailure)
import Transformations.Common (Transformation)
import Transformations.Associativity (associativity)
import Transformations.ModusPonens (modusPonens)
import Control.Monad.Except (runExcept)
import Transformations.Substitution (substitution)
import Transformations.Compute (compute)
import Text.JSON
--import Data.Map.Strict (fromList)

(|>) :: t1 -> (t1 -> t) -> t
x |> f = f x

parsePath :: String -> Maybe Path
parsePath "-" = Nothing
parsePath "" = Just []
parsePath path = Just (map read (splitOn "." path))


cmmlPathPairs :: [String] -> [(Openmath,Maybe Path)]
cmmlPathPairs [] = []
cmmlPathPairs (cmml:path:rest) = (fromCmathml cmml, parsePath path) : cmmlPathPairs rest
cmmlPathPairs _ = error "Invalid CMML/Path interleaving (need even number of transformation arguments)"

trafos :: [(String, Transformation)]
trafos = [
    ("commutativity", commutativity),
    ("associativity", associativity),
    ("modusponens", modusPonens),
    ("substitution", substitution),
    ("compute", compute)
    ]

jsonOut :: JSON a => [(String,a)] -> IO ()
jsonOut = putStr . encode . showJSON . toJSObject

ueRender :: UserErrorRenderer
ueRender = ()

main:: IO()
main = do
    args <- getArgs
    case args of
        ["tex2cmml", tex] -> do config <- texDefaultConfiguration
                                jsonOut [("success","success"),("result",toCmathml $ texToOpenmath config tex)]
--                                putStrLn $ toCmathml $ texToOpenmath config tex
        "transform" : name : rest ->
            case lookup name trafos of
                Nothing -> error $ "Unknown trafo "++name
                Just trafo ->
                    case runExcept $ trafo (cmmlPathPairs rest) of
                        Left err -> jsonOut [("success","error"),
                                             ("error",renderShortDescription ueRender err),
                                             ("longError",renderLongDescription ueRender err)]
--                        do hPutStr stderr err; exitFailure
                        Right out -> jsonOut [("success","success"),("result",toCmathml out)]
--                            putStr (toCmathml out)
        _ -> jsonOut [("success","error"),("error","invalid arguments")]
