-----------------------------------------------------------------------------
--
-- Module      :  Transformations.SMT
-- Copyright   :
-- License     :  OtherLicense
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Transformations.SMT (
    Solver, withSolver, isEqual
) where

import SimpleSMT
import Control.Concurrent.MVar
       (modifyMVar, newMVar, MVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (onException)

import Openmath.Types
import Control.Monad (forM_, forM)
import Data.Set (singleton, Set)
import Control.Monad.Writer (Writer, runWriter, tell)
import Openmath.TeX

-- $setup
-- >>> import Openmath.TeX
-- >>> let fromTeX t = do conf <- texDefaultConfiguration; return $ texToOpenmath conf t

singletonSolver :: MVar (Maybe Solver)
{-# NOINLINE singletonSolver #-}
singletonSolver = unsafePerformIO $ newMVar Nothing

initializeSolver :: IO Solver
initializeSolver = do
    solver <- newSolver "/opt/z3/bin/z3" (words "-smt2 -in") Nothing
    return solver

-- | Runs io using a singleton instance of the prover (threadsafe)
-- >>> withSolver $ \s -> declare s "varname" tBool
-- Atom "varname"
withSolver :: (Solver -> IO a) -> IO a
withSolver io =
    modifyMVar singletonSolver $ \var -> do
        solver <- case var of Just s -> return s; Nothing -> initializeSolver
        (do push solver; res <- io solver; pop solver; return (Just solver, res))
            `onException` (print "Killing Z3" >> stop solver)


openmathToSexpr' :: Openmath -> Writer (Set String) SExpr

openmathToSexpr' (OMA _ (OMS _ "arith1" "plus") args) = appl "+" args
openmathToSexpr' (OMA _ (OMS _ "arith1" "minus") args) = appl "-" args
openmathToSexpr' (OMA _ (OMS _ "arith1" "divide") args) = appl "/" args
openmathToSexpr' (OMA _ (OMS _ cd name) args) = appl (cd++"."++name) args
openmathToSexpr' (OMA _ hd args) = appl "applyFunction" (hd:args)

openmathToSexpr' (OMI _ i) = return $ Atom $ show i
openmathToSexpr' (OMF _ n) = return $ Atom $ show n
openmathToSexpr' (OMV _ name) = do
    tell (singleton $ name++"$")
    return $ Atom (name++"$")

openmathToSexpr' OME{} = error "openmathToSexpr: OME"
openmathToSexpr' OMSTR{} = error "openmathToSexpr: OMSTR"
openmathToSexpr' OMBIND{} = error "openmathToSexpr: OMBIND"
openmathToSexpr' OMS{} = error "openmathToSexpr: OMS"
openmathToSexpr' OMB{} = error "openmathToSexpr: OMB"

appl :: String -> [Openmath] -> Writer (Set String) SExpr
appl op args = do
    args' <- forM args openmathToSexpr'
    return $ List (Atom op:args')


openmathToSexpr :: Openmath -> (SExpr,Set String)
openmathToSexpr om = runWriter (openmathToSexpr' om)


-- | Checks whether a and b are equal
-- >>> a <- fromTeX "x+y"
-- >>> b <- fromTeX "y+x-x+x"
-- >>> c <- fromTeX "x+x"
-- >>> withSolver $ \s -> isEqual s a b
-- Just True
-- >>> withSolver $ \s -> isEqual s a c
-- Just False
isEqual :: Solver -> Openmath -> Openmath -> IO (Maybe Bool)
isEqual solver a b = do
    let (a',avars) = openmathToSexpr a
    let (b',bvars) = openmathToSexpr b
    let goal = SimpleSMT.not (eq a' b')
    --let vars = foldr' (:) [] (mappend avars bvars)
    let vars = mappend avars bvars
    forM_ vars $ \v -> declare solver v tReal
    assert solver goal
    res <- check solver
    return $ case res of Sat -> Just False; Unsat -> Just True; Unknown -> Nothing
