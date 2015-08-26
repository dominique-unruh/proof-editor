{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cmathml.QuickCheck where

import Control.Monad (replicateM)
import Test.QuickCheck (sized, resize, Gen, oneof, arbitrary, elements, Arbitrary)
import Cmathml.Types
import Cmathml.Utils (omvToBvar)

{-# ANN module "HLint: ignore Reduce duplication" #-}

shrinkingList :: Gen a -> Gen [a]
shrinkingList gen = sized (\size -> do
    let len = round (sqrt (fromIntegral size) :: Double)
    let size1 = size `div` len
    replicateM len (resize size1 gen))
    
shrinkingListN :: Gen a -> Int -> Gen [a]
shrinkingListN gen len = sized (\size -> do
    let size1 = size `div` len
    replicateM len (resize size1 gen))

shrinkingTuple2 :: Gen a -> Gen b -> Gen (a,b)
shrinkingTuple2 gen1 gen2 = sized (\size -> do
    let size1 = size `div` 2
    x1 <- resize size1 gen1
    x2 <- resize size1 gen2
    return (x1,x2))

shrinkingTuple3 :: Gen a -> Gen b -> Gen c -> Gen (a,b,c)
shrinkingTuple3 gen1 gen2 gen3 = sized (\size -> do
    let size1 = size `div` 3
    x1 <- resize size1 gen1
    x2 <- resize size1 gen2
    x3 <- resize size1 gen3
    return (x1,x2,x3))

shrinkingTuple4 :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a,b,c,d)
shrinkingTuple4 gen1 gen2 gen3 gen4 = sized (\size -> do
    let size1 = size `div` 4
    x1 <- resize size1 gen1
    x2 <- resize size1 gen2
    x3 <- resize size1 gen3
    x4 <- resize size1 gen4
    return (x1,x2,x3,x4))

attributionGen :: Gen Attribution
attributionGen = 
    oneof [shrinkingList annot, return []]
            where annot = do
                    (cd,name) <- arbitrary
                    a <- attribGen
                    return (cd,name,a)

attribGen :: Gen Attribute
attribGen = fmap AttributeOM cmathmlGen

omiGen :: Gen Openmath
omiGen = do
    sem <- attributionGen
    i <- arbitrary
    return (OMI sem i)

-- cnRealGen :: Gen Openmath
-- cnRealGen = do
--     sem <- attributionGen
--     r <- arbitrary
--     return (CN sem $ Real r)

omfGen :: Gen Openmath
omfGen = do
    sem <- attributionGen
    d <- arbitrary
    return (OMF sem d)

omstrGen :: Gen Openmath
omstrGen = do
    sem <- attributionGen
    s <- arbitrary
    return (OMSTR sem s)

nonemptyListGen :: Arbitrary a => Gen [a]
nonemptyListGen = do
    x <- arbitrary
    xs <- arbitrary
    return $ x:xs

omvGen :: Gen Openmath
omvGen = do
    sem <- attributionGen
    name <- nonemptyListGen
    return (OMV sem name)

omSymbols :: [(String, String)]
omSymbols = [("arith1","plus"),("arith1","minus"),("arith1","times"),("arith1","divide"),
    ("relation1","eq")]

omsGen :: Gen Openmath
omsGen = do
    sem <- attributionGen
    (cd,name) <- elements omSymbols
    return $ OMS sem cd name

omaGen :: Gen Openmath
omaGen = do
    (sem,hd,args) <- shrinkingTuple3 attributionGen cmathmlGen (shrinkingList cmathmlGen)
    return $ OMA sem hd args

ombindGen :: Gen Openmath
ombindGen = do
    (sem,hd,bvars,arg) <- shrinkingTuple4 attributionGen cmathmlGen (shrinkingList bvarGen) cmathmlGen
    return $ OMBIND sem hd bvars arg

bvarGen :: Gen Bvar
bvarGen = fmap omvToBvar omvGen

leafGens :: [Gen Openmath]
leafGens = [ omiGen, omfGen, omstrGen, omvGen, omsGen ]

nonleafGens :: [Gen Openmath]
nonleafGens = [ omaGen, ombindGen ]

cmathmlGen :: Gen Openmath
cmathmlGen = sized (\size -> if size<=1 then oneof leafGens else oneof $ leafGens++nonleafGens)


instance Arbitrary Openmath where
    arbitrary = cmathmlGen

instance Arbitrary Attribute where
    arbitrary = attribGen
    
