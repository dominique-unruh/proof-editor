{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cmathml.QuickCheck where

import Control.Monad (replicateM)
import Test.QuickCheck (sized, resize, Gen, oneof, arbitrary, elements, Arbitrary)
import Cmathml.Types
import Cmathml.Utils (ciToBvar)

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

semanticsGen :: Gen Semantics
semanticsGen = 
    oneof [shrinkingList annot, return []]
            where annot = do
                    (cd,name) <- arbitrary
                    a <- annotGen
                    return (cd,name,a)

annotGen :: Gen Annotation
annotGen = fmap AnnotationCMML cmathmlGen

cnIntGen :: Gen Cmathml
cnIntGen = do
    sem <- semanticsGen
    i <- arbitrary
    return (CN sem $ Int i)

cnRealGen :: Gen Cmathml
cnRealGen = do
    sem <- semanticsGen
    r <- arbitrary
    return (CN sem $ Real r)

cnIEEEGen :: Gen Cmathml
cnIEEEGen = do
    sem <- semanticsGen
    d <- arbitrary
    return (CN sem $ IEEE d)

cnGen :: Gen Cmathml
cnGen = oneof [cnIntGen, cnRealGen, cnIEEEGen]

csGen :: Gen Cmathml
csGen = do
    sem <- semanticsGen
    s <- arbitrary
    return (CS sem s)

nonemptyListGen :: Arbitrary a => Gen [a]
nonemptyListGen = do
    x <- arbitrary
    xs <- arbitrary
    return $ x:xs

ciGen :: Gen Cmathml
ciGen = do
    sem <- semanticsGen
    name <- nonemptyListGen
    return (CI sem name)

csymbols :: [(String, String)]
csymbols = [("arith1","plus"),("arith1","minus"),("arith1","times"),("arith1","divide"),
    ("relation1","eq")]

csymbolGen :: Gen Cmathml
csymbolGen = do
    sem <- semanticsGen
    (cd,name) <- elements csymbols
    return $ CSymbol sem cd name

applyGen :: Gen Cmathml
applyGen = do
    (sem,hd,args) <- shrinkingTuple3 semanticsGen cmathmlGen (shrinkingList cmathmlGen)
    return $ Apply sem hd args

bindGen :: Gen Cmathml
bindGen = do
    (sem,hd,bvars,arg) <- shrinkingTuple4 semanticsGen cmathmlGen (shrinkingList bvarGen) cmathmlGen
    return $ Bind sem hd bvars arg

bvarGen :: Gen Bvar
bvarGen = fmap ciToBvar ciGen

leafGens :: [Gen Cmathml]
leafGens = [ cnGen, csGen, ciGen, csymbolGen ]

nonleafGens :: [Gen Cmathml]
nonleafGens = [ applyGen, bindGen ]

cmathmlGen :: Gen Cmathml
cmathmlGen = sized (\size -> if size<=1 then oneof leafGens else oneof $ leafGens++nonleafGens)


instance Arbitrary Cmathml where
    arbitrary = cmathmlGen

instance Arbitrary Annotation where
    arbitrary = annotGen
    
