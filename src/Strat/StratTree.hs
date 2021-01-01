{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Strat.StratTree
  ( expandTo
  , decendUntil
  , DecentFilterF
  , MakeChildrenF
  , negaMax
  , negaRnd
  , NegaResult(..)
  , NegaMoves(..)
  , Sign(..)
  , treeSize
  ) where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Mutable
import Data.Tree
import System.Random
import Debug.Trace

instance Mutable s a => Mutable s (Tree a) where
    type Ref s (Tree a) = GRef s (Tree a)

type MakeChildrenF a = a -> [Tree a]

type DecentFilterF a = Maybe (a -> Int -> Bool)

newtype TraceCmp a = TraceCmp (a, [a])

instance Eq a => Eq (TraceCmp a) where
   (==) (TraceCmp (x, _xs)) (TraceCmp (y, _ys)) = x == y

instance (Ord a, Show a) => Ord (TraceCmp a) where
   (<=) (TraceCmp (x, _xs)) (TraceCmp (y, _ys)) = x <= y

newtype Sign = Sign { signToInt :: Int }

data NegaMoves a = NegaMoves
  { branchScore :: a
  , moveSeq :: [a] }
  deriving (Eq)

toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves (TraceCmp (x, xs)) =
    NegaMoves { branchScore = x
              , moveSeq = xs }

data NegaResult a = NegaResult
  { best :: NegaMoves a
  , alternatives :: [NegaMoves a] }

expandTo :: Mutable s a => Ref s (Tree a)
         -> MakeChildrenF a -> DecentFilterF a
         -> Int
         -> ST s (Tree a)
expandTo r makeChildren decFilter depth =
    decendUntil r makeChildren decFilter 0 depth

decendUntil :: (Mutable s a, Mutable s (Tree a)) => Ref s (Tree a)
            -> MakeChildrenF a -> DecentFilterF a
            -> Int -> Int
            -> ST s (Tree a)
decendUntil r makeChildren decFilter curDepth goalDepth =
    if curDepth == goalDepth
        then freezeRef r
        else do
            tempLabel <- freezePart (fieldMut #rootLabel) r
            -- if DecentFilterF exists, decend on nodes passing filter only
            if checkFilter decFilter tempLabel curDepth == False
                then freezeRef r
                else do
                    children <- buildChildren r makeChildren decFilter curDepth goalDepth
                    modifyPart (fieldMut #subForest) r (\_xs -> children)
                    freezeRef r

checkFilter :: forall a. DecentFilterF a -> a -> Int -> Bool
checkFilter Nothing _ _ = True
checkFilter (Just f) x depth = f x depth

buildChildren :: (Mutable s a, Mutable s (Tree a)) => Ref s (Tree a)
              -> MakeChildrenF a -> DecentFilterF a
              -> Int -> Int
              -> ST s [Tree a]
buildChildren parentRef makeChildren decFilter curDepth goalDepth = do
    tempLabel <- freezePart (fieldMut #rootLabel) parentRef
    tempForest <- freezePart (fieldMut #subForest) parentRef
    let theChildren = if not (null tempForest)
          then tempForest
          else makeChildren tempLabel
    foldM f [] theChildren
    where
        f acc t' = do
          r' <- thawRef t'
          newTree <- decendUntil r' makeChildren decFilter (curDepth + 1) goalDepth
          return (newTree : acc)

negaMax :: (Ord a, Show a) => Tree a -> Sign -> NegaResult a
negaMax t sign =
  let (theBest, traceCmps) = negaLoop t [] sign
      noDup = delete theBest traceCmps
      revTraceCmps = fmap (\(TraceCmp (tc, tcs)) -> TraceCmp (tc, reverse tcs)) noDup
  in NegaResult { best = toNegaMoves theBest
                , alternatives = toNegaMoves <$> revTraceCmps }

negaLoop :: forall a. (Ord a, Show a) => Tree a -> [a] -> Sign -> (TraceCmp a, [TraceCmp a])
negaLoop t cmpList sign =
    let xs = subForest t
    in if null xs then (TraceCmp (rootLabel t, cmpList), [])
        else let traceCmps = foldr f [] xs
                   where
                   f :: (Ord a, Show a) => Tree a -> [TraceCmp a] -> [TraceCmp a]
                   f t' acc =
                     let (tc, _tcs) = negaLoop t' (rootLabel t' : cmpList) (flipSign sign)
                     in tc : acc
             in findBest traceCmps sign

findBest :: (Ord a, Show a) => [TraceCmp a] -> Sign -> (TraceCmp a, [TraceCmp a])
findBest theList (Sign 1) = (maximum theList, theList)
findBest theList _ = (minimum theList, theList)

--------------------------------------------------------------------------------
-- Replace findBest with this version for detailed negamax output
--------------------------------------------------------------------------------
_findBest :: (Ord a, Show a) => [TraceCmp a] -> Sign -> (TraceCmp a, [TraceCmp a])
_findBest theList (Sign 1) =
    let listStr = intercalate ", " (showTCs theList)
        maxStr = showTC (maximum theList)
        str = "\nfindBest (+):\n" ++ maxStr ++ "\nis the max of:[\n" ++ listStr ++ " ]"
    in trace str (maximum theList, theList)
_findBest theList _ =
    let listStr = intercalate ", " (showTCs theList)
        minStr = showTC (minimum theList)
        str = "\nfindBest (-)\n" ++ minStr ++ "\nis the min of:[\n" ++ listStr ++ " ]"
    in trace str (minimum theList, theList)

showTC :: (Show a) => TraceCmp a -> String
showTC (TraceCmp (_x, xs)) =
    "\n[ " ++ intercalate ", " (fmap show (reverse xs)) ++ " ]"

showTCs :: (Show a) => [TraceCmp a] -> [String]
showTCs = fmap showTC
--------------------------------------------------------------------------------

flipSign :: Sign -> Sign
flipSign (Sign 1) = Sign (-1)
flipSign _ = Sign 1

negaRnd :: (Ord a, Show a, RandomGen g) => Tree a -> Sign -> g -> (a -> Float) -> Float -> NegaResult a
negaRnd t sign gen toFloat percentVar =
  let (theBest, traceCmps) = negaLoop t [] sign
      noDup = delete theBest traceCmps
      close = filter (\x -> isWithin x theBest toFloat percentVar) noDup
      pickedTC@(TraceCmp (picked, pickedPath)) = pickOne gen (theBest : close)
      notPicked = delete pickedTC close
      revNotPicked = fmap (\(TraceCmp (tc, tcs)) -> TraceCmp (tc, reverse tcs)) notPicked
  in NegaResult { best = toNegaMoves (TraceCmp (picked, reverse pickedPath))
                , alternatives = toNegaMoves <$> revNotPicked }

pickOne :: RandomGen g => g -> [TraceCmp a] -> TraceCmp a
pickOne gen choices =
  let (r, _g) = randomR (0, length choices - 1) gen
  in choices !! r

isWithin :: TraceCmp a -> TraceCmp a -> (a -> Float) -> Float -> Bool
isWithin (TraceCmp (bst, _)) (TraceCmp (possible, _)) toFloat percentVar =
    let ratio = toFloat possible / toFloat bst
    in (ratio <= (1 + percentVar)) && (ratio >= (1 - percentVar))

--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
                in (sum levelTotals, levelTotals)
