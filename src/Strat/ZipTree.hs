{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Strat.ZipTree
  ( expandTo
  , decendUntil
  , DeepDecentFilterF
  , MakeChildrenF
  , negaMax
  , negaRnd
  , NegaResult(..)
  , NegaMoves(..)
  , Sign(..)
  , treeSize
  ) where

import qualified Data.List as List
import Data.Tree
import Data.Tree.Zipper
import System.Random
import Debug.Trace

type MakeChildrenF a = a -> [Tree a]

type DeepDecentFilterF a = Maybe (Int -> a -> Bool)

newtype TraceCmp a = TraceCmp (a, [a])

revTraceCmps :: TraceCmp a -> TraceCmp a
revTraceCmps (TraceCmp (tc, tcs)) = TraceCmp (tc, reverse tcs)

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


expandTo :: Show a
         => Tree a
         -> MakeChildrenF a -> DeepDecentFilterF a
         -> Int
         -> Tree a
expandTo t makeChildren decFilter depth =
    decendUntil (fromTree t) makeChildren decFilter 0 depth

decendUntil :: (Show a)
            => TreePos Full a
            -> MakeChildrenF a -> DeepDecentFilterF a
            -> Int
            -> Int
            -> Tree a
decendUntil z makeChildren decFilter curDepth goalDepth =
    let tempLabel  = label z
    -- if DeepDecentFilterF exists, decend deeper than the goal depth
    -- but only for the (parent) nodes that pass the filter
    in if curDepth >= goalDepth && checkDeepDecentParent decFilter curDepth tempLabel == False
        then toTree z
        else
            let unfiltered = buildChildren z makeChildren decFilter curDepth goalDepth
                theChildren =
                    if curDepth >= goalDepth then
                        filterDeepDecentChildren decFilter curDepth unfiltered
                        -- let str = "Decending for parent (depth " ++ show curDepth ++ "): " ++ show (label z)
                            -- filtered = filterDeepDecentChildren decFilter curDepth unfiltered
                            -- str2 = List.intercalate ", " (show . rootLabel <$> filtered)
                            -- in trace (str ++ "\n" ++ str2) filtered
                    else unfiltered
            in toTree $ modifyTree (\(Node x _) -> Node x theChildren) z


checkDeepDecentParent :: forall a. DeepDecentFilterF a -> Int -> a -> Bool
checkDeepDecentParent Nothing _ _ = False
checkDeepDecentParent (Just f) depth x  = f depth x

filterDeepDecentChildren :: DeepDecentFilterF a -> Int -> [Tree a] -> [Tree a]
filterDeepDecentChildren Nothing _ xs = xs
filterDeepDecentChildren (Just f) d xs = filter (\t -> (f d) (rootLabel t)) xs

buildChildren :: forall a. (Show a)
              => TreePos Full a
              -> MakeChildrenF a -> DeepDecentFilterF a
              -> Int -> Int
              -> [Tree a]
buildChildren z makeChildren decFilter curDepth goalDepth =
    let tempLabel = label z
        tempForest = toForest $ children z
        theChildren = if length tempForest == 0
            then tempForest
            else makeChildren tempLabel

        f :: (Tree a -> ([Tree a], TreePos Empty a) -> ([Tree a], TreePos Empty a))
        f t (xs, childPos) =
            let zippedChild = fromTree t
                t' = decendUntil zippedChild makeChildren decFilter (curDepth + 1) goalDepth
                tmp = insert t childPos
                nextChildPos = nextSpace tmp
            in (t' : xs, nextChildPos)
        (results, _) = zipFoldR f ([], children z) theChildren
    in results

zipFoldR :: forall a. (Tree a -> ([Tree a], TreePos Empty a) -> ([Tree a], TreePos Empty a))
         -> ([Tree a], TreePos Empty a)
         -> [Tree a]
         -> ([Tree a], TreePos Empty a)
zipFoldR f = loop
  where
    loop :: ([Tree a], TreePos Empty a) -> [Tree a ] -> ([Tree a], TreePos Empty a)
    loop (acc, z) [] = (acc, z)
    loop (acc, z) (x : xs) =
        let (ys, z') = f x (acc, z)
        in loop (ys, z') xs

negaMax :: (Ord a, Show a) => Tree a -> Sign -> NegaResult a
negaMax t sign =
  let (theBest, traceCmps) = negaLoop t [] sign
      noDup = List.delete theBest traceCmps
      -- revTraceCmps = fmap (\(TraceCmp (tc, tcs)) -> TraceCmp (tc, reverse tcs)) noDup
  in NegaResult { best = toNegaMoves (revTraceCmps theBest)
                , alternatives = toNegaMoves . revTraceCmps <$> noDup }

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

flipSign :: Sign -> Sign
flipSign (Sign 1) = Sign (-1)
flipSign _ = Sign 1

negaRnd :: (Ord a, Show a, RandomGen g) => Tree a -> Sign -> g -> (a -> Float) -> Float -> NegaResult a
negaRnd t sign gen toFloat percentVar =
  let (theBest, traceCmps) = negaLoop t [] sign
      noDup = List.delete theBest traceCmps
      close = filter (\x -> isWithin x theBest toFloat percentVar) noDup
      pickedTC@(TraceCmp (picked, pickedPath)) = pickOne gen (theBest : close)
      notPicked = List.delete pickedTC close
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

--------------------------------------------------------------------------------
-- Replace findBest with this version for detailed negamax output
--------------------------------------------------------------------------------
_findBest :: (Ord a, Show a) => [TraceCmp a] -> Sign -> (TraceCmp a, [TraceCmp a])
_findBest theList (Sign 1) =
    let listStr = List.intercalate ", " (showTCs theList)
        maxStr = showTC (maximum theList)
        str = "\nfindBest (+):\n" ++ maxStr ++ "\nis the max of:[\n" ++ listStr ++ " ]"
    in trace str (maximum theList, theList)
_findBest theList _ =
    let listStr = List.intercalate ", " (showTCs theList)
        minStr = showTC (minimum theList)
        str = "\nfindBest (-)\n" ++ minStr ++ "\nis the min of:[\n" ++ listStr ++ " ]"
    in trace str (minimum theList, theList)

showTC :: (Show a) => TraceCmp a -> String
showTC (TraceCmp (_x, xs)) =
    "\n[ " ++ List.intercalate ", " (fmap show (reverse xs)) ++ " ]"

showTCs :: (Show a) => [TraceCmp a] -> [String]
showTCs = fmap showTC

--------------------------------------------------------------------------------
--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
                in (sum levelTotals, levelTotals)
