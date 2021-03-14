{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Strat.ZipTree
  ( expandTo
  , decendUntil
  , maxValue
  , minValue
  , negaMax
  , negaRnd
  , NegaResult(..)
  , NegaMoves(..)
  , Sign(..)
  , showMoveSeq
  , showTC
  , sortFromResult
  , treeSize
  , ZipTreeNode(..)
  ) where

import qualified Data.List as List
import Data.Sort
import Data.Tree
import Data.Tree.Zipper
import System.Random

data Sign = Pos | Neg
  deriving (Eq, Ord, Show)

data AlphaBeta = AlphaBeta
  { alpha :: Float
  , beta :: Float }
  deriving Show

maxValue :: Float
maxValue = 1000000.0

minValue :: Float
minValue = - maxValue

data TraceCmp a = Max | Min | TraceCmp (a, [a], Float)
  deriving Show

instance Eq a => Eq (TraceCmp a) where
   (==) Max Max = True
   (==) Max _ = False
   (==) _ Max = False
   (==) Min Min = True
   (==) Min _ = False
   (==) _ Min = False
   (==) (TraceCmp (_, _, x)) (TraceCmp (_, _, y)) = x == y

instance (Ord a, Show a) => Ord (TraceCmp a) where
   (<=) Max Max = True
   (<=) Max _ = False
   (<=) _  Max = True
   (<=) Min Min = True
   (<=) Min _ = True
   (<=) _ Min = False
   (<=) (TraceCmp (_, _, x)) (TraceCmp (_, _, y)) = x <= y

revTraceCmp :: TraceCmp a -> TraceCmp a
revTraceCmp (TraceCmp (tc, tcs, x)) = TraceCmp (tc, reverse tcs, x)
revTraceCmp Max = Max
revTraceCmp Min = Min

initAlphaBeta :: AlphaBeta
initAlphaBeta = AlphaBeta
  { alpha = minValue
  , beta = maxValue }

class ZipTreeNode a where
  ztnEvaluate :: a -> Float
  ztnMakeChildren :: a -> [Tree a]
  ztnSign :: a -> Sign

  ztnDeepDecend :: a -> Bool
  ztnDeepDecend _ = False

instance Ord a => Ord (Tree a) where
   (<=) (Node x _xs) (Node y _ys) = x <= y

-- TODO: Can probably get rid of NegaMoves now
data NegaMoves a = NegaMoves
  { evalScore :: Float
  , evalNode :: a
  , moveNode :: a
  , moveSeq :: [a] }
  deriving (Eq)

toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves (TraceCmp (a, as, z)) =
    NegaMoves { evalScore = z
              , evalNode = a
              , moveNode = head as -- never empty
              , moveSeq = as }
toNegaMoves Min = error "received 'Min' in toNegaMoves?"
toNegaMoves Max = error "received 'Max' in toNegaMoves?"

data NegaResult a = NegaResult
  { best :: NegaMoves a
  , alternatives :: [NegaMoves a]
  , allMoves :: [NegaMoves a]
  , evalCount :: Int }

expandTo :: (Ord a, Show a, ZipTreeNode a)
         => Tree a
         -> Int
         -> Int
         -> Tree a
expandTo t depth critDepth =
  let newTree = decendUntil (fromTree t) 1 depth critDepth
  in newTree

sortFromResult :: forall a. (Ord a, Show a, ZipTreeNode a)
         => Tree a
         -> NegaResult a
         -> Tree a
sortFromResult treeRoot prevResult =
    let tpos = fromTree treeRoot
        theChildren = subForest treeRoot
        moves = allMoves prevResult
        pairs = foldr f [] moves where
            f nmv acc =
                case List.find (\t -> rootLabel t == moveNode nmv ) theChildren of
                    Nothing -> error $ "sorting error: couldn't find: " ++ show (moveNode nmv)
                    Just tMatch -> (tMatch, evalScore nmv) : acc
        sorted = sortOn (\(_, x) -> -x) pairs
        sortedChildren = fst <$> sorted
    in toTree $ modifyTree (\(Node x _) -> Node x sortedChildren) tpos

decendUntil :: (Ord a, Show a, ZipTreeNode a)
            => TreePos Full a
            -> Int
            -> Int
            -> Int
            -> Tree a
decendUntil z curDepth goalDepth critDepth
    -- regular decent
    | curDepth <= goalDepth =
        let !theChildren = buildChildren z curDepth goalDepth critDepth
        in (toTree $ modifyTree (\(Node x _) -> Node x theChildren) z)
    -- past the goal depth and the parent isn't q crit -- stop
    | curDepth > goalDepth
    , ztnDeepDecend (label z) == False = toTree z
    -- past the goal depth and the crit depth -- stop
    | curDepth > goalDepth
    , curDepth > critDepth = toTree z
    -- crits only
    | otherwise = -- crits only...
        let unfiltered = buildChildren z curDepth goalDepth critDepth
            !theChildren = filterDeepDecentChildren unfiltered
        in (toTree $ modifyTree (\(Node x _) -> Node x theChildren) z)

filterDeepDecentChildren :: ZipTreeNode a => [Tree a] -> [Tree a]
filterDeepDecentChildren xs = filter (\t -> ztnDeepDecend (rootLabel t)) xs

buildChildren :: forall a. (Ord a, Show a, ZipTreeNode a)
              => TreePos Full a
              -> Int
              -> Int
              -> Int
              -> [Tree a]
buildChildren z curDepth goalDepth critDepth =
    let tempLabel = label z
        tempForest = subForest $ toTree z
        theChildren = if length tempForest /= 0
            then tempForest
            else ztnMakeChildren tempLabel
        (results, _) = zipFoldR (zipFoldFn curDepth goalDepth critDepth)
                       ([], children z) theChildren
    in results

zipFoldFn :: (Ord a, Show a, ZipTreeNode a)
  => Int -> Int -> Int
  -> Tree a
  -> ([Tree a], TreePos Empty a)
  -> ([Tree a], TreePos Empty a)
zipFoldFn curDepth goalDepth critDepth t (xs, childPos) =
    let zippedChild = fromTree t
        newT = decendUntil zippedChild (curDepth + 1) goalDepth critDepth
        tmp = insert newT childPos
        nextChildPos = nextSpace tmp
    in (newT : xs, nextChildPos)

zipFoldR :: forall a. Show a
         => ( Tree a
              -> ([Tree a], TreePos Empty a)
              -> ([Tree a], TreePos Empty a) )
         -> ([Tree a], TreePos Empty a)
         -> [Tree a]
         -> ([Tree a], TreePos Empty a)
zipFoldR f = loop
  where
    loop :: ([Tree a], TreePos Empty a)
         -> [Tree a]
         -> ([Tree a], TreePos Empty a)
    loop (acc, z ) [] = (acc, z)
    loop (acc, z) (x : xs) =
        let (ys, z') = f x (acc, z)
        in loop (ys, z') xs

-- alpha-beta comparison
-- A return value of True means the rest of the tree can be pruned
canPrune :: (Show a) => AlphaBeta -> a -> Bool -> Bool
canPrune AlphaBeta{..} _x enablePruning =
    enablePruning && (alpha >= beta)

updateAlphaBeta :: Show a => Sign -> AlphaBeta -> Float -> a -> AlphaBeta
updateAlphaBeta Pos alpBet newAlpha _x =
    let oldAlpha = alpha alpBet
        maybeUpdated = max oldAlpha newAlpha
    in if maybeUpdated == oldAlpha
           then alpBet
           else alpBet {alpha = maybeUpdated }
updateAlphaBeta Neg alpBet newBeta _x =
    let oldBeta = beta alpBet
        maybeUpdated = min oldBeta newBeta
    in if maybeUpdated == oldBeta
           then alpBet
           else alpBet {beta = maybeUpdated }

negaMax :: (Ord a, Show a, ZipTreeNode a) => Tree a -> Bool -> NegaResult a
negaMax t enablePruning =
  let sign = ztnSign $ rootLabel t
      alphaBeta = initAlphaBeta
      (theBest, traceCmps, ec) = alphaBetaPrune t [] alphaBeta sign enablePruning 0 0
  in NegaResult { best = toNegaMoves (revTraceCmp theBest)
                , allMoves = toNegaMoves . revTraceCmp <$> traceCmps
                , alternatives = []
                , evalCount = ec }

alphaBetaPrune :: forall a. (Ord a, Show a, ZipTreeNode a)
         => Tree a -> [a] -> AlphaBeta -> Sign -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
alphaBetaPrune t cmpList alphaBeta sign enablePruning ec lvl =
    let x = rootLabel t
        xs = reverse $ subForest t
    in if null xs then
        (TraceCmp (x, cmpList, ztnEvaluate x), [], ec)
    else
        case sign of
            Pos -> maxLoop x xs cmpList Min [] alphaBeta enablePruning ec (lvl + 1)
            Neg -> minLoop x xs cmpList Max [] alphaBeta enablePruning ec (lvl + 1)

maxLoop :: forall a. (Ord a, Show a, ZipTreeNode a)
         => a -> [Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
maxLoop _ [] _cmpList tcCurrentBest tcAltsAcc _ _ ec _lvl = (tcCurrentBest, tcAltsAcc, ec)
maxLoop parTemp (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta enablePruning ec lvl =
    let (tcPossibleBest, _, ec')
            = alphaBetaPrune t (rootLabel t : cmpList) alphaBeta Neg enablePruning ec lvl
        (tcNewBest, ec'') = (max tcCurrentBest tcPossibleBest, ec' + 1)
        TraceCmp(z, _zs, zVal) = tcNewBest
        newAlphaBeta = updateAlphaBeta Pos alphaBeta zVal z
    in if canPrune newAlphaBeta z enablePruning
       then (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
       else maxLoop parTemp ts cmpList tcNewBest (tcPossibleBest : tcAltsAcc) newAlphaBeta enablePruning ec'' lvl

minLoop :: forall a. (Ord a, Show a, ZipTreeNode a)
         => a -> [Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
minLoop _ [] _cmpList tcCurrentBest tcAltsAcc _ _ ec _lvl = (tcCurrentBest, tcAltsAcc, ec)
minLoop parTemp (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta enablePruning ec lvl =
    let (tcPossibleBest, _, ec')
             = alphaBetaPrune t (rootLabel t : cmpList) alphaBeta Pos enablePruning ec lvl
        (tcNewBest, ec'') = (min tcCurrentBest tcPossibleBest, ec' + 1)
        TraceCmp(z, _zs, zVal) = tcNewBest
        newAlphaBeta = updateAlphaBeta Neg alphaBeta zVal z
    in if canPrune newAlphaBeta z enablePruning
           then (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
           else minLoop parTemp ts cmpList tcNewBest (tcPossibleBest : tcAltsAcc) newAlphaBeta enablePruning ec'' lvl

negaRnd :: (Ord a, Show a, ZipTreeNode a, RandomGen g)
        => Tree a -> g -> Float -> Bool -> NegaResult a
negaRnd t gen percentVar enablePruning =
    let sign = ztnSign (rootLabel t)
        alphaBeta = initAlphaBeta
        (theBest, traceCmps, ec) = alphaBetaPrune t [] alphaBeta sign enablePruning 0 0
        noDup = List.delete theBest traceCmps
        close = filter (\x -> isWithin x theBest percentVar) noDup
        pickedTC@(TraceCmp (picked, pickedPath, pickedVal)) = pickOne gen (theBest : close)
        notPicked = List.delete pickedTC close
        revNotPicked = revTraceCmp <$> notPicked
    in NegaResult { best = toNegaMoves (TraceCmp (picked, reverse pickedPath, pickedVal))
                  , alternatives = toNegaMoves <$> revNotPicked
                  , allMoves = toNegaMoves . revTraceCmp <$> traceCmps
                  , evalCount = ec}

pickOne :: RandomGen g => g -> [TraceCmp a] -> TraceCmp a
pickOne gen choices =
    let (r, _g) = randomR (0, length choices - 1) gen
    in choices !! r

isWithin :: ZipTreeNode a => TraceCmp a -> TraceCmp a -> Float -> Bool
isWithin (TraceCmp (_, _, bst)) (TraceCmp (_, _, possible)) percentVar =
    let ratio = possible / bst
    in (ratio <= (1 + percentVar)) && (ratio >= (1 - percentVar))
isWithin _ _ _ = error "'Min' or 'Max' passed to isWithin?"

showTC :: (Show a) => TraceCmp a -> String
showTC Max = "<Max>"
showTC Min = "<Min>"
showTC (TraceCmp (x, xs, v)) =
    let midStr =
          if length xs == 1 then "."
          else "\n[ " ++ List.intercalate ",\n\t" (fmap show (reverse xs)) ++ " ]"
    in "x: " ++ show x ++ "\n"
      ++ "xs: " ++ midStr ++ "\n"
      ++ "v: " ++ show v

showMoveSeq :: (Show a) => TraceCmp a -> String
showMoveSeq Max = "<Max>"
showMoveSeq Min = "<Min>"
showMoveSeq (TraceCmp (_x, xs, _v)) =
    let strs = fmap show (reverse xs)
        str = List.intercalate ", " strs
    in "[" ++ str ++ "]"

--------------------------------------------------------------------------------
--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
--------------------------------------------------------------------------------
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
             in (sum levelTotals, levelTotals)
