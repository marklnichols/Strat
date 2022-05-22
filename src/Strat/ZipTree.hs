{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , CompareMode(..) -- exported for testing
  , MateIn(..) -- exported for testing
  , mateInCompare -- exported for testing
  , maxScore
  , maxValueTemp
  , minScore
  , minValueTemp
  , negaMax
  , negaRnd
  , nodeHash
  , NegaResult(..)
  , NegaMoves(..)
  , Sign(..)
  , showMoveSeq
  , showTC
  , sortFromResult
  , treeSize
  , ZipTreeNode(..)
  ) where

import Data.Hashable
import Data.List (intercalate)
import qualified Data.List as List
import Data.Sort
import Data.Text (Text, pack, isInfixOf)
import qualified Data.Tree as T
import Data.Tree.Zipper
import GHC.Generics
import System.Random
import Text.Printf
import Debug.Trace

data CompareMode = Minimizing | Maximizing

newtype  ZipTree a = ZipTree {unZipTree :: T.Tree a}
  deriving Eq

data Sign = Pos | Neg
  deriving (Eq, Generic, Hashable, Ord, Show)

data AlphaBeta = AlphaBeta
  { alpha :: Float
  , beta :: Float }
  deriving Show

--TODO: put some of these things into an env inside a Reader
cmpTracing :: Bool
cmpTracing = False

pruneTracing :: Bool
pruneTracing = False

moveTraceStr :: Text
moveTraceStr = pack "[ E6-F4, E3xF4"

-- max min values used for negaMax initial values
maxValueTemp :: Float
maxValueTemp = 1000000000.0

minValueTemp :: Float
minValueTemp = - maxValueTemp

-- practical max / min for score values
-- e.g. used as a value for checkmate in chess
maxScore :: Float
maxScore = 1000000.0

minScore :: Float
minScore = - maxScore

data MateIn = MateIn (Maybe (Int, Sign))
  deriving (Eq, Show)

data TraceCmp a = Max | Min | TraceCmp (a, [a], Float, MateIn)

instance (Show a) => Show (TraceCmp a) where
  show x = showTC x

instance Eq a => Eq (TraceCmp a) where
   (==) Max Max = True
   (==) Min Min = True
   (==) Max _ = False
   (==) Min _ = False
   (==) _ Max = False
   (==) _ Min = False
   (==) (TraceCmp (_, _, x, MateIn Nothing)) (TraceCmp (_, _, y, MateIn Nothing)) = x == y
   (==) (TraceCmp (_, _, _, _)) (TraceCmp (_, _, _, MateIn Nothing)) = False
   (==) (TraceCmp (_, _, _, MateIn Nothing)) (TraceCmp (_, _, _, _)) = False
   (==) (TraceCmp (_, _, _, MateIn (Just (xMateIn, _xSgn))))
            (TraceCmp (_, _, _, MateIn (Just (yMateIn, _ySgn)))) = xMateIn == yMateIn

nodeHash :: (ZipTreeNode a, Hashable a) => a -> Int
nodeHash n = hash n


-- TODO: move this back to Ord instance again
-- special comparison function for TraceCmp
cmpTC :: TraceCmp a -> TraceCmp a -> Bool
cmpTC Max Max = True
cmpTC Min Min = True
cmpTC Max _   = False
cmpTC Min _   = True
cmpTC _   Max = True
cmpTC _   Min = False
cmpTC (TraceCmp (_, _, x, MateIn Nothing)) (TraceCmp (_, _, y, MateIn Nothing)) = x <= y
cmpTC (TraceCmp (_, _, x, MateIn (Just pairX))) (TraceCmp (_, _, y, MateIn (Just pairY))) =
    if pairX == pairY
      then x <= y
      else mateInCompare (MateIn (Just pairX)) (MateIn (Just pairY))
cmpTC (TraceCmp (_, _, _, maybeX)) (TraceCmp (_, _, _, maybeY)) =
     mateInCompare maybeX maybeY

maxTC :: Eq a => TraceCmp a -> TraceCmp a -> TraceCmp a
maxTC x y
  | x == y    = x
  | cmpTC x y = y
  | otherwise = x

minTC :: Eq a => TraceCmp a -> TraceCmp a -> TraceCmp a
minTC x y
  | x == y    = x
  | cmpTC x y = x
  | otherwise = y

mateInCompare :: MateIn -> MateIn ->  Bool
mateInCompare (MateIn Nothing) (MateIn Nothing) = error "mateInCompare -- this shouldn't happen"
mateInCompare (MateIn(Just (_, xSign))) (MateIn Nothing) =
  case xSign of
    Pos -> False
    Neg -> True
mateInCompare (MateIn Nothing) (MateIn (Just (_, ySign))) =
  case ySign of
    Pos -> True
    Neg -> False
mateInCompare (MateIn (Just (x, xSign))) (MateIn (Just (y,ySign))) =
  case xSign of
    Pos | ySign == Pos
          -> x >= y
        | ySign == Neg
          -> False
        | otherwise -> error "mateInCompare - shouldn't happen"
    Neg | ySign == Pos
          -> True
        | ySign == Neg
          -> x <= y
        | otherwise -> error "mateInCompare - shouldn't happen"

revTraceCmp :: TraceCmp a -> TraceCmp a
revTraceCmp Max = Max
revTraceCmp Min = Min
revTraceCmp (TraceCmp (tc, tcs, x, xMateIn)) = TraceCmp (tc, reverse tcs, x, xMateIn)

initAlphaBeta :: AlphaBeta
initAlphaBeta = AlphaBeta
  { alpha = minValueTemp
  , beta = maxValueTemp }

class ZipTreeNode a where
  ztnEvaluate :: a -> Float
  ztnMakeChildren :: a -> [T.Tree a]
  ztnSign :: a -> Sign
  ztnFinal :: a -> Bool

instance forall a. Ord a => Ord (ZipTree a) where
   (<=) r s =
     let x = unZipTree r
         y = unZipTree s
     in x <= y

-- TODO: Can probably get rid of NegaMoves now
data NegaMoves a = NegaMoves
  { evalScore :: Float
  , mateIn :: Maybe (Int, Sign)
  , evalNode :: a
  , moveNode :: a
  , moveSeq :: [a] }
  deriving (Eq)

toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves (TraceCmp (a, as, z, MateIn mi)) =
    NegaMoves { evalScore = z
              , mateIn = mi
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

expandTo :: forall a. (Ord a, Show a, ZipTreeNode a)
         => T.Tree a
         -> Int
         -> T.Tree a
expandTo t depth =
  decendUntil (fromTree t) 1 depth

sortFromResult :: forall a. (Ord a, Show a, ZipTreeNode a)
         => T.Tree a
         -> NegaResult a
         -> T.Tree a
sortFromResult treeRoot prevResult =
    let tpos = fromTree treeRoot
        theChildren = T.subForest treeRoot
        moves = allMoves prevResult
        pairs = foldr f [] moves where
            f nmv acc =
                case List.find (\t -> T.rootLabel t == moveNode nmv ) theChildren of
                    Nothing -> error $ "sorting error: couldn't find: " ++ show (moveNode nmv)
                    Just tMatch -> (tMatch, evalScore nmv) : acc
        sorted = sortOn (\(_, x) -> -x) pairs
        sortedChildren = fst <$> sorted
    in toTree $ modifyTree (\(T.Node x _) -> T.Node x sortedChildren) tpos

decendUntil :: (Ord a, Show a, ZipTreeNode a)
            => TreePos Full a
            -> Int
            -> Int
            -> T.Tree a
decendUntil z curDepth goalDepth
    | curDepth <= goalDepth =
        let !theChildren = buildChildren z curDepth goalDepth
        in toTree $ modifyTree (\(T.Node x _) -> T.Node x theChildren) z
    | otherwise = toTree z

buildChildren :: forall a. (Ord a, Show a, ZipTreeNode a)
              => TreePos Full a
              -> Int
              -> Int
              -> [T.Tree a]
buildChildren z curDepth goalDepth =
    let tempLabel = label z
        tempForest = T.subForest $ toTree z
        theChildren = if length tempForest /= 0
            then tempForest
            else ztnMakeChildren tempLabel
        (results, _) = zipFoldR (zipFoldFn curDepth goalDepth)
                       ([], children z) theChildren
    in results

zipFoldFn :: (Ord a, Show a, ZipTreeNode a)
  => Int -> Int
  -> T.Tree a
  -> ([T.Tree a], TreePos Empty a)
  -> ([T.Tree a], TreePos Empty a)
zipFoldFn curDepth goalDepth t (xs, childPos) =
    let zippedChild = fromTree t
        newT = decendUntil zippedChild (curDepth + 1) goalDepth
        tmp = insert newT childPos
        nextChildPos = nextSpace tmp
    in (newT : xs, nextChildPos)

zipFoldR :: forall a. Show a
         => ( T.Tree a
              -> ([T.Tree a], TreePos Empty a)
              -> ([T.Tree a], TreePos Empty a) )
         -> ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ([T.Tree a], TreePos Empty a)
zipFoldR f = loop
  where
    loop :: ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ([T.Tree a], TreePos Empty a)
    loop (acc, z ) [] = (acc, z)
    loop (acc, z) (x : xs) =
        let (ys, z') = f x (acc, z)
        in loop (ys, z') xs

-- alpha-beta comparison
-- A return value of (True, _) means the rest of the tree can be pruned
-- The string in the 2nd tuple component provides debug information
canPrune :: (Show a) => AlphaBeta -> TraceCmp a -> Bool -> String -> (Bool, String)
canPrune AlphaBeta{..} tcNewBest enablePruning moreInfo =
    if not pruneTracing
      then (enablePruning && alpha >= beta, "")
      else
        let str = printf "pruning during %s, alpha (%f) >= beta (%f) \n \
                         \ (Because of %s)" moreInfo alpha beta (showTC tcNewBest)
        in (enablePruning && alpha >= beta, str)

updateAlphaBeta :: Show a => Sign -> AlphaBeta -> Float -> TraceCmp a -> Int -> Bool -> AlphaBeta
updateAlphaBeta Pos alpBet newAlpha tc lvl enablePruning =
    if not enablePruning then alpBet
    else
        let oldAlpha = alpha alpBet
            maybeUpdated = max oldAlpha newAlpha
        in if maybeUpdated == oldAlpha
              then alpBet
              else
                if pruneTracing
                  then
                    let str = printf "updating alpha (level %d) from %f to %f (due to: %s)"
                              lvl oldAlpha newAlpha (show tc)
                        !temp = alpBet {alpha = maybeUpdated }
                    in trace str temp
                  else alpBet {alpha = maybeUpdated }

updateAlphaBeta Neg alpBet newBeta tc lvl enablePruning =
    if not enablePruning then alpBet
    else
        let oldBeta = beta alpBet
            maybeUpdated = min oldBeta newBeta
        in if maybeUpdated == oldBeta
              then alpBet
              else
                if pruneTracing
                  then
                    let str = printf "updating beta (level %d) from %f to %f (due to: %s)"
                              lvl oldBeta newBeta (show tc)
                        !temp = alpBet {beta = maybeUpdated }
                    in trace str temp
                  else alpBet {beta = maybeUpdated }

negaMax :: (Ord a, Show a, ZipTreeNode a, Hashable a) => T.Tree a -> Bool -> NegaResult a
negaMax t enablePruning =
  let sign = ztnSign $ T.rootLabel t
      alphaBeta = initAlphaBeta
      (theBest, traceCmps, ec) = alphaBetaPrune t [] alphaBeta sign enablePruning 0 0
  in NegaResult { best = toNegaMoves (revTraceCmp theBest)
                , allMoves = toNegaMoves . revTraceCmp <$> traceCmps
                , alternatives = []
                , evalCount = ec }

alphaBetaPrune :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
         => T.Tree a -> [a] -> AlphaBeta -> Sign -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
alphaBetaPrune t cmpList alphaBeta sign enablePruning ec lvl =
    let x = T.rootLabel t
        xs = reverse $ T.subForest t
    in if null xs || kingCaptureRisk x then
         let tc = tcFromT t cmpList lvl
         in (tc, [], ec)
    else
      case sign of
          Pos -> maxLoop xs cmpList Min [] alphaBeta enablePruning ec (lvl + 1)
          Neg -> minLoop xs cmpList Max [] alphaBeta enablePruning ec (lvl + 1)

tcFromT :: forall a. (Ord a, Show a, ZipTreeNode a)
         => T.Tree a -> [a] -> Int -> TraceCmp a
tcFromT t cmpList lvl =
    let x = T.rootLabel t
        xVal = ztnEvaluate x
        mateIn = MateIn
          (if xVal >= maxScore
             then Just (lvl, Pos)
             else if xVal <= minScore
               then Just (lvl, Neg)
               else Nothing)
        in
          TraceCmp (x, cmpList, xVal, mateIn)


-- TODO: remove this
kingCaptureRisk :: (Ord a, Show a, ZipTreeNode a) => a -> Bool
kingCaptureRisk _n = False

maxLoop :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
maxLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec _lvl = (tcCurrentBest, tcAltsAcc, ec)
maxLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta enablePruning ec lvl =
    let (tcPossibleBest, _, ec')
            = alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta Neg enablePruning ec lvl
        (tcNewBest, ec'') = (maxTC tcCurrentBest tcPossibleBest, ec' + 1)
        zTC@(TraceCmp(_, zs, zVal, _)) = tcNewBest
        newAlphaBeta =
          if not cmpTracing
            then updateAlphaBeta Pos alphaBeta zVal zTC lvl enablePruning
            else case tcCurrentBest of
              Min -> updateAlphaBeta Pos alphaBeta zVal zTC lvl enablePruning
              _   ->
                  let TraceCmp (cb, _cbs, _tcCurVal, _) = tcCurrentBest
                      TraceCmp(pb, _, _, _) = tcPossibleBest
                      newWinner = maxTC tcCurrentBest tcPossibleBest == tcPossibleBest
                      (currentStar, newStar) =
                          if newWinner then ("     ", "*+*+*") else ("+++++", "     ")
                      s = "\n"
                          ++ printf "%s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
                          ++ printf "\n %s tcPossibleBest = %s" newStar (show tcPossibleBest)
                          ++ printf "\n |moves| = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
                              (length zs) (nodeHash cb) (nodeHash pb)
                      !temp = updateAlphaBeta Pos alphaBeta zVal zTC lvl enablePruning
                      showIt = isInfixOf moveTraceStr (pack (show tcCurrentBest))
                              || isInfixOf moveTraceStr (pack (show tcPossibleBest))
                      -- debugHash = -3107579353474362187
                      -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
                  in if showIt then trace s temp
                    else temp
        (bCanPrune, pruneInfo) = canPrune newAlphaBeta zTC enablePruning "maxLoop"
    in if bCanPrune
      then if pruneTracing
        then let TraceCmp(_, _z: sharedCmpList, _, _) = tcNewBest
             in trace (tracePruned ts sharedCmpList "maxLoop" lvl pruneInfo)
                      (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
        else (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
      else maxLoop ts cmpList tcNewBest (tcPossibleBest : tcAltsAcc) newAlphaBeta enablePruning ec'' lvl

minLoop :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool -> Int -> Int
         -> (TraceCmp a, [TraceCmp a], Int)
minLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec _lvl = (tcCurrentBest, tcAltsAcc, ec)
minLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta enablePruning ec lvl =
    let (tcPossibleBest, _, ec')
             = alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta Pos enablePruning ec lvl
        (tcNewBest, ec'') = (minTC tcCurrentBest tcPossibleBest, ec' + 1)
        zTC@(TraceCmp(_, zs, zVal, _)) = tcNewBest
        newAlphaBeta =
          if not cmpTracing
            then updateAlphaBeta Neg alphaBeta zVal zTC lvl enablePruning
            else case tcCurrentBest of
              Max -> updateAlphaBeta Neg alphaBeta zVal zTC lvl enablePruning
              _   ->
                  let TraceCmp (cb, _cbs, _tcCurVal, _) = tcCurrentBest
                      TraceCmp(pb, _, _, _) = tcPossibleBest
                      newWinner = minTC tcCurrentBest tcPossibleBest == tcPossibleBest
                      (currentStar, newStar) =
                          if newWinner then ("     ", "*-*-*") else ("-----", "     ")
                      s = "\n"
                          ++ printf "%s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
                          ++ printf "%s tcPossibleBest = %s" newStar (show tcPossibleBest)
                          ++ printf "|moves| = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
                              (length zs) (nodeHash cb) (nodeHash pb)
                      !temp = updateAlphaBeta Neg alphaBeta zVal zTC lvl enablePruning
                      showIt = isInfixOf moveTraceStr (pack (show tcCurrentBest))
                              || isInfixOf moveTraceStr (pack (show tcPossibleBest))
                      -- debugHash = -3107579353474362187
                      -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
                  in if showIt then trace s temp
                    else temp
        (bCanPrune, pruneInfo) = canPrune newAlphaBeta zTC enablePruning "minLoop"
    in if bCanPrune
         then if pruneTracing
           then let TraceCmp(_, _z: sharedCmpList, _, _) = tcNewBest
                in trace (tracePruned ts sharedCmpList "minLoop" lvl pruneInfo)
                         (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
           else (tcNewBest, tcPossibleBest : tcAltsAcc, ec'')
         else minLoop ts cmpList tcNewBest (tcPossibleBest : tcAltsAcc) newAlphaBeta enablePruning ec'' lvl

tracePruned :: forall a. (Show a, ZipTreeNode a, Ord a)
            => [T.Tree a] -> [a] -> String -> Int -> String -> String
tracePruned tsPruned sharedCmpList srcStr lvl moreInfo =
    let prunedAsTCs = fmap (\t -> tcFromT t (T.rootLabel t:sharedCmpList) lvl) tsPruned
        tsFiltered = filter (\tc -> moveTraceStr `isInfixOf` pack (show tc)) prunedAsTCs
        movesInContext = fmap show tsFiltered
        suffix = if null tsFiltered
            then "None."
            else "\nWith context: " ++ moreInfo ++ "(" ++ show (length tsPruned) ++ ")" ++ "\n"
          ++ "(p): " ++ intercalate "\n(p): " movesInContext
    in  "Pruned during " ++ srcStr ++ ": " ++ suffix

negaRnd :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
        => T.Tree a -> g -> Float -> Bool -> NegaResult a
negaRnd t gen percentVar enablePruning =
    let sign = ztnSign (T.rootLabel t)
        alphaBeta = initAlphaBeta
        (theBest, traceCmps, ec) = alphaBetaPrune t [] alphaBeta sign enablePruning 0 0
        noDup = List.delete theBest traceCmps
        close = filter (\x -> isWithin x theBest percentVar) noDup
        pickedTC@(TraceCmp (picked, pickedPath, pickedVal, pickedMateIn)) = pickOne gen (theBest : close)
        notPicked = List.delete pickedTC close
        revNotPicked = revTraceCmp <$> notPicked
    in NegaResult { best = toNegaMoves (TraceCmp (picked, reverse pickedPath, pickedVal, pickedMateIn))
                  , alternatives = toNegaMoves <$> revNotPicked
                  , allMoves = toNegaMoves . revTraceCmp <$> traceCmps
                  , evalCount = ec}

pickOne :: RandomGen g => g -> [TraceCmp a] -> TraceCmp a
pickOne gen choices =
    let (r, _g) = randomR (0, length choices - 1) gen
    in choices !! r

isWithin :: (Show a, ZipTreeNode a) => TraceCmp a -> TraceCmp a -> Float -> Bool
isWithin (TraceCmp (_, _, _, MateIn (Just _))) (TraceCmp (_, _, _, MateIn (Nothing))) _percentVar = False
isWithin (TraceCmp (_, _, _, MateIn Nothing)) (TraceCmp (_, _, _, MateIn (Just _))) _percentVar = False
isWithin (TraceCmp (_, _, _, MateIn (Just bstMateIn))) (TraceCmp (_, _, _, MateIn (Just possMateIn ))) _percentVar =
    possMateIn == bstMateIn
isWithin pos@(TraceCmp (_, _, bst, MateIn Nothing)) (TraceCmp (_, _, possible, MateIn Nothing)) percentVar =
    if signum bst /= signum possible then False
    else
      let ratio = possible / bst
          str = printf "possible alt: %s (possible: %f, best: %f, ratio: %f)"
                       (showMoveSeq pos) possible bst ratio
          !res = (ratio <= (1 + percentVar)) && (ratio >= (1 - percentVar))
      in if res then trace str res
         else res
isWithin _ _ _ = error "'Min' or 'Max' passed to isWithin?"

showTC :: (Show a) => TraceCmp a -> String
showTC Max = "<Max>"
showTC Min = "<Min>"
showTC tc = showFilteredTC tc Nothing

showFilteredTC :: (Show a) => TraceCmp a -> Maybe String -> String
showFilteredTC Max (Just _s) = ""
showFilteredTC Min (Just _s) = ""
showFilteredTC Max Nothing = "Max"
showFilteredTC Min Nothing = "Min"
showFilteredTC (TraceCmp (x, xs, v, mateIn)) filterStr =
    let showIt = case filterStr of
          Just str -> isInfixOf (pack str) (pack (show x))
          Nothing -> True
    in if not showIt then "" else
        let rev = reverse xs
            midStr =
              if length xs == 1 then "."
              else "[ " ++ List.intercalate ", " (fmap show rev) ++ " ]"
            mateStr = case mateIn of
              MateIn Nothing -> ""
              MateIn (Just n) -> "(mate in " ++ show n ++ " moves)"
        in "leaf: " ++ show x ++ " | "
            ++ "head: " ++ show (head rev)  ++ " | "
            ++ "xs: " ++ midStr ++ " | "
            ++ "v: " ++ show v
            ++ mateStr

showMoveSeq :: (Show a) => TraceCmp a -> String
showMoveSeq Max = "<Max>"
showMoveSeq Min = "<Min>"
showMoveSeq (TraceCmp (_x, xs, _v, _)) =
    let strs = fmap show (reverse xs)
        str = List.intercalate ", " strs
    in "[ " ++ str ++ " ]"

--------------------------------------------------------------------------------
--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
--------------------------------------------------------------------------------
treeSize :: T.Tree t -> (Int, [Int])
treeSize t =
    let levelTotals = fmap length (T.levels t)
    in (sum levelTotals, levelTotals)
