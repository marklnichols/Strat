{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternGuards #-}
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
  -- , sortFromResult
  , treeSize
  , ZipReaderT
  , ZipTreeEnv(..)
  , ZipTreeNode(..)
  ) where

import Control.Monad.Reader
import Data.Hashable
import qualified Data.List as List
import Data.Text (Text, pack, isInfixOf)
import qualified Data.Tree as T
import Data.Tree.Zipper
import GHC.Generics
import Text.Printf
import Debug.Trace
import System.Random

data CompareMode = Minimizing | Maximizing

newtype  ZipTree a = ZipTree {unZipTree :: T.Tree a}
  deriving Eq

data ZipTreeEnv = ZipTreeEnv
  { verbose :: Bool
  , enablePruneTracing :: Bool
  , enableCmpTracing :: Bool
  , enableRandom :: Bool
  , maxRandomChange :: Float
  , enablePreSort :: Bool
  , moveTraceStr :: Text
  , maxDepth :: Int
  , maxCritDepth :: Int
  , aiPlaysWhite :: Bool
  , aiPlaysBlack :: Bool
  }
    deriving (Eq, Show)

type ZipReaderT a = ReaderT ZipTreeEnv a

data Sign = Pos | Neg
  deriving (Eq, Generic, Hashable, Ord, Show)

data AlphaBeta = AlphaBeta
  { alpha :: Float
  , beta :: Float }
  deriving Show

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

data TraceCmp a where
  Max :: TraceCmp a
  Min :: TraceCmp a
  TraceCmp ::
    { node :: a
    , movePath :: [a]
    , value :: Float
    , mateIn :: MateIn
    , alts :: [TraceCmp a]
    } -> TraceCmp a

instance (Show a) => Show (TraceCmp a) where
  show x = showCompactTC x

instance Eq a => Eq (TraceCmp a) where
   (==) Max Max = True
   (==) Min Min = True
   (==) Max _ = False
   (==) Min _ = False
   (==) _ Max = False
   (==) _ Min = False
   (==) TraceCmp {value = x, mateIn = MateIn Nothing} TraceCmp {value = y, mateIn = MateIn Nothing} = x == y
   (==) TraceCmp {} TraceCmp {mateIn = MateIn Nothing} = False
   (==) TraceCmp {mateIn = MateIn Nothing} TraceCmp {} = False
   (==) TraceCmp {mateIn = MateIn (Just (xMateIn, _xSgn))}
        TraceCmp {mateIn = MateIn (Just (yMateIn, _ySgn))}
        = xMateIn == yMateIn

nodeHash :: (ZipTreeNode a, Hashable a) => a -> Int
nodeHash n = hash n


-- TODO: move this back to Ord instance again
-- special comparison function for TraceCmp
cmpTC :: (ZipTreeNode a, Hashable a) => TraceCmp a -> TraceCmp a -> Bool
cmpTC Max Max = True
cmpTC Min Min = True
cmpTC Max _   = False
cmpTC Min _   = True
cmpTC _ Max   = True
cmpTC _ Min   = False
cmpTC TraceCmp {value = x, mateIn = (MateIn Nothing)}
      TraceCmp {value = y, mateIn = MateIn Nothing}
    = x <= y
cmpTC TraceCmp {value = x, mateIn = (MateIn (Just pairX))}
      TraceCmp {value = y, mateIn = MateIn (Just pairY)} =
    if pairX == pairY
      then x <= y
      else mateInCompare (MateIn (Just pairX)) (MateIn (Just pairY))
cmpTC TraceCmp {mateIn = maybeX} TraceCmp { mateIn = maybeY} =
     mateInCompare maybeX maybeY

maxTC :: (Eq a, ZipTreeNode a, Hashable a) => TraceCmp a -> TraceCmp a -> TraceCmp a
maxTC x y
  | x == y    = x
  | cmpTC x y = y
  | otherwise = x

minTC :: (Eq a, ZipTreeNode a, Hashable a) => TraceCmp a -> TraceCmp a -> TraceCmp a
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
revTraceCmp tc@TraceCmp{..} = tc {movePath = reverse movePath}

initAlphaBeta :: AlphaBeta
initAlphaBeta = AlphaBeta
  { alpha = minValueTemp
  , beta = maxValueTemp }

class ZipTreeNode a where
  ztnEvaluate :: a -> Float
  ztnMakeChildren :: a -> [T.Tree a]
  ztnSign :: a -> Sign
  ztnFinal :: a -> Bool
  ztnDeepDescend :: a -> Bool
  ztnDeepDescend _ = False

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

-- START HERE, evalNode is not right on alt selection
toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves TraceCmp{..} =
    let MateIn mi = mateIn
    in NegaMoves { evalScore = value
              , mateIn = mi
              , evalNode = node
              , moveNode = head movePath -- never empty
              , moveSeq = movePath }
toNegaMoves Min = error "received 'Min' in toNegaMoves?"
toNegaMoves Max = error "received 'Max' in toNegaMoves?"

data NegaResult a = NegaResult
  { picked :: NegaMoves a
  , bestScore :: NegaMoves a
  , alternatives :: [NegaMoves a]
  , evalCount :: Int }

expandTo :: forall a m. (Ord a, Show a, ZipTreeNode a, Monad m)
         => T.Tree a
         -> Int
         -> Int
         -> ZipReaderT m (T.Tree a)
expandTo t depth critDepth =
  decendUntil (fromTree t) 1 depth critDepth

-- sortFromResult :: forall a. (Ord a, Show a, ZipTreeNode a)
--          => T.Tree a
--          -> NegaResult a
--          -> T.Tree a
-- sortFromResult treeRoot prevResult =
--     let tpos = fromTree treeRoot
--         theChildren = T.subForest treeRoot
--         moves = allMoves prevResult
--         pairs = foldr f [] moves where
--             f nmv acc =
--                 case List.find (\t -> T.rootLabel t == moveNode nmv ) theChildren of
--                     Nothing -> error $ "sorting error: couldn't find: " ++ show (moveNode nmv)
--                     Just tMatch -> (tMatch, evalScore nmv) : acc
--         sorted = sortOn (\(_, x) -> -x) pairs
--         sortedChildren = fst <$> sorted
--     in toTree $ modifyTree (\(T.Node x _) -> T.Node x sortedChildren) tpos

decendUntil :: (Ord a, Show a, ZipTreeNode a, Monad m)
            => TreePos Full a
            -> Int
            -> Int
            -> Int
            -> ZipReaderT m (T.Tree a)
decendUntil z curDepth goalDepth critDepth
    -- not past the goal depth
    | curDepth <= goalDepth = do
        !theChildren <- buildChildren z curDepth goalDepth critDepth
        return $ toTree $ modifyTree (\(T.Node x _) -> T.Node x theChildren) z
    -- past the goal depth and the parent isn't a crit -- stop
    | curDepth > goalDepth
    , ztnDeepDescend (label z) == False = return $ toTree z
    -- past the goal depth and the crit depth -- stop
    | curDepth > goalDepth
    , curDepth > critDepth = return $ toTree z
    -- crits only
    | otherwise = do -- crits only...
        unfiltered <- buildChildren z curDepth goalDepth critDepth
        let !theChildren = filterDeepDecentChildren unfiltered
        return (toTree $ modifyTree (\(T.Node x _) -> T.Node x theChildren) z)

filterDeepDecentChildren :: ZipTreeNode a => [T.Tree a] -> [T.Tree a]
filterDeepDecentChildren xs = filter (\t -> ztnDeepDescend (T.rootLabel t)) xs

buildChildren :: forall a m. (Ord a, Show a, ZipTreeNode a, Monad m)
              => TreePos Full a
              -> Int
              -> Int
              -> Int
              -> ZipReaderT m [T.Tree a]
buildChildren z curDepth goalDepth critDepth = do
    let tempLabel = label z
    let tempForest = T.subForest $ toTree z
    theChildren <-
        -- if only crits, assume non-crits were filtered out -- rebuild the list of moves
        if null tempForest || critsOnly tempForest
          then return $ ztnMakeChildren tempLabel
          else return tempForest
    (results, _) <- zipFoldR (zipFoldFn curDepth goalDepth critDepth)
                    ([], children z) theChildren
    return results

critsOnly :: ZipTreeNode a => [T.Tree a] -> Bool
critsOnly trees = all (\t -> ztnDeepDescend (T.rootLabel t)) trees

zipFoldFn :: (Ord a, Show a, ZipTreeNode a, Monad m)
  => Int -> Int -> Int
  -> T.Tree a
  -> ([T.Tree a], TreePos Empty a)
  -> ZipReaderT m ([T.Tree a], TreePos Empty a)
zipFoldFn curDepth goalDepth critDepth t (xs, childPos) = do
    let zippedChild = fromTree t
    newT <- decendUntil zippedChild (curDepth + 1) goalDepth critDepth
    let tmp = insert newT childPos
    let nextChildPos = nextSpace tmp
    return (newT : xs, nextChildPos)

zipFoldR :: forall a m. (Show a, Monad m)
         => ( T.Tree a
              -> ([T.Tree a], TreePos Empty a)
              -> ZipReaderT m ([T.Tree a], TreePos Empty a) )
         -> ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ZipReaderT m ([T.Tree a], TreePos Empty a)
zipFoldR f = loop
  where
    loop :: ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ZipReaderT m ([T.Tree a], TreePos Empty a)
    loop (acc, z ) [] = return (acc, z)
    loop (acc, z) (x : xs) = do
        (ys, z') <- f x (acc, z)
        loop (ys, z') xs

-- alpha-beta comparison
-- A return value of (True, _) means the rest of the tree can be pruned
-- The string in the 2nd tuple component provides debug info
canPrune :: (Show a, Monad m) => AlphaBeta -> Bool -> TraceCmp a -> String -> ZipReaderT m (Bool, String)
canPrune AlphaBeta{..} bPruning tcNewBest moreInfo = do
    env <- ask
    let pruneTracing = enablePruneTracing env
    if not pruneTracing
      then return (bPruning && alpha >= beta, "")
      else do
        let str = printf "pruning during %s, alpha (%f) >= beta (%f) \n \
                         \ (Because of %s)" moreInfo alpha beta (showTC tcNewBest)
        return (bPruning && alpha >= beta, str)

updateAlphaBeta :: (Show a, Monad m) => Sign -> AlphaBeta -> Bool -> Float -> TraceCmp a -> Int -> ZipReaderT m AlphaBeta
updateAlphaBeta Pos alpBet bPruning newAlpha tc lvl = do
    env <- ask
    if not bPruning then return alpBet
    else do
        let oldAlpha = alpha alpBet
        let maybeUpdated = max oldAlpha newAlpha
        if maybeUpdated == oldAlpha
          then return alpBet
          else do
            let pruneTracing = enablePruneTracing env
            if pruneTracing
              then do
                let str = printf "updating alpha (level %d) from %f to %f \ndue to: %s"
                                 lvl oldAlpha newAlpha (show tc)
                let !temp = alpBet {alpha = maybeUpdated }
                let showIt = isInfixOf (moveTraceStr env) (pack (show tc))
                return $ if showIt
                  then trace str temp
                  else temp
              else return $ alpBet {alpha = maybeUpdated }
updateAlphaBeta Neg alpBet bPruning newBeta tc lvl = do
    env <- ask
    if not bPruning then return alpBet
    else do
      let oldBeta = beta alpBet
      let maybeUpdated = min oldBeta newBeta
      if maybeUpdated == oldBeta
        then return alpBet
        else do
          let pruneTracing = enablePruneTracing env
          if pruneTracing
            then do
              let str = printf "updating beta (level %d) from %f to %f \ndue to: %s"
                               lvl oldBeta newBeta (show tc)
              let !temp = alpBet {beta = maybeUpdated }
              let showIt = isInfixOf (moveTraceStr env) (pack (show tc))
              return $ if showIt
                then trace str temp
                else temp
            else return alpBet {beta = maybeUpdated }

negaMax :: (Ord a, Show a, ZipTreeNode a, Hashable a, Monad m)
        => T.Tree a -> Bool -> ZipReaderT m (NegaResult a)
negaMax t bPruning = do
  let sign = ztnSign $ T.rootLabel t

  let alphaBeta = initAlphaBeta
  (theBest, ec) <-  alphaBetaPrune t [] alphaBeta bPruning sign 0 0

  return $ NegaResult { picked = toNegaMoves (revTraceCmp theBest)
                      , bestScore = toNegaMoves (revTraceCmp theBest)
                      , alternatives = []
                      , evalCount = ec }

alphaBetaPrune :: forall a m. (Ord a, Show a, ZipTreeNode a, Hashable a, Monad m)
         => T.Tree a -> [a] -> AlphaBeta -> Bool -> Sign -> Int -> Int
         -> ZipReaderT m (TraceCmp a, Int)
alphaBetaPrune t cmpList alphaBeta bPruning sign ec lvl = do
    let x = T.rootLabel t
    let xs = reverse $ T.subForest t
    if null xs || kingCaptureRisk x then do
       let tc = tcFromT t cmpList lvl
       return (tc, ec)
    else
      case sign of
          Pos -> maxLoop xs cmpList Min [] alphaBeta bPruning ec (lvl + 1)
          Neg -> minLoop xs cmpList Max [] alphaBeta bPruning ec (lvl + 1)

tcFromT :: forall a. (Ord a, Show a, ZipTreeNode a)
         => T.Tree a -> [a] -> Int -> TraceCmp a
tcFromT t movePath lvl =
    let node = T.rootLabel t
        value = ztnEvaluate node
        mateIn = MateIn
          (if value >= maxScore
             then Just (lvl, Pos)
             else if value <= minScore
               then Just (lvl, Neg)
               else Nothing)
        in
          TraceCmp {node, movePath, value, mateIn, alts = []}

-- TODO: remove this
kingCaptureRisk :: (Ord a, Show a, ZipTreeNode a) => a -> Bool
kingCaptureRisk _n = False

maxLoop :: forall a m. (Ord a, Show a, ZipTreeNode a, Hashable a, Monad m)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool
         -> Int -> Int -> ZipReaderT m (TraceCmp a, Int)
maxLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec lvl = do
    let altsToSave = if lvl==1 then tcAltsAcc else []
    return (tcCurrentBest{alts = altsToSave}, ec)
maxLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta bPruning ec lvl = do
    -- env <- ask
    (tcPossibleBest, ec') <- alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta bPruning Neg ec lvl
    let (tcNewBest, ec'') = (maxTC tcCurrentBest tcPossibleBest, ec' + 1)
    let newWinner = tcNewBest == tcPossibleBest
    let zTC@TraceCmp {value = zVal} = tcNewBest
    -- let cmpTracing = enableCmpTracing env
    -- let pruneTracing = enablePruneTracing env
    newAlphaBeta <-
        updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
        -- if not cmpTracing
        --   then updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
        --   else case tcCurrentBest of
        --     Min -> updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
        --     _   -> do
        --         let TraceCmp {node = cb}  = tcCurrentBest
        --         let TraceCmp {node = pb} = tcPossibleBest
        --         let (currentStar, newStar) =
        --               if newWinner then ("     ", "*+*+*") else ("+++++", "     ")
        --         let s = "\n"
        --               ++ printf " %s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
        --               ++ printf "\n %s tcPossibleBest = %s" newStar (show tcPossibleBest)
        --               ++ printf "\n Level = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
        --                  lvl (nodeHash cb) (nodeHash pb)
        --         let !temp = updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
        --         let showIt = isInfixOf (moveTraceStr env) (pack (show tcCurrentBest))
        --                       || isInfixOf (moveTraceStr env) (pack (show tcPossibleBest))
        --             -- debugHash = -3107579353474362187
        --             -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
        --         if showIt then trace s temp
        --         else temp
    let altsToSave = if lvl == 1 then tcAltsAcc else []
    (bCanPrune, _pruneInfo) <- canPrune newAlphaBeta bPruning zTC "maxLoop"
    if bCanPrune
      then
        return (tcNewBest{alts = altsToSave}, ec'')
        -- if pruneTracing
        --   then do
        --     let TraceCmp {movePath = sharedCmpList} = tcNewBest
        --     strM <- tracePruned ts sharedCmpList "maxLoop" lvl pruneInfo
        --     case strM of
        --         Nothing -> return (tcNewBest{alts = altsToSave}, ec'')
        --         Just str -> return $ trace str (tcNewBest{alts = altsToSave}, ec'')
        --   else return (tcNewBest{alts = altsToSave}, ec'')
      else
        let newAlts = if newWinner then tcPossibleBest : tcAltsAcc else tcAltsAcc
        in maxLoop ts cmpList tcNewBest newAlts newAlphaBeta bPruning ec'' lvl

minLoop :: forall a m. (Ord a, Show a, ZipTreeNode a, Hashable a, Monad m)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool
            -> Int -> Int -> ZipReaderT m (TraceCmp a, Int)
minLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec lvl = do
    let altsToSave = if lvl==1 then tcAltsAcc else []
    return (tcCurrentBest{alts = altsToSave}, ec)
minLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta bPruning ec lvl = do
    -- env <- ask
    (tcPossibleBest, ec') <- alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta bPruning Pos  ec lvl
    let (tcNewBest, ec'') = (minTC tcCurrentBest tcPossibleBest, ec' + 1)
    let newWinner = tcNewBest == tcPossibleBest
    let zTC@TraceCmp{value = zVal} = tcNewBest
    -- let cmpTracing = enableCmpTracing env
    -- let pruneTracing = enablePruneTracing env
    newAlphaBeta <-
        updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
        -- if not cmpTracing
        --   then updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
        --   else case tcCurrentBest of
        --     Max -> updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
        --     _   -> do
        --       let TraceCmp {node = cb} = tcCurrentBest
        --       let TraceCmp {node = pb} = tcPossibleBest
        --       let (currentStar, newStar) =
        --             if newWinner then ("     ", "*-*-*") else ("-----", "     ")
        --       let s = "\n"
        --               ++ printf " %s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
        --               ++ printf "\n %s tcPossibleBest = %s" newStar (show tcPossibleBest)
        --               ++ printf "\n Level = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
        --                   lvl (nodeHash cb) (nodeHash pb)
        --       let !temp = updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
        --       let showIt = isInfixOf (moveTraceStr env) (pack (show tcCurrentBest))
        --                     || isInfixOf (moveTraceStr env) (pack (show tcPossibleBest))
        --             -- debugHash = -3107579353474362187
        --             -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
        --       if showIt then trace s temp
        --       else temp
    let altsToSave = if lvl == 1 then tcAltsAcc else []
    (bCanPrune, _pruneInfo) <- canPrune newAlphaBeta bPruning zTC "minLoop"
    if bCanPrune
      then
        return (tcNewBest {alts = altsToSave}, ec'')
        -- if pruneTracing
        --   then do
        --     let TraceCmp {movePath = sharedCmpList} = tcNewBest
        --     strM <- tracePruned ts sharedCmpList "minLoop" lvl pruneInfo
        --     case strM of
        --         Nothing -> return (tcNewBest {alts = altsToSave}, ec'')
        --         Just str -> return $ trace str (tcNewBest {alts = altsToSave}, ec'')
        --   else return (tcNewBest {alts = altsToSave}, ec'')
      else
        let newAlts = if newWinner then tcPossibleBest : tcAltsAcc else tcAltsAcc
        in minLoop ts cmpList tcNewBest newAlts newAlphaBeta bPruning ec'' lvl

-- tracePruned :: forall a m. (Show a, ZipTreeNode a, Ord a, Monad m)
--             => [T.Tree a] -> [a] -> String -> Int -> String -> ZipReaderT m (Maybe String)
-- tracePruned tsPruned sharedCmpList srcStr lvl moreInfo = do
--     env <- ask
--     let prunedAsTCs = fmap (\t -> tcFromT t (T.rootLabel t:sharedCmpList) lvl) tsPruned
--     let tsFiltered = filter (\tc -> (moveTraceStr env) `isInfixOf` pack (show tc)) prunedAsTCs
--     let movesInContext = fmap show tsFiltered
--     if null tsFiltered
--       then return Nothing
--       else return $ Just $ "Pruned during " ++ srcStr ++ ": " ++
--         "\nWith context: " ++ moreInfo ++ "(" ++ show (length tsPruned) ++ ")" ++ "\n"
--          ++ "(p): " ++ intercalate "\n(p): " movesInContext

negaRnd :: (Ord a, Show a, ZipTreeNode a, Hashable a, Monad m, RandomGen g)
        => T.Tree a -> g -> Float -> Bool -> ZipReaderT m (NegaResult a)
negaRnd t gen maxRndChange bPruning = do
    let sign = ztnSign (T.rootLabel t)
    let alphaBeta = initAlphaBeta
    (theBestTC, ec) <- alphaBetaPrune t [] alphaBeta bPruning sign 0 0
    let choices =
          let !tmp = theBestTC : (filter (\tc -> isWithin theBestTC tc sign maxRndChange) (alts theBestTC))
              str = printf "Number of choices: %d" (length tmp)
          in trace str tmp
    let pickedTC = pickOne gen choices
    let notPicked = List.delete pickedTC choices
    let revNotPicked = revTraceCmp <$> notPicked
    let reverseBest = theBestTC{movePath = reverse (movePath theBestTC)}
    let reversePicked = pickedTC{movePath = reverse (movePath pickedTC)}
    return NegaResult { picked = toNegaMoves reversePicked
                      , bestScore = toNegaMoves reverseBest
                      , alternatives = toNegaMoves <$> revNotPicked
                      , evalCount = ec}

pickOne :: RandomGen g => g -> [TraceCmp a] -> TraceCmp a
pickOne gen choices =
    let (r, _g) = randomR (0, length choices - 1) gen
    in choices !! r

isWithin :: (Show a, ZipTreeNode a) => TraceCmp a -> TraceCmp a -> Sign -> Float -> Bool
isWithin TraceCmp {mateIn = MateIn (Just _)} TraceCmp {mateIn = MateIn Nothing} _sign _maxRandChg = False
isWithin TraceCmp{mateIn = MateIn Nothing} TraceCmp {mateIn = MateIn (Just _)} _sign _maxRandChg = False
isWithin TraceCmp {mateIn = (MateIn (Just bstMateIn))}
         TraceCmp {mateIn = (MateIn (Just possMateIn ))} _sign _maxRandomChg =
    possMateIn == bstMateIn
isWithin TraceCmp {value = bst, mateIn = (MateIn Nothing)}
         TraceCmp {value = possible, mateIn = (MateIn Nothing)}
         sign maxRandomChg =
    case sign of
      Pos ->
        bst - maxRandomChg <= possible
      Neg ->
        bst + maxRandomChg >= possible
isWithin _ _ _ _ = error "'Min' or 'Max' passed to isWithin?"

showTC :: (Show a) => TraceCmp a -> String
showTC Max = "<Max>"
showTC Min = "<Min>"
showTC tc = showFilteredTC tc Nothing

showFilteredTC :: (Show a) => TraceCmp a -> Maybe String -> String
showFilteredTC Max (Just _s) = ""
showFilteredTC Min (Just _s) = ""
showFilteredTC Max Nothing = "Max"
showFilteredTC Min Nothing = "Min"
showFilteredTC TraceCmp {..} filterStr =
    let showIt = case filterStr of
          Just str -> isInfixOf (pack str) (pack (show node))
          Nothing -> True
    in if not showIt then "" else
        let rev = reverse movePath
            midStr =
              if length movePath == 1 then "."
              else "[ " ++ List.intercalate ", " (fmap show rev) ++ " ]"
            mateStr = case mateIn of
              MateIn Nothing -> ""
              MateIn (Just n) -> "(mate in " ++ show n ++ " moves)"
        in "leaf: " ++ show node ++ " | "
            ++ "head: " ++ show (head rev)  ++ " | "
            ++ "movePath: " ++ midStr ++ " | "
            ++ "value: " ++ show value
            ++ mateStr

showMoveSeq :: (Show a) => TraceCmp a -> String
showMoveSeq Max = "<Max>"
showMoveSeq Min = "<Min>"
showMoveSeq TraceCmp {..} =
    let strs = fmap show (reverse movePath)
        str = List.intercalate ", " strs
    in "[ " ++ str ++ " ]"

showCompactTC :: (Show a) => TraceCmp a -> String
showCompactTC Max = "<Max>"
showCompactTC Min = "<Min>"
showCompactTC TraceCmp {..} =
    let rev = reverse movePath
        midStr =
          if length movePath == 1 then "."
          else "[" ++ List.intercalate ", " (fmap show rev) ++ "]"
        mateStr = case mateIn of
          MateIn Nothing -> ""
          MateIn (Just n) -> "(mate in " ++ show n ++ ")"
    in midStr ++ " | "
        ++ "value: " ++ show value
        ++ mateStr

--------------------------------------------------------------------------------
--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
--------------------------------------------------------------------------------
treeSize :: T.Tree t -> (Int, [Int])
treeSize t =
    let levelTotals = fmap length (T.levels t)
    in (sum levelTotals, levelTotals)
