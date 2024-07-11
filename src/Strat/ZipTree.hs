{-# language GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Strat.ZipTree
  ( expandTo
  , decendUntil
  , CompareMode(..) -- exported for testing
  , HasZipTreeEnv (..)
  , isWithin
  , MateIn(..) -- exported for testing
  , mateInCompare -- exported for testing
  , maxScore
  , maxTC
  , maxValueTemp
  , minScore
  , minTC
  , minValueTemp
  , negaMax
  , negaWorker
  , nodeHash
  , revTraceCmp
  , NegaResult(..)
  , NegaMoves(..)
  , pickOne
  , PositionState(..)
  , showCompactTCList
  , Sign(..)
  , TraceCmp(..)
  , showMoveSeq
  , showTC
  , toNegaMoves
  , treeSize
  , ZipTreeM
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

class PositionState p where
    toString :: p -> String
    combineTwo :: p -> p -> p

    combine :: [p] -> p
    combine [x] = x
    combine (x:xs) = List.foldl' combineTwo x xs

data ZipTreeEnv = ZipTreeEnv
  { verbose :: Bool
  , enablePruneTracing :: Bool
  , enableCmpTracing :: Bool
  , enableRandom :: Bool
  , maxRandomChange :: Float
  , enablePruning :: Bool
  , singleThreaded :: Bool
  , enablePreSort :: Bool
  , moveTraceStr :: Text
  , maxDepth :: Int
  , maxCritDepth :: Int
  , aiPlaysWhite :: Bool
  , aiPlaysBlack :: Bool
  }

class HasZipTreeEnv a where
  zte :: a -> ZipTreeEnv

instance HasZipTreeEnv ZipTreeEnv
  where zte = id

type ZipTreeM r a = (HasZipTreeEnv r) => ReaderT r IO a

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

newtype MateIn = MateIn (Maybe (Int, Sign))
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
  show = showCompactTC

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

instance (Eq a, ZipTreeNode a, Hashable a) => Ord (TraceCmp a) where
  (<=) = cmpTC

-- Similar to TraceCmp, but with move list reversed so that earlier moves preceed later ones
data NegaMoves a where
  NegaMoves ::
    { nmNode :: a
    , nmMovePath :: [a]
    , nmValue :: Float
    , nmMateIn :: MateIn
    , nmAlts :: [TraceCmp a]
    } -> NegaMoves a
instance (Show a) => Show (NegaMoves a) where
  show = showNegaMoves

toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves Min = error "received 'Min' in toNegaMoves?"
toNegaMoves Max = error "received 'Max' in toNegaMoves?"
toNegaMoves tOrig =
    let t = revTraceCmp tOrig
    in NegaMoves
        { nmNode = node t
        , nmMovePath = movePath t
        , nmValue = value t
        , nmMateIn = mateIn t
        , nmAlts = alts t }

data NegaResult a = NegaResult
  { picked :: NegaMoves a
  , bestScore :: NegaMoves a
  , alternatives :: [NegaMoves a]
  , evalCount :: Int }

-- TODO: move this back to Ord instance again
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

nodeHash :: (ZipTreeNode a, Hashable a) => a -> Int
nodeHash = hash

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

expandTo :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
         => T.Tree a
         -> Int
         -> Int
         -> Int
         -> ZipTreeM r (T.Tree a)
expandTo t curDepth depth critDepth =
    decendUntil (fromTree t) curDepth depth critDepth

decendUntil :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
            => TreePos Full a
            -> Int
            -> Int
            -> Int
            -> ZipTreeM r (T.Tree a)
decendUntil z curDepth goalDepth critDepth
    -- not past the goal depth
    | curDepth <= goalDepth = do
        !theChildren <- buildChildren z curDepth goalDepth critDepth
        return $ toTree $ modifyTree (\(T.Node x _) -> T.Node x theChildren) z
    -- past the goal depth and the parent isn't a crit -- stop
    | curDepth > goalDepth
    , not (ztnDeepDescend (label z)) = return $ toTree z

    -- past the goal depth and the crit depth -- stop
    | curDepth > goalDepth
    , curDepth > critDepth = return $ toTree z

    -- crits only
    | otherwise = do -- crits only...
        unfiltered <- buildChildren z curDepth goalDepth critDepth
        let !theChildren = filterDeepDecentChildren unfiltered
        return (toTree $ modifyTree (\(T.Node x _) -> T.Node x theChildren) z)

filterDeepDecentChildren :: ZipTreeNode a => [T.Tree a] -> [T.Tree a]
filterDeepDecentChildren = filter (ztnDeepDescend . T.rootLabel)

buildChildren :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
              => TreePos Full a
              -> Int
              -> Int
              -> Int
              -> ZipTreeM r [T.Tree a]
buildChildren z curDepth goalDepth critDepth = do
    let tempLabel = label z
    let tempForest = T.subForest $ toTree z
    theChildren <-
        -- note: rebuild the list of moves if non-crits could have been filtered
        -- out but are needed again
        if null tempForest || critsOnly tempForest
          then return $ ztnMakeChildren tempLabel
          else return tempForest
    (results, _) <- zipFoldR (zipFoldFn curDepth goalDepth critDepth)
                    ([], children z) theChildren
    return results

critsOnly :: ZipTreeNode a => [T.Tree a] -> Bool
critsOnly trees = all (ztnDeepDescend . T.rootLabel) trees

zipFoldFn :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
  => Int -> Int -> Int
  -> T.Tree a
  -> ([T.Tree a], TreePos Empty a)
  -> ZipTreeM r ([T.Tree a], TreePos Empty a)
zipFoldFn curDepth goalDepth critDepth t (xs, childPos) = do
    let zippedChild = fromTree t
    newT <- decendUntil zippedChild (curDepth + 1) goalDepth critDepth
    let tmp = insert newT childPos
    let nextChildPos = nextSpace tmp
    return (newT : xs, nextChildPos)

zipFoldR :: forall a r. (Show a, HasZipTreeEnv r)
         => ( T.Tree a
             -> ([T.Tree a], TreePos Empty a)
             -> ZipTreeM r ([T.Tree a], TreePos Empty a) )
         -> ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ZipTreeM r ([T.Tree a], TreePos Empty a)
zipFoldR f = loop
  where
    loop :: (HasZipTreeEnv r) => ([T.Tree a], TreePos Empty a)
         -> [T.Tree a]
         -> ZipTreeM r ([T.Tree a], TreePos Empty a)
    loop (acc, z ) [] = return (acc, z)
    loop (acc, z) (x : xs) = do
        (ys, z') <- f x (acc, z)
        loop (ys, z') xs

-- alpha-beta comparison
-- A return value of (True, _) means the rest of the tree can be pruned
-- The string in the 2nd tuple component provides debug info
canPrune :: (Show a, HasZipTreeEnv r) => AlphaBeta -> Bool -> TraceCmp a -> String -> ZipTreeM r (Bool, String)
canPrune AlphaBeta{..} bPruning tcNewBest moreInfo = do
    r <- ask
    let env = zte r
    let pruneTracing = enablePruneTracing env
    if not pruneTracing
      then return (bPruning && alpha >= beta, "")
      else do
        let str = printf "pruning during %s, alpha (%f) >= beta (%f) \n \
                         \ (Because of %s)" moreInfo alpha beta (showTC tcNewBest)
        return (bPruning && alpha >= beta, str)

updateAlphaBeta :: (Show a, HasZipTreeEnv r)
                => Sign -> AlphaBeta -> Bool -> Float -> TraceCmp a -> Int -> ZipTreeM r AlphaBeta
updateAlphaBeta Pos alpBet bPruning newAlpha tc lvl = do
    r <- ask
    let env = zte r
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
                let showIt = moveTraceStr env `isInfixOf` pack (show tc)
                return $ if showIt
                  then trace str temp
                  else temp
              else return $ alpBet {alpha = maybeUpdated }
updateAlphaBeta Neg alpBet bPruning newBeta tc lvl = do
    r <- ask
    let env = zte r
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
              let showIt = moveTraceStr env `isInfixOf` pack (show tc)
              return $ if showIt
                then trace str temp
                else temp
            else return alpBet {beta = maybeUpdated }

alphaBetaPrune :: (Ord a, Show a, ZipTreeNode a, Hashable a, HasZipTreeEnv r)
         => T.Tree a -> [a] -> AlphaBeta -> Bool -> Sign -> Int -> Int
         -> ZipTreeM r (TraceCmp a, Int)
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

maxLoop :: (Ord a, Show a, ZipTreeNode a, Hashable a, HasZipTreeEnv r)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool
         -> Int -> Int -> ZipTreeM r (TraceCmp a, Int)
maxLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec lvl = do
    let altsToSave = if lvl==1 then tcAltsAcc else []
    return (tcCurrentBest{alts = altsToSave}, ec)
maxLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta bPruning ec lvl = do
    r <- ask
    let env = zte r
    (tcPossibleBest, ec') <- alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta bPruning Neg ec lvl
    let (tcNewBest, ec'') = (maxTC tcCurrentBest tcPossibleBest, ec' + 1)
    --
    let newWinner = tcNewBest == tcPossibleBest
    let zTC@TraceCmp {value = zVal} = tcNewBest
    let cmpTracing = enableCmpTracing env
    let pruneTracing = enablePruneTracing env
    --
    newAlphaBeta <-
        --
        -- updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
        if not cmpTracing
          then updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
          else case tcCurrentBest of
            Min -> updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
            _   -> do
                let TraceCmp {node = cb}  = tcCurrentBest
                let TraceCmp {node = pb} = tcPossibleBest
                let (currentStar, newStar) =
                      if newWinner then ("     ", "*+*+*") else ("+++++", "     ")
                let s = "\n"
                      ++ printf " %s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
                      ++ printf "\n %s tcPossibleBest = %s" newStar (show tcPossibleBest)
                      ++ printf "\n Level = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
                         lvl (nodeHash cb) (nodeHash pb)
                let !temp = updateAlphaBeta Pos alphaBeta bPruning zVal zTC lvl
                let showIt = isInfixOf (moveTraceStr env) (pack (show tcCurrentBest))
                              || isInfixOf (moveTraceStr env) (pack (show tcPossibleBest))
                    -- debugHash = -3107579353474362187
                    -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
                if showIt then trace s temp
                else temp
        --
    let altsToSave = if lvl == 1 then tcAltsAcc else []
    (bCanPrune, pruneInfo) <- canPrune newAlphaBeta bPruning zTC "maxLoop"
    if bCanPrune
      then
        --
        -- return (tcNewBest{alts = altsToSave}, ec'')
        if pruneTracing
          then do
            let TraceCmp {movePath = sharedCmpList} = tcNewBest
            strM <- tracePruned ts sharedCmpList "maxLoop" lvl pruneInfo
            case strM of
                Nothing -> return (tcNewBest{alts = altsToSave}, ec'')
                Just str -> return $ trace str (tcNewBest{alts = altsToSave}, ec'')
          else return (tcNewBest{alts = altsToSave}, ec'')
        --
      else
        let newAlts = if newWinner then tcPossibleBest : tcAltsAcc else tcAltsAcc
        in maxLoop ts cmpList tcNewBest newAlts newAlphaBeta bPruning ec'' lvl

minLoop :: (Ord a, Show a, ZipTreeNode a, Hashable a, HasZipTreeEnv r)
         => [T.Tree a] -> [a] -> TraceCmp a -> [TraceCmp a] -> AlphaBeta -> Bool
            -> Int -> Int -> ZipTreeM r (TraceCmp a, Int)
minLoop [] _cmpList tcCurrentBest tcAltsAcc _ _ ec lvl = do
    let altsToSave = if lvl==1 then tcAltsAcc else []
    return (tcCurrentBest{alts = altsToSave}, ec)
minLoop (t:ts) cmpList tcCurrentBest tcAltsAcc alphaBeta bPruning ec lvl = do
    r <- ask
    let env = zte r
    (tcPossibleBest, ec') <- alphaBetaPrune t (T.rootLabel t : cmpList) alphaBeta bPruning Pos  ec lvl
    let (tcNewBest, ec'') = (minTC tcCurrentBest tcPossibleBest, ec' + 1)
    --
    let newWinner = tcNewBest == tcPossibleBest
    let zTC@TraceCmp{value = zVal} = tcNewBest
    let cmpTracing = enableCmpTracing env
    let pruneTracing = enablePruneTracing env
    --
    newAlphaBeta <-
        --
        -- updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
        if not cmpTracing
          then updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
          else case tcCurrentBest of
            Max -> updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
            _   -> do
              let TraceCmp {node = cb} = tcCurrentBest
              let TraceCmp {node = pb} = tcPossibleBest
              let (currentStar, newStar) =
                    if newWinner then ("     ", "*-*-*") else ("-----", "     ")
              let s = "\n"
                      ++ printf " %s tcCurrentBest  = %s" currentStar (show tcCurrentBest)
                      ++ printf "\n %s tcPossibleBest = %s" newStar (show tcPossibleBest)
                      ++ printf "\n Level = %d, #tcCurrentBest = %d, #tcPossibleBest = %d"
                          lvl (nodeHash cb) (nodeHash pb)
              let !temp = updateAlphaBeta Neg alphaBeta bPruning zVal zTC lvl
              let showIt = isInfixOf (moveTraceStr env) (pack (show tcCurrentBest))
                            || isInfixOf (moveTraceStr env) (pack (show tcPossibleBest))
                    -- debugHash = -3107579353474362187
                    -- showIt = nodeHash cb == debugHash || nodeHash pb == debugHash
              if showIt then trace s temp
              else temp
        --
    let altsToSave = if lvl == 1 then tcAltsAcc else []
    (bCanPrune, pruneInfo) <- canPrune newAlphaBeta bPruning zTC "minLoop"
    if bCanPrune
      then
        --
        -- return (tcNewBest {alts = altsToSave}, ec'')
        if pruneTracing
          then do
            let TraceCmp {movePath = sharedCmpList} = tcNewBest
            strM <- tracePruned ts sharedCmpList "minLoop" lvl pruneInfo
            case strM of
                Nothing -> return (tcNewBest {alts = altsToSave}, ec'')
                Just str -> return $ trace str (tcNewBest {alts = altsToSave}, ec'')
          else return (tcNewBest {alts = altsToSave}, ec'')
        --
      else
        let newAlts = if newWinner then tcPossibleBest : tcAltsAcc else tcAltsAcc
        in minLoop ts cmpList tcNewBest newAlts newAlphaBeta bPruning ec'' lvl

tracePruned :: (Show a, ZipTreeNode a, Ord a, HasZipTreeEnv r)
            => [T.Tree a] -> [a] -> String -> Int -> String -> ZipTreeM r (Maybe String)
tracePruned tsPruned sharedCmpList srcStr lvl moreInfo = do
    r <- ask
    let env = zte r
    let prunedAsTCs = fmap (\t -> tcFromT t (T.rootLabel t:sharedCmpList) lvl) tsPruned
    let tsFiltered = filter (\tc -> moveTraceStr env `isInfixOf` pack (show tc)) prunedAsTCs
    let movesInContext = fmap show tsFiltered
    if null tsFiltered
      then return Nothing
      else return $ Just $ "Pruned during " ++ srcStr ++ ": " ++
        "\nWith context: " ++ moreInfo ++ "(" ++ show (length tsPruned) ++ ")" ++ "\n"
         ++ "(p): " ++ List.intercalate "\n(p): " movesInContext


----------------------------------------------------------------------------------------------------
-- Single threaded worker for multithreaded evaluation
----------------------------------------------------------------------------------------------------
negaWorker :: (Ord a, Show a, ZipTreeNode a, Hashable a, HasZipTreeEnv r)
           => T.Tree a -> ZipTreeM r (TraceCmp a, Int)
negaWorker t = do
  let sign = ztnSign $ T.rootLabel t
  r <- ask
  let env = zte r
  let bPruning = enablePruning env
  let alphaBeta = initAlphaBeta
  alphaBetaPrune t [] alphaBeta bPruning sign 0 0

----------------------------------------------------------------------------------------------------
-- Entry point for evaluation when running single threaded
----------------------------------------------------------------------------------------------------
negaMax :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, HasZipTreeEnv r)
        => T.Tree a -> Maybe g -> ZipTreeM r (NegaResult a)
negaMax t gen = do
    case gen of
      Just g -> negaRnd t g
      Nothing -> negaNoRand t

----------------------------------------------------------------------------------------------------
-- Evaluation when running single threaded, no randomness
----------------------------------------------------------------------------------------------------
negaNoRand :: (Ord a, Show a, ZipTreeNode a, Hashable a, HasZipTreeEnv r)
        => T.Tree a -> ZipTreeM r (NegaResult a)
negaNoRand t  = do
  r <- ask
  let env = zte r
  let bPruning = enablePruning env
  let sign = ztnSign $ T.rootLabel t
  let alphaBeta = initAlphaBeta
  (theBest, ec) <-  alphaBetaPrune t [] alphaBeta bPruning sign 0 0
  return $ NegaResult { picked = toNegaMoves theBest
                      , bestScore = toNegaMoves theBest
                      , alternatives = []
                      , evalCount = ec }

----------------------------------------------------------------------------------------------------
-- Evaluation when running single threaded, with randomness
----------------------------------------------------------------------------------------------------
negaRnd :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, HasZipTreeEnv r)
        => T.Tree a -> g -> ZipTreeM r (NegaResult a)
negaRnd t gen = do
    r <- ask
    let env = zte r
    let bPruning = enablePruning env
    let maxRndChange = maxRandomChange env
    let sign = ztnSign $ T.rootLabel t
    let alphaBeta = initAlphaBeta
    (theBestTC, ec) <- alphaBetaPrune t [] alphaBeta bPruning sign 0 0
    let choices =
          let !tmp = theBestTC : filter (isWithin sign maxRndChange theBestTC) (alts theBestTC)
              str = printf "Number of choices: %d" (length tmp)
          in trace str tmp
    let pickedTC = pickOne gen choices
    let notPicked = List.delete pickedTC choices
    return NegaResult { picked = toNegaMoves pickedTC
                      , bestScore = toNegaMoves theBestTC
                      , alternatives = toNegaMoves <$> notPicked
                      , evalCount = ec }

pickOne :: RandomGen g => g -> [TraceCmp a] -> TraceCmp a
pickOne gen choices =
    let (r, _g) = randomR (0, length choices - 1) gen
    in choices !! r

isWithin :: (Show a, Eq a, ZipTreeNode a) => Sign -> Float -> TraceCmp a -> TraceCmp a -> Bool
isWithin _sign _maxRndChg TraceCmp {mateIn = MateIn (Just _)} TraceCmp {mateIn = MateIn Nothing}  = False
isWithin _sign _maxRandChg TraceCmp {mateIn = MateIn Nothing} TraceCmp {mateIn = MateIn (Just _)} = False
isWithin _sign _maxRandomChg
         x@(TraceCmp {mateIn = (MateIn (Just bstMateIn))})
         y@(TraceCmp {mateIn = (MateIn (Just possMateIn ))}) =
    x /= y &&
    possMateIn == bstMateIn
isWithin sign maxRandomChg
         x@TraceCmp {value = bst, mateIn = (MateIn Nothing)}
         y@TraceCmp {value = possible, mateIn = (MateIn Nothing)} =
    x /= y &&
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
          Just str -> pack str `isInfixOf` pack (show node)
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

showCompactTCList :: (Show a) => [TraceCmp a] -> String
showCompactTCList [] = ""
showCompactTCList (x:xs) = show x ++ "\n" ++ showCompactTCList xs

showNegaMoves :: (Show a) => NegaMoves a -> String
showNegaMoves NegaMoves {..} =
    let midStr =
          if length nmMovePath == 1 then "."
          else "[" ++ List.intercalate ", " (fmap show nmMovePath) ++ "]"
        mateStr = case nmMateIn of
          MateIn Nothing -> ""
          MateIn (Just n) -> "(mate in " ++ show n ++ ")"
    in midStr ++ " | "
        ++ "value: " ++ show nmValue
        ++ mateStr

--------------------------------------------------------------------------------
--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
--------------------------------------------------------------------------------
treeSize :: T.Tree t -> (Int, [Int])
treeSize t =
    let levelTotals = fmap length (T.levels t)
    in (sum levelTotals, levelTotals)
