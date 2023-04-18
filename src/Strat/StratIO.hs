{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Strat.StratIO where

import qualified Control.Concurrent.Async as Async
import Control.Exception (assert)
import Control.Monad.RWS.Lazy
import qualified Data.List as List (delete)
import Data.Hashable
import Data.Maybe
import qualified Data.Tree as T
import Data.Tree.Zipper
import Data.Tuple.Extra
import Strat.StratTree.TreeNode
import Strat.ZipTree hiding (expandTo)
import qualified Strat.ZipTree as Z
import Text.Printf
-- import Debug.Trace
import System.Random

resolveRandom :: [MoveScore m e] -> IO (Maybe (MoveScore m e))
resolveRandom [] = return Nothing
resolveRandom xs = do
    r <- getStdRandom $ randomR (1, length xs)
    return $ Just $ xs !! (r-1)

expandToParallel :: forall a p. (Ord a, Show a, ZipTreeNode a)
                 => T.Tree a -> Int -> Int -> Z.ZipTreeM p (T.Tree a)
expandToParallel t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let (_, levels) = treeSize t
    let _num_levels = assert (length levels >= 2) (length levels)
    let theChildren = T.subForest t
    liftIO $ putStrLn $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    liftIO $ putStrLn $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth

    -- a little weird: runReaderT within a ReaderT monad - to get an IO type usable with Async
    -- runRWST :: RWST r w s m a -> r -> s -> m (a, s, w)
    env <- ask
    origState <- get
    triples <- liftIO $ Async.forConcurrently theChildren
      (\x -> runRWST (Z.expandTo x 2 depth critDepth) env origState)
    let newChildren = fst3 <$> triples
    let newState = Z.combineList (snd3 <$> triples)
    case newState of
      Nothing -> error "StratIO.expandToParallel -- no state?"
      Just ns -> put ns
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z

    liftIO $ putStrLn $ printf "size of newTree: %s" (show (treeSize newTree))
    return newTree

expandToSingleThreaded :: forall a p. (Ord a, Show a, ZipTreeNode a)
                 => T.Tree a -> Int -> Int -> Z.ZipTreeM p (T.Tree a)
expandToSingleThreaded t depth critDepth =
    Z.expandTo t 1 depth critDepth

negaMaxParallel :: forall a g p. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, Z.PositionState p)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM p  (NegaResult a)
negaMaxParallel env t gen = do
    let theChildren = T.subForest t
    env <- ask
    origState <- get
    resultsList <- liftIO $ Async.forConcurrently theChildren (\x -> do

        -- TODO: use UnliftIO to remove the need to another runRWST here
        triple <- runRWST (Z.negaWorker x) env origState
        -- The TraceCmp returned from each thread needs to be re-attached to
        -- the thread's root node,  part of the move sequence:
        let (threadTC, threadEC) = fst3 triple
        let threadNode = T.rootLabel x
        let newState' = snd3 triple
        return ( ( threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }
                 , threadEC)
               , newState'))
    let newState = Z.combineList (snd <$> resultsList)
    case newState of
        Nothing -> error "StratIO.negaMaxParallel -- no state?"
        Just ns -> put ns
    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max

    let tcResults = fst <$> resultsList
    let curried = foldf sign
    let (theBestTC, ec::Int) = foldr curried (initTC, 0) tcResults
    liftIO $ putStrLn $ printf "theBest after comparing thread results: %s" (showTC theBestTC)

    -- TODO: combine this with the code in Strat.ZipTree.negaRnd
    if enableRandom env && isJust gen
      then do
        let theGen = fromJust gen
        let maxRnd = maxRandomChange env
        let curriedAltf = foldfAlts sign (maxRandomChange env) theBestTC
        let theAlts = foldr curriedAltf [] (fst <$> tcResults)

        let pickedTC = Z.pickOne theGen theAlts
        let notPicked = List.delete pickedTC theAlts
        let revNotPicked = revTraceCmp <$> notPicked
        let reverseBest = theBestTC{movePath = reverse (movePath theBestTC)}
        let reversePicked = pickedTC{movePath = reverse (movePath pickedTC)}
        return NegaResult { picked = toNegaMoves reversePicked
                          , bestScore = toNegaMoves (revTraceCmp theBestTC)
                          , alternatives = toNegaMoves <$> revNotPicked
                          , evalCount = ec }
        else
          return $ NegaResult { picked = toNegaMoves (revTraceCmp theBestTC)
                              , bestScore = toNegaMoves (revTraceCmp theBestTC)
                              , alternatives = []
                              , evalCount = ec }

    where
        foldf :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
              => Sign
              -> (TraceCmp a, Int)
              -> (TraceCmp a, Int)
              -> (TraceCmp a, Int)
        foldf Pos (tc, numEvals) (tcAcc, numEvalsAcc) =
          (maxTC tcAcc tc, numEvalsAcc + numEvals)
        foldf Neg (tc, numEvals) (tcAcc, numEvalsAcc) =
          (minTC tcAcc tc, numEvalsAcc + numEvals)

        foldfAlts :: forall a. (Ord a, Show a, ZipTreeNode a, Hashable a)
                  => Sign -> Float -> TraceCmp a -> TraceCmp a -> [(TraceCmp a)] -> [(TraceCmp a)]
        foldfAlts sgn maxRnd tcBest x acc =
          case isWithin sgn maxRnd tcBest x of
            True -> x : acc
            False -> acc

negaMaxSingleThreaded :: forall a g p. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, Z.PositionState p)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM p (NegaResult a)
negaMaxSingleThreaded env t gen =
    Z.negaMax t gen
