{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Strat.StratIO where

import qualified Control.Concurrent.Async as Async
import Control.Exception (assert)
import Control.Monad.Reader
import Data.Hashable
import qualified Data.Tree as T
import Data.Tree.Zipper
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
                 => T.Tree a -> Int -> Int -> Z.ZipReaderIO p (T.Tree a)
expandToParallel t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let (_, levels) = treeSize t
    let _num_levels = assert (length levels >= 2) (length levels)
    let theChildren = T.subForest t
    liftIO $ putStrLn $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    liftIO $ putStrLn $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth

    -- a little weird: runReaderT within a ReaderT monad - to get an IO type usable with Async
    env <- ask
    newChildren <- liftIO $ Async.forConcurrently theChildren (\x -> runReaderT (Z.expandTo x 2 depth critDepth) env)
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z

    liftIO $ putStrLn $ printf "size of newTree: %s" (show (treeSize newTree))
    return newTree

expandToSingleThreaded :: forall a p. (Ord a, Show a, ZipTreeNode a)
                 => T.Tree a -> Int -> Int -> Z.ZipReaderIO p (T.Tree a)
expandToSingleThreaded t depth critDepth =
    Z.expandTo t 1 depth critDepth

negaMaxParallel :: forall a g p. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
        => Z.ZipTreeEnv p -> T.Tree a -> Maybe g -> Z.ZipReaderIO p  (NegaResult a)
negaMaxParallel env t _gen = do
    let theChildren = T.subForest t
    env <- ask
    tcResults <- liftIO $ Async.forConcurrently theChildren (\x -> do
          (threadTC, threadEC) <- runReaderT (Z.negaWorker x) env
          -- The TraceCmp returned from each thread needs to be re-attached to
          -- the thread's root node,  part of the move sequence:
          let threadNode = T.rootLabel x
          return (threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }, threadEC))

    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max
    let curried = foldf sign
    let (theBest, ec::Int) = foldr curried (initTC, 0) tcResults
    liftIO $ putStrLn $ printf "theBest after comparing thread results: %s" (showTC theBest)

    -- TODO: find the best among the thread results
    -- TODO: find random alternates within the threshold
    -- TODO: make a random pick among the alternatives
    -- just for now:
    return $ NegaResult { picked = toNegaMoves (revTraceCmp theBest)
                        , bestScore = toNegaMoves (revTraceCmp theBest)
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

negaMaxSingleThreaded :: forall a g p. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
        => Z.ZipTreeEnv p -> T.Tree a -> Maybe g -> Z.ZipReaderIO p (NegaResult a)
negaMaxSingleThreaded env t gen =
    Z.negaMax t gen
