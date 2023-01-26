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


expandToParallel :: forall a. (Ord a, Show a, ZipTreeNode a)
                 => ZipTreeEnv -> T.Tree a -> Int -> Int -> IO (T.Tree a)
expandToParallel env t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let (_, levels) = treeSize t
    let _num_levels = assert (length levels >= 2) (length levels)
    let theChildren = T.subForest t
    putStrLn $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    putStrLn $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth
    newChildren <- Async.forConcurrently theChildren (\x -> runReaderT (Z.expandTo x 2 depth critDepth) env)
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z
    putStrLn $ printf "size of newTree: %s" (show (treeSize newTree))
    return newTree

negaMaxParallel :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> IO (NegaResult a)
negaMaxParallel env t _gen = do
    let theChildren = T.subForest t
    tcResults <- Async.forConcurrently theChildren (\x -> do
          (threadTC, threadEC) <- runReaderT (Z.negaWorker x) env
          -- The TraceCmp returned from each thread needs to be re-attached to
          -- the thread's root node,  part of the move sequence:
          let threadNode = T.rootLabel x
          return (threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }, threadEC))

    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max
    let curried = foldf sign
    let (theBest, ec::Int) = foldr curried (initTC, 0) tcResults
    putStrLn $ printf "theBest after comparing thread results: %s" (showTC theBest)

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
