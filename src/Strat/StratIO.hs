{-# language GHC2021 #-}
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

expandToParallel :: (Ord a, Show a, ZipTreeNode a, Z.PositionState p)
                 => T.Tree a -> Int -> Int -> Z.ZipTreeM p (T.Tree a)
expandToParallel t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let (_, levels) = treeSize t
    let _num_levels = assert (length levels >= 2) (length levels)
    let theChildren = T.subForest t
    liftIO $ putStrLn $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    liftIO $ putStrLn $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth
    env <- ask
    origState <- get
    triples <- liftIO $ Async.forConcurrently theChildren
      (\x -> runRWST (Z.expandTo x 2 depth critDepth) env origState)
    let newChildren = fst3 <$> triples
    let newState = Z.combine (snd3 <$> triples)
    put newState
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z
    return newTree

expandToSingleThreaded :: forall a p. (Ord a, Show a, ZipTreeNode a)
                       => T.Tree a -> Int -> Int -> Z.ZipTreeM p (T.Tree a)
expandToSingleThreaded t depth critDepth = Z.expandTo t 1 depth critDepth

negaMaxParallel :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, Z.PositionState p)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM p  (NegaResult a)
negaMaxParallel env t gen = do
    let theChildren = T.subForest t
    env <- ask
    origState <- get
    outerList <- liftIO $ Async.forConcurrently theChildren (\x -> do
        triple <- runRWST (Z.negaWorker x) env origState
        let (threadTC, threadEC) = fst3 triple
        let threadNode = T.rootLabel x
        let newState' = snd3 triple
        return ( ( threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }
                 , threadEC)
               , newState'))
    let newState = Z.combine $ snd <$> outerList
    put newState
    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max
    let resultsList = fst <$> outerList
    let curried = foldf sign
    let (theBestTC, ec::Int) = foldr curried (initTC, 0) resultsList

    -- TODO: combine this with the code in Strat.ZipTree.negaRnd
    if enableRandom env && isJust gen
      then do
        let theGen = fromJust gen
        let maxRnd = maxRandomChange env
        let curriedAltf = foldfAlts sign (maxRandomChange env) theBestTC
        let theAlts = foldr curriedAltf [] (fst <$> resultsList)
        let allChoices = theBestTC : theAlts
        let pickedTC = Z.pickOne theGen allChoices
        let notPicked = List.delete pickedTC allChoices
        return NegaResult { picked = toNegaMoves pickedTC
                          , bestScore = toNegaMoves theBestTC
                          , alternatives = toNegaMoves <$> notPicked
                          , evalCount = ec }
      else
        return $ NegaResult { picked = toNegaMoves theBestTC
                            , bestScore = toNegaMoves theBestTC
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
          if isWithin sgn maxRnd tcBest x
            then x : acc
            else acc

negaMaxSingleThreaded :: forall a g p. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, Z.PositionState p)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM p (NegaResult a)
negaMaxSingleThreaded env t gen =
    Z.negaMax t gen
