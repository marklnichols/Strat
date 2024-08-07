{-# language GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Strat.StratIO
  ( evalTreeSingleThreaded
  , evalTreeMultiThreaded
  , expandToSingleThreaded
  , expandToParallel
  , negaMaxParallel
  , negaMaxSingleThreaded
  , searchToSingleThreaded
  , searchToMultiThreaded
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Exception (assert)
import Control.Logger.Simple
import Control.Monad.Reader
import qualified Data.List as List (delete)
import Data.Hashable
import Data.Maybe
import Data.Text (pack, append)
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Tree.Zipper
import Strat.ZipTree hiding (expandTo)
import qualified Strat.ZipTree as Z
import Strat.StratTree.TreeNode
import Text.Printf
import System.Random

searchToSingleThreaded
    :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
    => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
searchToSingleThreaded t gen maxDepth maxCritDepth = do
    expanded <- expandToSingleThreaded t maxDepth maxCritDepth
    evalTreeSingleThreaded expanded gen

searchToMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
searchToMultiThreaded t gen maxDepth maxCritDepth = do
    expanded <- expandToParallel t maxDepth maxCritDepth
    evalTreeMultiThreaded expanded gen

evalTreeSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
evalTreeSingleThreaded t gen = do
    r <- ask
    let env = Z.zte r
    res <- negaMaxSingleThreaded env t gen
    return (t, res)

evalTreeMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g, Z.HasZipTreeEnv r)
         => Tree n -> Maybe g -> Z.ZipTreeM r (Tree n, Z.NegaResult n)
evalTreeMultiThreaded t gen = do
    r <- ask
    let env = Z.zte r
    res <- negaMaxParallel env t gen
    return (t, res)

expandToParallel :: (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
                 => T.Tree a -> Int -> Int -> Z.ZipTreeM r (T.Tree a)
expandToParallel t depth critDepth = do
    -- expansion of at least one level should always have been previously done
    let s = printf "\nexpandToParallel called with depth:%d, critDepth:%d" depth critDepth
    liftIO $ logInfo $ pack $ s
    let (tSize, tLevels)  = treeSize t
    liftIO $ logInfo $ "Tree size: " `append` pack (show tSize)
    liftIO $ logInfo $ pack (show tLevels)
    let _num_levels = assert (length tLevels >= 2) (length tLevels)
    let theChildren = T.subForest t
    liftIO $ logInfo $ pack $ printf "expandToParallel -- number of threads that will be created: %d" (length theChildren)
    liftIO $ logInfo $ pack $ printf "(expansion to depth:%d, critDepth %d)" depth critDepth
    env <- ask
    newChildren <- liftIO $ Async.forConcurrently theChildren
      (\x -> runReaderT (Z.expandTo x 2 depth critDepth) env)
    let z = fromTree t
    let newTree = toTree $ modifyTree (\(T.Node x _) -> T.Node x newChildren) z
    return newTree

expandToSingleThreaded :: forall a r p. (Ord a, Show a, ZipTreeNode a, HasZipTreeEnv r)
                       => T.Tree a -> Int -> Int -> Z.ZipTreeM r (T.Tree a)
expandToSingleThreaded t depth critDepth = do
  liftIO $ logInfo $ pack $ printf "expandSingleThreaded called with depth:%d, critDepth:%d" depth critDepth
  Z.expandTo t 1 depth critDepth

negaMaxParallel :: (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g, HasZipTreeEnv r)
        => Z.ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM r (NegaResult a)
negaMaxParallel env t gen = do
    let theChildren = T.subForest t

    resultsList <- liftIO $ Async.forConcurrently theChildren (\x -> do
        (threadTC, threadEC) <- runReaderT (Z.negaWorker x) env
        let threadNode = T.rootLabel x
        return ( threadTC { node = threadNode , movePath = movePath threadTC ++  [threadNode] }
                 , threadEC))
    let sign = ztnSign $ T.rootLabel t
    let initTC = if sign == Pos then Min else Max
    let curried = foldf sign
    let (theBestTC, ec::Int) = foldr curried (initTC, 0) resultsList

    -- TODO: combine this with the code in Strat.ZipTree.negaRnd
    if enableRandom env && isJust gen
      then do
        let theGen = fromJust gen
        let curriedAltf = foldfAlts sign (maxRandomChange env) theBestTC
        let theAlts = foldr (curriedAltf . fst) [] resultsList
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
                  => Sign -> Float -> TraceCmp a -> TraceCmp a -> [TraceCmp a] -> [TraceCmp a]
        foldfAlts sgn maxRnd tcBest x acc =
          if isWithin sgn maxRnd tcBest x
            then x : acc
            else acc

negaMaxSingleThreaded :: forall a g r. (Ord a, Show a, ZipTreeNode a, Hashable a, RandomGen g)
                       => ZipTreeEnv -> T.Tree a -> Maybe g -> Z.ZipTreeM r (NegaResult a)
negaMaxSingleThreaded env t gen = do
    Z.negaMax t gen
