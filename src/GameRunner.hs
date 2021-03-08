{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module GameRunner
 ( expandTree
 , startGame
 ) where

import Data.Tree
import Strat.Helpers
import Strat.ZipTree
import Strat.StratTree.TreeNode
import System.Random hiding (next)
-- import System.Time.Extra (duration, showDuration)
import Debug.Trace
import Text.Printf

gameEnv :: Env
gameEnv = Env { equivThreshold = 0.05, p1Comp = False, p2Comp = True }

startGame :: (Output o n m, TreeNode n m, ZipTreeNode n, Ord n, Eval n)
          => o -> Tree n -> Int -> Int -> IO ()
startGame o node depth critDepth = do
  rnd <- getStdGen
  let newTree = expandTree node 2 2
  loop rnd o newTree depth critDepth

loop :: (Output o n m, TreeNode n m, ZipTreeNode n, RandomGen g , Ord n, Eval n)
     => g -> o -> Tree n -> Int -> Int -> IO ()
loop gen o node depth critDepth = do
    let label = rootLabel node
    updateBoard o label
    theNext <- case final $ label of
        WWins -> do
            out o "White wins."
            return Nothing
        BWins -> do
            out o "White wins."
            return Nothing
        Draw -> do
            out o "Draw."
            return Nothing
        _ -> do
            nextNode <- if isCompTurn (ztnSign label)
                  then computerMove gen o node depth critDepth
                  else playerMove o node depth critDepth
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next depth critDepth

playerMove :: (Output o n m, TreeNode n m, ZipTreeNode n)
           => o -> Tree n -> Int -> Int -> IO (Tree n)
playerMove o tree _depth _critDepth = do
    mv <- getPlayerMove o tree
    return (findMove tree mv)

computerMove :: (Output o n m, TreeNode n m, ZipTreeNode n, RandomGen g, Ord n, Eval n)
             => g -> o -> Tree n -> Int -> Int -> IO (Tree n)
computerMove gen o t maxDepth maxCritDepth = do
    -- (newRoot, res@NegaResult{..}) <- runIncremental o t gen (equivThreshold gameEnv) maxDepth maxCritDepth
    (newRoot, res@NegaResult{..}) <- runNonIncremental t gen (equivThreshold gameEnv) maxDepth maxCritDepth
    let nextMove = getMove $ head $ moveSeq best
    putStrLn "\n--------------------------------------------------\n"
    putStrLn "Computer move:"
    showCompMove o newRoot res True
    return (findMove newRoot nextMove)

expandTree :: (TreeNode n m, ZipTreeNode n) => Tree n -> Int -> Int -> Tree n
expandTree t depth critDepth =
    let (newTree, str) =
          let newT = expandTo t depth critDepth
              s = printf "expandTree - size before: %s, size after: %s"
                  (show (treeSize t)) (show (treeSize newT))
          in (newT, s)
  in trace str newTree

isCompTurn :: Sign -> Bool
isCompTurn sign =
    let p1 = p1Comp gameEnv
        p2 = p2Comp gameEnv
    in if sign == Pos then p1 else p2

runNonIncremental :: (TreeNode n m, ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                  => Tree n
                  -> g
                  -> Float
                  -> Int
                  -> Int
                  -> IO (Tree n, NegaResult n)
runNonIncremental t _gen _percentVar maxDepth maxCritDepth = do
    let expanded = expandTree t maxDepth maxCritDepth
    -- let res = negaRnd expanded gen percentVar True
    let res = negaMax expanded True
    return (expanded, res)

_runIncremental :: (Output o n m, TreeNode n m, ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                  => o
                  -> Tree n
                  -> g
                  -> Float
                  -> Int
                  -> Int
                  -> IO (Tree n, NegaResult n)
_runIncremental o t gen percentVar maxDepth maxCritDepth =
    incrementalDecent o t gen percentVar 1 1 maxDepth maxCritDepth

--TODO: add sort
incrementalDecent :: (Output o n m, TreeNode n m, ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                  => o -> Tree n -> g -> Float -> Int -> Int -> Int -> Int
                  -> IO (Tree n, NegaResult n)
incrementalDecent o t gen percentVar curDepth curCritDepth maxDepth maxCritDepth = do
    let expanded = expandTree t curDepth curCritDepth
    let res = negaRnd expanded gen percentVar True
    if curDepth == maxDepth && curCritDepth == maxCritDepth
        then return (expanded, res)
        else
            let newDepth = if curDepth < maxDepth then curDepth + 1 else curDepth
                newCritDepth = if curCritDepth < maxCritDepth then curCritDepth + 1 else curCritDepth
            in incrementalDecent o expanded gen percentVar newDepth newCritDepth maxDepth maxCritDepth
