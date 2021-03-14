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
 , incrementalSearchTo
 , preSort
 , searchTo
 , startGame
 ) where

import Data.List
import Data.Tree
import Strat.Helpers
import Strat.ZipTree
import Strat.StratTree.TreeNode
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration)

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

-- computerMove :: (Output o n m, TreeNode n m, ZipTreeNode n, RandomGen g, Ord n, Eval n)
--              => g -> o -> Tree n -> Int -> Int -> IO (Tree n)
-- computerMove gen o t maxDepth maxCritDepth = do
--     let (newRoot, res@NegaResult{..}) = incrementalSearchTo t gen (equivThreshold gameEnv) maxDepth maxCritDepth
--     let nextMove = getMove $ moveNode best
--     putStrLn "\n--------------------------------------------------\n"
--     putStrLn "Computer move:"
--     showCompMove o newRoot res True
--     return (findMove newRoot nextMove)

sortDepth :: Int
sortDepth = 2

computerMove :: (Output o n m, TreeNode n m, ZipTreeNode n, RandomGen g, Ord n, Eval n)
             => g -> o -> Tree n -> Int -> Int -> IO (Tree n)
computerMove gen o t maxDepth maxCritDepth = do
    (sec, newRoot) <- duration $ do
        (preSortT, preSortResult) <- preSort t sortDepth
        putStrLn ("result values used in pre-sorting\n" ++ showResultMoves preSortResult)
        (expandedT, result) <- searchTo preSortT gen (equivThreshold gameEnv) maxDepth maxCritDepth
        putStrLn "\n--------------------------------------------------\n"
        showCompMove o expandedT result True
        let nextMove = getMove $ moveNode (best result)
        return (findMove expandedT nextMove)
    putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    return newRoot

--TODO remove this:
showResultMoves :: (TreeNode n m ) => NegaResult n -> String
showResultMoves NegaResult{..} =
    let strs = showNegaMovesBrief <$> allMoves
    in intercalate ", " strs

--TODO remove this:
showNegaMovesBrief :: (TreeNode n m) => NegaMoves n -> String
showNegaMovesBrief NegaMoves{..} =
    let mv = getMove moveNode
    in (show mv) ++ ":" ++ (show evalScore)

expandTree :: (ZipTreeNode n, Ord n, Show n) => Tree n -> Int -> Int -> Tree n
expandTree t depth critDepth = expandTo t depth critDepth

isCompTurn :: Sign -> Bool
isCompTurn sign =
    let p1 = p1Comp gameEnv
        p2 = p2Comp gameEnv
    in if sign == Pos then p1 else p2

preSort :: (ZipTreeNode n, Ord n, Show n, Eval n)
                   => Tree n -> Int
                   -> IO (Tree n, NegaResult n)
preSort t maxDepth = do
    let expanded = expandTree t maxDepth maxDepth
    let res = negaMax expanded True
    let sorted = sortFromResult expanded res
    return (sorted, res)

searchTo :: (ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                   => Tree n -> g -> Float -> Int -> Int
                   -> IO (Tree n, NegaResult n)
searchTo t gen percentVar maxDepth maxCritDepth = do
    let expanded = expandTree t maxDepth maxCritDepth
    let res = negaRnd expanded gen percentVar True
    return (expanded, res)

incrementalSearchTo :: (ZipTreeNode n, Ord n, Show n, RandomGen g)
               => Tree n -> g -> Float -> Int -> Int
               -> (Tree n, NegaResult n)
incrementalSearchTo t gen percentVar maxDepth maxCritDepth =
    incrementalDecent t gen percentVar 1 1 maxDepth maxCritDepth

incrementalDecent :: (ZipTreeNode n, Ord n, Show n, RandomGen g)
                  => Tree n -> g -> Float -> Int -> Int -> Int -> Int
                  -> (Tree n, NegaResult n)
incrementalDecent t gen percentVar curDepth curCritDepth maxDepth maxCritDepth =
    let expanded = expandTree t curDepth curCritDepth
        res = negaRnd expanded gen percentVar True
    in if curDepth == maxDepth && curCritDepth == maxCritDepth
        then (expanded, res)
        else
            let newDepth = if curDepth < maxDepth then curDepth + 1 else curDepth
                newCritDepth = if curCritDepth < maxCritDepth then curCritDepth + 1 else curCritDepth
                sorted = sortFromResult expanded res
            in incrementalDecent sorted gen percentVar newDepth newCritDepth maxDepth maxCritDepth
