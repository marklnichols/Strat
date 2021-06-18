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
 , showResultMoves
 , showResultMovesFull
 , startGame
 ) where

import Data.List
import Data.Tree
import Strat.Helpers
import qualified Strat.ZipTree as Z
import Strat.StratTree.TreeNode
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration)

gameEnv :: Env
gameEnv = Env { equivThreshold = 0.1, p1Comp = False, p2Comp = True }

startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n)
          => o -> Tree n -> Int -> Int -> IO ()
startGame o node depth critDepth = do
  rnd <- getStdGen
  let newTree = expandTree node 2 2
  loop rnd o newTree depth critDepth

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g , Ord n, Eval n)
     => g -> o -> Tree n -> Int -> Int -> IO ()
loop gen o node depth critDepth = do
    let label = rootLabel node
    updateBoard o label
    theNext <- case final $ label of
        WWins -> do
            out o "White wins."
            return Nothing
        BWins -> do
            out o "Black wins."
            return Nothing
        Draw -> do
            out o "Draw."
            return Nothing
        _ -> do
            nextNode <- if isCompTurn (Z.ztnSign label)
                  then computersTurn gen o node depth critDepth
                  else playersTurn gen o node depth critDepth
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next depth critDepth

-- TODO: remove gen, maxDepth,  and maxCritDepth params once its clear that 'exclusions'
-- list is not needed
playersTurn :: forall o n m g. (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g)
           => g -> o -> Tree n -> Int -> Int -> IO (Tree n)
playersTurn _gen o t _maxDepth _maxCritDepth = do
    -- TODO: remove this exclusions parameter once its clear it is not needed
    mv <- getPlayerMove o t []
    return (findMove t mv)

sortDepth :: Int
sortDepth = 2

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g, Ord n, Eval n)
             => g -> o -> Tree n -> Int -> Int -> IO (Tree n)
computersTurn gen o t maxDepth maxCritDepth = do
    (sec, newRoot) <- duration $ do
        (preSortT, _preSortResult) <- preSort t sortDepth
        (expandedT, result) <- searchTo preSortT gen (equivThreshold gameEnv) maxDepth maxCritDepth
        putStrLn "\n--------------------------------------------------\n"
        showCompMove o expandedT result True
        let nextMove = getMove $ Z.moveNode (Z.best result)
        return (findMove expandedT nextMove)
    putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    return newRoot

--TODO remove this:
showResultMoves :: (TreeNode n m ) => Z.NegaResult n -> String
showResultMoves Z.NegaResult{..} =
    let strs = showNegaMovesBrief <$> allMoves
    in intercalate ", " strs

--TODO remove this:
showResultMovesFull :: (TreeNode n m ) => Z.NegaResult n -> String
showResultMovesFull Z.NegaResult{..} =
    let strs = showNegaMovesFull <$> allMoves
    in intercalate ", " strs

--TODO remove this:
showNegaMovesBrief :: (TreeNode n m) => Z.NegaMoves n -> String
showNegaMovesBrief Z.NegaMoves{..} =
    let mv = getMove moveNode
    in (show mv) ++ ":" ++ (show evalScore)

--TODO remove this:
showNegaMovesFull :: (TreeNode n m) => Z.NegaMoves n -> String
showNegaMovesFull Z.NegaMoves{..} =
    let mv = getMove moveNode
        mvStr = "Move: " ++ (show mv) ++ ":" ++ (show evalScore)
        seqStrs = show <$> moveSeq
        seqStr = intercalate ", " seqStrs
    in mvStr ++ " - Sequence:" ++ seqStr ++ "\n"

expandTree :: (Z.ZipTreeNode n, Ord n, Show n) => Tree n -> Int -> Int -> Tree n
expandTree t depth critDepth = Z.expandTo t depth critDepth

isCompTurn :: Z.Sign -> Bool
isCompTurn sign =
    let p1 = p1Comp gameEnv
        p2 = p2Comp gameEnv
    in if sign == Z.Pos then p1 else p2

preSort :: (Z.ZipTreeNode n, Ord n, Show n, Eval n)
                   => Tree n -> Int
                   -> IO (Tree n, Z.NegaResult n)
preSort t maxDepth = do
    let expanded = expandTree t maxDepth maxDepth
    let res = Z.negaMax expanded True
    let sorted = Z.sortFromResult expanded res
    return (sorted, res)

searchTo :: (Z.ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                   => Tree n -> g -> Float -> Int -> Int
                   -> IO (Tree n, Z.NegaResult n)
searchTo t gen percentVar maxDepth maxCritDepth = do
    let expanded = expandTree t maxDepth maxCritDepth
    let res = Z.negaRnd expanded gen percentVar True
    return (expanded, res)

incrementalSearchTo :: (Z.ZipTreeNode n, Ord n, Show n, RandomGen g)
               => Tree n -> g -> Float -> Int -> Int
               -> (Tree n, Z.NegaResult n)
incrementalSearchTo t gen percentVar maxDepth maxCritDepth =
    incrementalDecent t gen percentVar 1 1 maxDepth maxCritDepth

incrementalDecent :: (Z.ZipTreeNode n, Ord n, Show n, RandomGen g)
                  => Tree n -> g -> Float -> Int -> Int -> Int -> Int
                  -> (Tree n, Z.NegaResult n)
incrementalDecent t gen percentVar curDepth curCritDepth maxDepth maxCritDepth =
    let expanded = expandTree t curDepth curCritDepth
        res = Z.negaRnd expanded gen percentVar True
    in if curDepth == maxDepth && curCritDepth == maxCritDepth
        then (expanded, res)
        else
            let newDepth = if curDepth < maxDepth then curDepth + 1 else curDepth
                newCritDepth = if curCritDepth < maxCritDepth then curCritDepth + 1 else curCritDepth
                sorted = Z.sortFromResult expanded res
            in incrementalDecent sorted gen percentVar newDepth newCritDepth maxDepth maxCritDepth
