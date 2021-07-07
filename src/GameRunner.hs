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
          => o -> Tree n -> Int -> IO ()
startGame o node depth = do
  rnd <- getStdGen
  let newTree = expandTree node 2
  loop rnd o newTree depth

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g , Ord n, Eval n)
     => g -> o -> Tree n -> Int -> IO ()
loop gen o node depth = do
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
                  then computersTurn gen o node depth
                  else playersTurn gen o node depth
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next depth

-- TODO: remove gen, maxDepth params once its clear that 'exclusions'
-- list is not needed
playersTurn :: forall o n m g. (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g)
           => g -> o -> Tree n -> Int -> IO (Tree n)
playersTurn gen o t maxDepth = do
    -- TODO: remove this exclusions parameter once its clear it is not needed
    mv <- getPlayerMove o t []
    case (findMove t mv) of
      Right newTree -> return newTree
      Left s ->  do
          putStrLn s
          let newNodeMoves = possibleMoves $ rootLabel t
          putStrLn $ "Available moves:" ++ show newNodeMoves
          playersTurn gen o t maxDepth

sortDepth :: Int
sortDepth = 2

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g, Ord n, Eval n)
             => g -> o -> Tree n -> Int -> IO (Tree n)
computersTurn gen o t maxDepth = do
    (sec, newRoot) <- duration $ do
        (preSortT, _preSortResult) <- preSort t sortDepth
        (expandedT, result) <- searchTo preSortT gen (equivThreshold gameEnv) maxDepth
        putStrLn "\n--------------------------------------------------\n"
        showCompMove o expandedT result True
        let nextMove = getMove $ Z.moveNode (Z.best result)
        return (findMove expandedT nextMove)
    putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    case newRoot of
        Right r -> return r
        Left s -> do
          let newNodeMoves = possibleMoves $ rootLabel t
          putStrLn $ "Available moves:" ++ show newNodeMoves
          error s


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

expandTree :: (Z.ZipTreeNode n, Ord n, Show n) => Tree n -> Int -> Tree n
expandTree t depth = Z.expandTo t depth

isCompTurn :: Z.Sign -> Bool
isCompTurn sign =
    let p1 = p1Comp gameEnv
        p2 = p2Comp gameEnv
    in if sign == Z.Pos then p1 else p2

preSort :: (Z.ZipTreeNode n, Ord n, Show n, Eval n)
                   => Tree n -> Int
                   -> IO (Tree n, Z.NegaResult n)
preSort t maxDepth = do
    let expanded = expandTree t maxDepth
    let res = Z.negaMax expanded True
    let sorted = Z.sortFromResult expanded res
    return (sorted, res)

searchTo :: (Z.ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                   => Tree n -> g -> Float -> Int
                   -> IO (Tree n, Z.NegaResult n)
searchTo t gen percentVar maxDepth = do
    let expanded = expandTree t maxDepth
    let res = Z.negaRnd expanded gen percentVar True
    return (expanded, res)

incrementalSearchTo :: (Z.ZipTreeNode n, Ord n, Show n, RandomGen g)
               => Tree n -> g -> Float -> Int
               -> (Tree n, Z.NegaResult n)
incrementalSearchTo t gen percentVar maxDepth =
    incrementalDecent t gen percentVar 1 maxDepth

incrementalDecent :: (Z.ZipTreeNode n, Ord n, Show n, RandomGen g)
                  => Tree n -> g -> Float -> Int -> Int
                  -> (Tree n, Z.NegaResult n)
incrementalDecent t gen percentVar curDepth maxDepth =
    let expanded = expandTree t curDepth
        res = Z.negaRnd expanded gen percentVar True
    in if curDepth == maxDepth
        then (expanded, res)
        else
            let newDepth = if curDepth < maxDepth then curDepth + 1 else curDepth
                sorted = Z.sortFromResult expanded res
            in incrementalDecent sorted gen percentVar newDepth maxDepth
