{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module GameRunner
 ( expandTree
 , incrementalSearchTo
 , preSort
 , searchTo
 , showResultMoves
 , showResultMovesFull
 , startGame
 ) where

import Control.Monad
import Data.Hashable
import Data.List
import Data.Tree
import Strat.Helpers
import qualified Strat.ZipTree as Z
import Strat.StratTree.TreeNode
    ( Entry(MoveEntry, CmdEntry),
      Env(..),
      Eval,
      FinalState(Draw, WWins, BWins),
      Move,
      Output(showCompMove, updateBoard, out, getPlayerEntry),
      TreeNode(getMove, final, possibleMoves) )
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration)

gameEnv :: Env
gameEnv = Env { equivThreshold = 0.1 }

-- TODO: make an enviroment for these flags...
startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n)
          => o -> Tree n -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
startGame o node depth aiPlaysWhite aiPlaysBlack useRandom enablePruning = do
  let newTree = expandTree node 2
  unless enablePruning $
    putStrLn "***** Alpha-Beta pruning is turned OFF *****";

  if useRandom
    then do
      rnd <- getStdGen
      loop (Just rnd) o newTree depth aiPlaysWhite aiPlaysBlack enablePruning []
    else do
      putStrLn "***** Random move selection: OFF *****"
      loop (Nothing :: Maybe StdGen) o newTree depth aiPlaysWhite aiPlaysBlack enablePruning []

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g , Ord n, Eval n, Hashable n)
     => Maybe g -> o -> Tree n -> Int -> Bool -> Bool -> Bool -> [m] -> IO ()
loop gen o node depth aiPlaysWhite aiPlaysBlack enablePruning moveHistory = do
    let label = rootLabel node
    updateBoard o label
    (theNext, updatedHistory) <- case final $ label of
        WWins -> do
            out o "White wins."
            return (Nothing, moveHistory)
        BWins -> do
            out o "Black wins."
            return (Nothing, moveHistory)
        Draw -> do
            out o "Draw."
            return (Nothing, moveHistory)
        _ -> do
            (nextNode, h) <- if isCompTurn (Z.ztnSign label) aiPlaysWhite aiPlaysBlack
                  then computersTurn gen o node depth enablePruning moveHistory

                  else playersTurn o node depth enablePruning moveHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next depth aiPlaysWhite aiPlaysBlack enablePruning updatedHistory

-- TODO: remove this exclusions parameter once its clear it is not needed
-- TODO: remove maxDepth params once its clear that 'exclusions'list is not needed
playersTurn :: forall o n m. (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n)
           => o -> Tree n -> Int -> Bool -> [m] -> IO (Tree n, [m])
playersTurn o t maxDepth enablePruning moveHistory = do
    -- populate 1 deep just so findMove can work with player vs player games
    (expandedT, _result) <- searchTo t (Nothing::Maybe StdGen) (equivThreshold gameEnv) 1 enablePruning
    entry <- getPlayerEntry o expandedT []
    case entry of
      CmdEntry s -> do
        _ <- processCommand s (rootLabel t) moveHistory
        playersTurn o t maxDepth enablePruning moveHistory
      MoveEntry mv ->
        case findMove expandedT mv of
          Right newTree -> return (newTree, mv:moveHistory)
          Left s ->  do
              putStrLn s
              let newNodeMoves = possibleMoves $ rootLabel expandedT
              putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn o t maxDepth enablePruning moveHistory

sortDepth :: Int
sortDepth = 2

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, RandomGen g, Ord n, Eval n)
             => Maybe g -> o -> Tree n -> Int -> Bool -> [m] -> IO (Tree n, [m])
computersTurn gen o t maxDepth enablePruning moveHistory = do
    (sec, (newRoot, updatedHistory)) <- duration $ do
        (preSortT, _preSortResult) <- preSort t sortDepth
        (expandedT, result) <- searchTo preSortT gen (equivThreshold gameEnv) maxDepth enablePruning
        putStrLn "\n--------------------------------------------------\n"
        showCompMove o expandedT result True
        let nextMove = getMove $ Z.moveNode (Z.best result)
        return (findMove expandedT nextMove, nextMove:moveHistory)
    putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    case newRoot of
        Right r -> return (r, updatedHistory)
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

isCompTurn :: Z.Sign -> Bool -> Bool -> Bool
isCompTurn sign aiPlaysWhite aiPlaysBlack
   | sign == Z.Pos
   , aiPlaysWhite
     = True
   | sign == Z.Neg
   , aiPlaysBlack
     = True
   | otherwise
     = False

preSort :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n)
                   => Tree n -> Int
                   -> IO (Tree n, Z.NegaResult n)
preSort t maxDepth = do
    let expanded = expandTree t maxDepth
    let res = Z.negaMax expanded False
    let sorted = Z.sortFromResult expanded res
    return (sorted, res)

searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
                   => Tree n -> Maybe g -> Float -> Int -> Bool
                   -> IO (Tree n, Z.NegaResult n)
searchTo t gen percentVar maxDepth enablePruning = do
    let expanded = expandTree t maxDepth
    let res =
          case gen of
              Just g -> Z.negaRnd expanded g percentVar enablePruning
              Nothing -> Z.negaMax expanded enablePruning
    return (expanded, res)

incrementalSearchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, RandomGen g)
               => Tree n -> g -> Float -> Int
               -> (Tree n, Z.NegaResult n)
incrementalSearchTo t gen percentVar maxDepth =
    incrementalDecent t gen percentVar 1 maxDepth

incrementalDecent :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, RandomGen g)
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

processUndo :: (Move m) => [m] -> IO ()
processUndo (mv : mvs) = do
  putStrLn $ "Undo for |" ++ show mv ++ "|"
  case mvs of
      (prev : _) -> putStrLn $ "Undo for |" ++ show prev ++ "|"
      [] -> putStrLn "Only one move to undo."
processUndo [] = putStrLn "Nothing to undo."

-- playersTurn :: forall o n m. (Output o n m, TreeNode n m, Z.ZipTreeNode n)
processCommand :: forall n m. (TreeNode n m, Move m, Z.ZipTreeNode n, Hashable n) => String -> n -> [m] -> IO ()
processCommand cmd node moveHistory
    | cmd == "hash" = putStrLn $ "hash of current position: " ++ (show $ Z.nodeHash node)
    | cmd == "list" = putStrLn $ intercalate "\n" (show <$> (reverse moveHistory))
    | cmd == "undo" = processUndo moveHistory
    | otherwise = putStrLn $ "Unhandled Commandi!: " ++ cmd
