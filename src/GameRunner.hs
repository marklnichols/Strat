{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- TODO: make some typedefs for these Bools...
startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n)
          => o -> Tree n -> Int -> Bool -> Bool -> Bool -> IO ()
startGame o node depth aiPlaysWhite aiPlaysBlack useRandom = do
  let newTree = expandTree node 2
  if useRandom then do
      rnd <- getStdGen
      loop (Just rnd) o newTree depth aiPlaysWhite aiPlaysBlack []
  else do
      putStrLn "***** Random move selection: OFF *****"
      loop (Nothing :: Maybe StdGen) o newTree depth aiPlaysWhite aiPlaysBlack []

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g , Ord n, Eval n)
     => Maybe g -> o -> Tree n -> Int -> Bool -> Bool -> [m] -> IO ()
loop gen o node depth aiPlaysWhite aiPlaysBlack moveHistory = do
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
                  then computersTurn gen o node depth moveHistory
                  else playersTurn o node depth moveHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next depth aiPlaysWhite aiPlaysBlack updatedHistory

-- TODO: remove maxDepth params once its clear that 'exclusions'
-- list is not needed
playersTurn :: forall o n m. (Output o n m, TreeNode n m, Z.ZipTreeNode n)
           => o -> Tree n -> Int -> [m] -> IO (Tree n, [m])
playersTurn o t maxDepth moveHistory = do
    -- TODO: remove this exclusions parameter once its clear it is not needed
    entry <- getPlayerEntry o t []
    case entry of
      CmdEntry s -> do
        _ <- processCommand s moveHistory
        playersTurn o t maxDepth moveHistory
      MoveEntry mv ->
        case findMove t mv of
          Right newTree -> return (newTree, mv:moveHistory)
          Left s ->  do
              putStrLn s
              let newNodeMoves = possibleMoves $ rootLabel t
              putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn o t maxDepth moveHistory

sortDepth :: Int
sortDepth = 2

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, RandomGen g, Ord n, Eval n)
             => Maybe g -> o -> Tree n -> Int -> [m] -> IO (Tree n, [m])
computersTurn gen o t maxDepth moveHistory = do
    (sec, (newRoot, updatedHistory)) <- duration $ do
        (preSortT, _preSortResult) <- preSort t sortDepth
        (expandedT, result) <- searchTo preSortT gen (equivThreshold gameEnv) maxDepth
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

preSort :: (Z.ZipTreeNode n, Ord n, Show n, Eval n)
                   => Tree n -> Int
                   -> IO (Tree n, Z.NegaResult n)
preSort t maxDepth = do
    let expanded = expandTree t maxDepth
    let res = Z.negaMax expanded True
    let sorted = Z.sortFromResult expanded res
    return (sorted, res)

searchTo :: (Z.ZipTreeNode n, Ord n, Show n, Eval n, RandomGen g)
                   => Tree n -> Maybe g -> Float -> Int
                   -> IO (Tree n, Z.NegaResult n)
searchTo t gen percentVar maxDepth = do
    let expanded = expandTree t maxDepth
    let res =
          case gen of
              Just g -> Z.negaRnd expanded g percentVar True
              Nothing -> Z.negaMax expanded True
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

processUndo :: (Move m) => [m] -> IO ()
processUndo (mv : mvs) = do
  putStrLn $ "Undo for |" ++ show mv ++ "|"
  case mvs of
      (prev : _) -> putStrLn $ "Undo for |" ++ show prev ++ "|"
      [] -> putStrLn "Only one move to undo."
processUndo [] = putStrLn "Nothing to undo."

processCommand :: (Move m) => String -> [m] -> IO ()
processCommand cmd moveHistory
    | cmd == "list" = putStrLn $ intercalate "\n" (show <$> (reverse moveHistory))
    | cmd == "undo" = processUndo moveHistory
    | otherwise = putStrLn $ "Unhandled Commandi!: " ++ cmd
