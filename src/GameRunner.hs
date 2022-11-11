{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
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
 -- , incrementalSearchTo
 -- , preSort
 , searchTo
 -- , showResultMoves
 -- , showResultMovesFull
 , startGame
 ) where

import Control.Monad
import Control.Monad.Reader
import Data.Hashable
import Data.List
import Data.Text (Text)
import Data.Tree
import Strat.Helpers
import qualified Strat.ZipTree as Z
import Strat.StratTree.TreeNode
    ( Entry(MoveEntry, CmdEntry),
      Eval,
      FinalState(Draw, WWins, BWins),
      Move,
      Output(showCompMove, updateBoard, out, getPlayerEntry),
      TreeNode(getMove, final, possibleMoves) )
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration)
-- import Debug.Trace

-- TODO: make a type for all these flags...
startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n)
          => o -> Tree n -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Text -> IO ()
startGame o node depth critDepth aiWhite aiBlack usePreSort useRandom enablePruning verbosity bPruneTracing
          bCmpTracing moveTrace = do
  let env = Z.ZipTreeEnv
        { verbose = verbosity
        , enablePruneTracing = bPruneTracing
        , enableCmpTracing = bCmpTracing
        , enableRandom = useRandom
        , maxRandomChange = 10.0
        , enablePreSort = usePreSort
        , moveTraceStr = moveTrace
        , maxDepth = depth
        , maxCritDepth = critDepth
        , aiPlaysWhite = aiWhite
        , aiPlaysBlack = aiBlack
        }
  runReaderT (startGameLoop o node enablePruning) env

startGameLoop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n)
              => o -> Tree n -> Bool -> Z.ZipReaderT IO ()
startGameLoop o node enablePruning = do
    env <- ask
    newTree <- expandTree node 2 2
    unless enablePruning $
      liftIO $ putStrLn "***** Alpha-Beta pruning is turned OFF *****";
    unless (Z.enablePreSort env) $
      liftIO $ putStrLn "***** Pre-evaluation sorting is turned OFF *****";
    when (Z.verbose env) $
      liftIO $ putStrLn "***** Verbose output *****";
    liftIO $ putStrLn $ "critDepth: " ++ show (Z.maxCritDepth env)
    if (Z.enableRandom env)
      then do
        rnd <- getStdGen
        loop (Just rnd) o newTree enablePruning []
      else do
        liftIO $ putStrLn "***** Random move selection: OFF *****"
        loop (Nothing :: Maybe StdGen)  o newTree enablePruning []

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n, RandomGen g)
     => Maybe g -> o -> Tree n -> Bool -> [m] -> Z.ZipReaderT IO ()
loop gen o node enablePruning moveHistory = do
    env <- ask
    let label = rootLabel node
    liftIO $ updateBoard o label
    (theNext, updatedHistory) <- case final $ label of
        WWins -> do
            liftIO $ out o "White wins."
            return (Nothing, moveHistory)
        BWins -> do
            liftIO $ out o "Black wins."
            return (Nothing, moveHistory)
        Draw -> do
            liftIO $ out o "Draw."
            return (Nothing, moveHistory)
        _ -> do
            (nextNode, h) <- if isCompTurn (Z.ztnSign label) (Z.aiPlaysWhite env) (Z.aiPlaysBlack env)
                  then computersTurn gen o node enablePruning moveHistory
                  else playersTurn o node enablePruning moveHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next enablePruning updatedHistory

-- TODO: remove this exclusions parameter once its clear it is not needed
-- TODO: remove maxDepth params once its clear that 'exclusions'list is not needed
playersTurn :: forall o n m. (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n)
           => o -> Tree n -> Bool -> [m] -> Z.ZipReaderT IO (Tree n, [m])
playersTurn o t enablePruning moveHistory = do
    -- populate 1 deep just so findMove can work with player vs player games

    (expandedT, _result) <- searchTo t (Nothing :: Maybe StdGen) 1 1 False
    entry <- liftIO $ getPlayerEntry o expandedT []
    case entry of
      CmdEntry s -> do
        _ <- liftIO $ processCommand s (rootLabel t) moveHistory
        playersTurn o t enablePruning moveHistory
      MoveEntry mv ->
        case findMove expandedT mv of
          Right newTree -> return (newTree, mv:moveHistory)
          Left s ->  do
              liftIO $ putStrLn s
              let newNodeMoves = possibleMoves $ rootLabel expandedT
              liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn o t enablePruning moveHistory

-- sortDepth :: Int
-- sortDepth = 2

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, Ord n, Eval n, RandomGen g)
             => Maybe g-> o -> Tree n -> Bool -> [m] -> Z.ZipReaderT IO (Tree n, [m])
computersTurn gen o t enablePruning moveHistory = do
    env <- ask
    (sec, (newRoot, updatedHistory)) <- duration $ do
        --TODO: Look into: preSort is now OFF by default, it seemed to cause a problem with the AI's move,
        -- PLUS it wasn't reducing the evaluations??
        -- preSortT <- do
        --   if Z.enablePreSort env then fst <$> (preSort t sortDepth)
        --   else return t
        -- (expandedT, result) <- searchTo preSortT etc...

        (expandedT, result) <- searchTo t gen (Z.maxDepth env)
                               (Z.maxCritDepth env) enablePruning

        liftIO $ putStrLn "\n--------------------------------------------------\n"
        liftIO $ showCompMove o expandedT result True
        let nextMove = getMove $ Z.moveNode (Z.picked result)
        return (findMove expandedT nextMove, nextMove:moveHistory)
    liftIO $ putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    case newRoot of
        Right r -> return (r, updatedHistory)
        Left s -> do
          let newNodeMoves = possibleMoves $ rootLabel t
          liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
          error s

--TODO remove this:
-- showResultMoves :: (TreeNode n m ) => Z.NegaResult n -> String
-- showResultMoves Z.NegaResult{..} =
--     let strs = showNegaMovesBrief <$> allMoves
--     in intercalate ", " strs

--TODO remove this:
-- showResultMovesFull :: (TreeNode n m ) => Z.NegaResult n -> String
-- showResultMovesFull Z.NegaResult{..} =
--     let strs = showNegaMovesFull <$> allMoves
--     in intercalate ", " strs

--TODO remove this:
-- showNegaMovesBrief :: (TreeNode n m) => Z.NegaMoves n -> String
-- showNegaMovesBrief Z.NegaMoves{..} =
--     let mv = getMove moveNode
--     in (show mv) ++ ":" ++ (show evalScore)

--TODO remove this:
-- showNegaMovesFull :: (TreeNode n m) => Z.NegaMoves n -> String
-- showNegaMovesFull Z.NegaMoves{..} =
--     let mv = getMove moveNode
--         mvStr = "Move: " ++ (show mv) ++ ":" ++ (show evalScore)
--         seqStrs = show <$> moveSeq
--         seqStr = intercalate ", " seqStrs
--     in mvStr ++ " - Sequence:" ++ seqStr ++ "\n"

expandTree :: (Z.ZipTreeNode n, Ord n, Show n)
           => Tree n -> Int -> Int -> Z.ZipReaderT IO (Tree n)
expandTree = Z.expandTo

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

{- Important: pruning is NEVER allowed during the pre-sort -}
-- preSort :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n)
--                    => Tree n -> Int
--                    -> Z.ZipReaderT IO (Tree n, Z.NegaResult n)
-- preSort t maxDepth = do
--     expanded <- expandTree t maxDepth
--     res <- Z.negaMax expanded False -- False for no pruning
--     let sorted = Z.sortFromResult expanded res
--     return (sorted, res)

searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Int -> Int -> Bool -> Z.ZipReaderT IO (Tree n, Z.NegaResult n)
searchTo t gen maxDepth maxCritDepth enablePruning = do
    expanded <- expandTree t maxDepth maxCritDepth
    env <- ask
    res <-
        case gen of
            Just g -> Z.negaRnd expanded g (Z.maxRandomChange env) enablePruning
            Nothing -> Z.negaMax expanded enablePruning
    return (expanded, res)

-- incrementalSearchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, RandomGen g)
--                => Tree n -> g -> Float -> Int
--                -> Z.ZipReaderT IO (Tree n, Z.NegaResult n)
-- incrementalSearchTo t gen percentVar maxDepth =
--     incrementalDecent t gen percentVar 1 maxDepth

-- incrementalDecent :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, RandomGen g)
--                   => Tree n -> g -> Float -> Int -> Int
--                   -> Z.ZipReaderT IO (Tree n, Z.NegaResult n)
-- incrementalDecent t gen percentVar curDepth maxDepth = do
--     expanded <- expandTree t curDepth
--     res <- Z.negaRnd expanded gen percentVar True
--     if curDepth == maxDepth
--         then return (expanded, res)
--         else do
--             let newDepth = if curDepth < maxDepth then curDepth + 1 else curDepth
--             let sorted = Z.sortFromResult expanded res
--             incrementalDecent sorted gen percentVar newDepth maxDepth

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
