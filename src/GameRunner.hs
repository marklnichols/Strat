{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module GameRunner
 ( expandTree
 , searchTo
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
import Strat.StratIO (expandToParallel, negaMaxParallel)
import Strat.StratTree.TreeNode
    ( Entry(MoveEntry, CmdEntry),
      Eval,
      FinalState(Draw, WWins, BWins),
      Move,
      Output(showCompMove, updateBoard, out, getPlayerEntry),
      TreeNode(getMove, color, final, possibleMoves) )
import System.Random hiding (next)
import System.Time.Extra (duration, showDuration)
import Text.Printf
-- import Debug.Trace

-- TODO: make a type for all these flags...
startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n)
          => o -> Tree n -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
          -> Bool -> Bool -> Text -> IO ()
startGame o node maxDepth maxCritDepth aiPlaysWhite aiPlaysBlack enablePreSort enableRandom
          enablePruning singleThreaded
          verbose enablePruneTracing enableCmpTracing moveTraceStr = do
  let env = Z.ZipTreeEnv
        { verbose
        , enablePruneTracing
        , enableCmpTracing
        , enableRandom
        -- , maxRandomChange = 2.0
        , maxRandomChange = 10.0
        , enablePruning
        , enablePreSort
        , moveTraceStr
        , maxDepth
        , maxCritDepth
        , aiPlaysWhite
        , aiPlaysBlack
        , singleThreaded
        }
  startGameLoop env o node

startGameLoop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n)
              => Z.ZipTreeEnv -> o -> Tree n -> IO ()
startGameLoop env o node = do
    -- TODO: Test this with 1 1
    newTree <- expandSingleThreaded env node 2 2
    putStrLn $ "(startGameLoop - treesize: " ++ show (Z.treeSize newTree) ++ ")\n"
    unless (Z.enablePruning env) $
      putStrLn "***** Alpha-Beta pruning is turned OFF *****";
    when (Z.singleThreaded env) $
      putStrLn "***** Running SINGLE THREADED *****";
    unless (Z.enablePreSort env) $
      putStrLn "***** Pre-evaluation sorting is turned OFF *****";
    when (Z.enableCmpTracing env) $
      putStrLn $ printf "***** Compare tracing for: |%s| is ON *****" (Z.moveTraceStr env)
    when (Z.enablePruneTracing env) $
      putStrLn $ printf "***** Prune tracing for: |%s| is ON *****" (Z.moveTraceStr env)
    putStrLn $ "critDepth: " ++ show (Z.maxCritDepth env)
    if (Z.enableRandom env)
      then do
        rnd <- getStdGen
        loop env (Just rnd) o newTree []
      else do
        liftIO $ putStrLn "***** Random move selection: OFF *****"
        loop env (Nothing :: Maybe StdGen)  o newTree []

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n, RandomGen g)
     => Z.ZipTreeEnv -> Maybe g -> o -> Tree n -> [m] -> IO ()
loop env gen o node moveHistory = do
    let label = rootLabel node
    liftIO $ updateBoard o label
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
            (nextNode, h) <- if isCompTurn (Z.ztnSign label) (Z.aiPlaysWhite env) (Z.aiPlaysBlack env)
                  then computersTurn env gen o node moveHistory
                  else playersTurn env gen o node moveHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop env gen o next updatedHistory

-- TODO: remove this exclusions parameter once its clear it is not needed
-- TODO: remove maxDepth params once its clear that 'exclusions'list is not needed
playersTurn :: forall o n m g. (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, RandomGen g)
           => Z.ZipTreeEnv -> Maybe g -> o -> Tree n -> [m] -> IO (Tree n, [m])
playersTurn env gen o t moveHistory = do
    -- populate 1 deep just so findMove can work with player vs player games
    (expandedT, _result) <- searchToSingleThreaded env t (Nothing :: Maybe StdGen) 1 1
    entry <- getPlayerEntry o expandedT []
    case entry of
      CmdEntry s -> do
        _ <- processCommand s (rootLabel t) moveHistory
        playersTurn env gen o t moveHistory
      MoveEntry mv ->
        case findMove expandedT mv of
          Right newTree -> return (newTree, mv:moveHistory)
          Left s ->  do
              putStrLn s
              let newNodeMoves = possibleMoves (rootLabel expandedT)
              putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn env gen o t moveHistory

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, Ord n, Eval n, RandomGen g)
             => Z.ZipTreeEnv -> Maybe g-> o -> Tree n -> [m] -> IO (Tree n, [m])
computersTurn env gen o t moveHistory = do
    (sec, (newRoot, updatedHistory)) <- duration $ do

        let labe = rootLabel t
        let sss = printf "computersTurn - top - move:%s color:%s" (show (getMove labe)) (show (color labe))
        putStrLn sss

        (expandedT, result) <- searchTo env t gen (Z.maxDepth env) (Z.maxCritDepth env)
        let labe2 = rootLabel expandedT
        let sss2 = printf "computersTurn - after searchTo - move:%s color:%s" (show (getMove labe2)) (show (color labe2))
        putStrLn sss2

        liftIO $ putStrLn "\n--------------------------------------------------\n"
        liftIO $ showCompMove o expandedT result True
        let nextMove = getMove $ Z.moveNode (Z.picked result)
        return (findMove expandedT nextMove, nextMove:moveHistory)
    liftIO $ putStrLn $ "Computer move (time: " ++ showDuration sec ++ "):"
    case newRoot of
        Right r -> return (r, updatedHistory)
        Left s -> do
          let newNodeMoves = possibleMoves (rootLabel t)
          liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
          error s

expandTree :: forall a. (Ord a, Show a, Z.ZipTreeNode a)
           => Z.ZipTreeEnv -> Tree a -> Int -> Int -> IO (Tree a)
expandTree env t depth critDepth =
    if Z.singleThreaded env
      then expandSingleThreaded env t depth critDepth
      else expandMultiThreaded env t depth critDepth

expandSingleThreaded :: forall a. (Ord a, Show a, Z.ZipTreeNode a)
           => Z.ZipTreeEnv -> Tree a -> Int -> Int -> IO (Tree a)
expandSingleThreaded env t depth critDepth = do
    let s = printf "expandSingleThreaded called with depth:%d, critDepth:%d" depth critDepth
    putStrLn s
    runReaderT (Z.expandTo t 1 depth critDepth) env

expandMultiThreaded :: forall a. (Ord a, Show a, Z.ZipTreeNode a)
           => Z.ZipTreeEnv -> Tree a -> Int -> Int -> IO (Tree a)
expandMultiThreaded env t depth critDepth = do
    let s = printf "expandMultiThreaded called with depth:%d, critDepth:%d" depth critDepth
    putStrLn s
    expandToParallel env t depth critDepth

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

searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Z.ZipTreeEnv -> Tree n -> Maybe g -> Int -> Int -> IO (Tree n, Z.NegaResult n)
searchTo env t gen depth critDepth =
    if Z.singleThreaded env
      then searchToSingleThreaded env t gen depth critDepth
      else searchToMultiThreaded env t gen depth critDepth

searchToSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Z.ZipTreeEnv -> Tree n -> Maybe g -> Int -> Int -> IO (Tree n, Z.NegaResult n)
searchToSingleThreaded env t gen maxDepth maxCritDepth = do
    expanded <- expandSingleThreaded env t maxDepth maxCritDepth
    evalTreeSingleThreaded env expanded gen

searchToMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Z.ZipTreeEnv -> Tree n -> Maybe g -> Int -> Int -> IO (Tree n, Z.NegaResult n)
searchToMultiThreaded env t gen maxDepth maxCritDepth = do
    expanded <- expandMultiThreaded env t maxDepth maxCritDepth
    evalTreeMultiThreaded env expanded gen

evalTreeSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Z.ZipTreeEnv -> Tree n -> Maybe g -> IO (Tree n, Z.NegaResult n)
evalTreeSingleThreaded env t gen = do
    res <- runReaderT (Z.negaMax t gen) env
    return (t, res)

evalTreeMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Z.ZipTreeEnv -> Tree n -> Maybe g -> IO (Tree n, Z.NegaResult n)
evalTreeMultiThreaded env t gen = do
    res <- negaMaxParallel env t gen
    return (t, res)

processUndo :: (Move m) => [m] -> IO ()
processUndo (mv : mvs) = do
  putStrLn $ "Undo for |" ++ show mv ++ "|"
  case mvs of
      (prev : _) -> putStrLn $ "Undo for |" ++ show prev ++ "|"
      [] -> putStrLn "Only one move to undo."
processUndo [] = putStrLn "Nothing to undo."

processCommand :: forall n m. (TreeNode n m, Move m, Z.ZipTreeNode n, Hashable n) => String -> n -> [m] -> IO ()
processCommand cmd node moveHistory
    | cmd == "hash" = putStrLn $ "hash of current position: " ++ (show $ Z.nodeHash node)
    | cmd == "list" = putStrLn $ intercalate "\n" (show <$> (reverse moveHistory))
    | cmd == "undo" = processUndo moveHistory
    | otherwise = putStrLn $ "Unhandled Commandi!: " ++ cmd
