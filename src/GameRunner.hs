{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module GameRunner
 ( searchTo
 , expandSingleThreaded
 , startGame
 ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Data.Hashable
import Data.List
import Data.Text (Text)
import Data.Tree
import Strat.Helpers
import qualified Strat.ZipTree as Z
import Strat.StratIO (expandToParallel, expandToSingleThreaded,
                      negaMaxParallel, negaMaxSingleThreaded)
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
startGame :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n, Z.PositionState p)
          => o -> Tree n -> p -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
          -> Bool -> Bool -> Text -> IO ()
startGame o node startState maxDepth maxCritDepth aiPlaysWhite aiPlaysBlack enablePreSort enableRandom
          enablePruning singleThreaded
          verbose enablePruneTracing enableCmpTracing moveTraceStr = do
  let env = Z.ZipTreeEnv
        { Z.verbose
        , Z.enablePruneTracing
        , Z.enableCmpTracing
        , Z.enableRandom
        , Z.maxRandomChange = 2.0
        , Z.enablePruning
        , Z.enablePreSort
        , Z.moveTraceStr
        , Z.maxDepth
        , Z.maxCritDepth
        , Z.aiPlaysWhite
        , Z.aiPlaysBlack
        , Z.singleThreaded
        }
  startGameLoop env startState o node

startGameLoop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n, Z.PositionState p)
              => Z.ZipTreeEnv -> p -> o -> Tree n -> IO ()
startGameLoop env state o node = do
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
    rnd <- if Z.enableRandom env
             then
               Just <$> getStdGen
             else do
               liftIO $ putStrLn "***** Random move selection: OFF *****"
               return (Nothing :: Maybe StdGen)
    _ <- liftIO $ runRWST
      ( do
        -- TODO: Test this with 1 1
        newTree <- expandSingleThreaded node 2 2
        liftIO $ putStrLn $ "(startGameLoop - treesize: " ++ show (Z.treeSize newTree) ++ ")"
        loop rnd o newTree []
      ) env state
    return ()

loop :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Ord n, Eval n, Hashable n, RandomGen g)
     => Maybe g -> o -> Tree n -> [m] -> Z.ZipTreeM p ()
loop gen o node moveHistory = do
    let label = rootLabel node
    liftIO $ updateBoard o label
    (theNext, updatedHistory) <- case final label of
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
            (nextNode, h) <- do
                bCompTurn <- isCompTurn (Z.ztnSign label)
                if bCompTurn
                  then computersTurn gen o node moveHistory
                  else playersTurn gen o node moveHistory
            return (Just nextNode, h)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next updatedHistory

-- TODO: remove this exclusions parameter once its clear it is not needed
-- TODO: remove maxDepth params once its clear that 'exclusions'list is not needed
playersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, RandomGen g)
           => Maybe g -> o -> Tree n -> [m] -> Z.ZipTreeM p (Tree n, [m])
playersTurn gen o t moveHistory = do
    -- populate 1 deep just so findMove can work with player vs player games
    (expandedT, _result) <- searchToSingleThreaded t (Nothing :: Maybe StdGen) 1 1
    entry <- liftIO $ getPlayerEntry o expandedT []
    case entry of
      CmdEntry s -> do
        _ <- liftIO $ processCommand s (rootLabel t) moveHistory
        playersTurn gen o t moveHistory
      MoveEntry mv ->
        case findMove expandedT mv of
          Right newTree -> return (newTree, mv:moveHistory)
          Left s ->  do
              liftIO $ putStrLn s
              let newNodeMoves = possibleMoves (rootLabel expandedT)
              liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
              playersTurn gen o t moveHistory

computersTurn :: (Output o n m, TreeNode n m, Z.ZipTreeNode n, Hashable n, Ord n, Eval n, RandomGen g)
              => Maybe g-> o -> Tree n -> [m] -> Z.ZipTreeM p (Tree n, [m])
computersTurn gen o t moveHistory = do
    (sec, (newRoot, updatedHistory)) <- duration $ do
        env <- ask
        let labe = rootLabel t
        (expandedT, result) <- searchTo t gen (Z.maxDepth env) (Z.maxCritDepth env)
        let labe2 = rootLabel expandedT
        liftIO $ putStrLn "\n--------------------------------------------------\n"
        liftIO $ showCompMove o expandedT result True
        let nextMove = getMove $ Z.nmNode (Z.picked result)
        return (findMove expandedT nextMove, nextMove:moveHistory)
    liftIO $ putStrLn $ "Computer move time: " ++ showDuration sec ++ "\n"
    case newRoot of
        Right r -> return (r, updatedHistory)
        Left s -> do
          let newNodeMoves = possibleMoves (rootLabel t)
          liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
          error s

expandSingleThreaded :: (Ord a, Show a, Z.ZipTreeNode a)
           => Tree a -> Int -> Int -> Z.ZipTreeM p (Tree a)
expandSingleThreaded t depth critDepth = do
    let s = printf "expandSingleThreaded called with depth:%d, critDepth:%d" depth critDepth
    liftIO $ putStrLn s
    expandToSingleThreaded t depth critDepth

expandMultiThreaded :: (Ord a, Show a, Z.ZipTreeNode a)
           => Z.ZipTreeEnv -> Tree a -> Int -> Int -> Z.ZipTreeM p (Tree a)
expandMultiThreaded env t depth critDepth = do
    let s = printf "\nexpandMultiThreaded called with depth:%d, critDepth:%d" depth critDepth
    liftIO $ putStrLn s
    expandToParallel t depth critDepth

isCompTurn :: Z.Sign -> Z.ZipTreeM p Bool
isCompTurn sign = do
    env <- ask
    let aiPlaysWhite = Z.aiPlaysWhite env
    let aiPlaysBlack = Z.aiPlaysBlack env
    return $
      case sign of
        Z.Pos
          | aiPlaysWhite -> True
          | otherwise -> False
        Z.Neg
          | aiPlaysBlack -> True
          | otherwise -> False

searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
searchTo t gen depth critDepth = do
    env <- ask
    if Z.singleThreaded env
      then searchToSingleThreaded t gen depth critDepth
      else searchToMultiThreaded t gen depth critDepth

searchToSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
searchToSingleThreaded t gen maxDepth maxCritDepth = do
    expanded <- expandSingleThreaded t maxDepth maxCritDepth
    evalTreeSingleThreaded expanded gen

searchToMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
searchToMultiThreaded t gen maxDepth maxCritDepth = do
    env <- ask
    expanded <- expandMultiThreaded env t maxDepth maxCritDepth
    evalTreeMultiThreaded expanded gen

evalTreeSingleThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
evalTreeSingleThreaded t gen = do
    env <- ask
    res <- negaMaxSingleThreaded env t gen
    return (t, res)

evalTreeMultiThreaded :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
evalTreeMultiThreaded t gen = do
    env <- ask
    res <- negaMaxParallel env t gen
    return (t, res)

processUndo :: (Move m) => [m] -> IO ()
processUndo (mv : mvs) = do
  putStrLn $ "Undo for |" ++ show mv ++ "|"
  case mvs of
      (prev : _) -> putStrLn $ "Undo for |" ++ show prev ++ "|"
      [] -> putStrLn "Only one move to undo."
processUndo [] = putStrLn "Nothing to undo."

processCommand :: (TreeNode n m, Move m, Z.ZipTreeNode n, Hashable n) => String -> n -> [m] -> IO ()
processCommand cmd node moveHistory
    | cmd == "hash" = putStrLn $ "hash of current position: " ++ show (Z.nodeHash node)
    | cmd == "list" = putStrLn $ intercalate "\n" (show <$> reverse moveHistory)
    | cmd == "undo" = processUndo moveHistory
    | otherwise = putStrLn $ "Unhandled Commandi!: " ++ cmd
