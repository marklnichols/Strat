{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module GameRunner
 ( startGame
 , expandTree) where

import Control.Monad.ST
import Data.Mutable
import Data.Tree
import Strat.Helpers
import Strat.StratTree
import Strat.StratTree.TreeNode
import System.Random hiding (next)

gameEnv :: Env
gameEnv = Env { depth = 3, equivThreshold = 0.1, p1Comp = False, p2Comp = True }

startGame :: (Output o n m, TreeNode n m, Ord n, Eval n)
          => o -> Tree n -> IO ()
startGame o node = do
  rnd <- getStdGen
  let newTree = runST $ expandTree node
  loop rnd o newTree 1

loop :: (Output o n m, TreeNode n m, RandomGen g , Ord n, Eval n)
     => g -> o -> Tree n -> Int -> IO ()
loop gen o node turn = do
    updateBoard o $ rootLabel node
    theNext <- case final $ rootLabel node of
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
            nextNode <- if isCompTurn turn
                  then computerMove gen o node turn
                  else playerMove o node turn
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop gen o next (swapTurns turn)

playerMove :: (Output o n m, TreeNode n m) => o -> Tree n -> Int -> IO (Tree n)
playerMove o tree turn = do
    mv <- getPlayerMove o tree turn
    return (findMove tree mv)

computerMove :: (Output o n m, TreeNode n m, RandomGen g, Ord n, Eval n)
             => g -> o -> Tree n -> Int -> IO (Tree n)
computerMove gen o t turn = do
    let newTree = runST $ expandTree t
    let res@NegaResult{..} = negaRnd newTree (turnToSign turn) gen toFloat (equivThreshold gameEnv)
    let nextMove = getMove $ head $ moveSeq best
    showCompMove o newTree res
    return (findMove newTree nextMove)

expandTree :: (Mutable s n, TreeNode n m)
           => Tree n -> ST s (Tree n)
expandTree t =
    let newTree = do
          r <- thawRef t
          expandTo r makeChildren Nothing (depth gameEnv)
    in newTree

isCompTurn :: Int -> Bool
isCompTurn turn =
    let p1 = p1Comp gameEnv
        p2 = p2Comp gameEnv
    in if turn == 1 then p1 else p2

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

--TODO: allow player to play black
-- convert values 1 | 2 into 1 | -1
turnToSign :: Int -> Sign
turnToSign 1 = Sign 1
turnToSign 2 = Sign (-1)
turnToSign n = error $ "Illegal value in turnToSign (" ++ show n ++ ")"
