{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module TicTac.TicTac (calcNewPosition, getPossibleMoves, eval, TTPosition (..), TTNode (..)) where
  
import TicTac.Internal.TicTac  
import StratTree.TreeNode  
import Data.List.Lens
import Control.Lens
------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------    
data TTPosition = TTPosition {_grid :: [Int]}
makeLenses ''TTPosition

instance Position TTPosition where
    evaluate = eval 
    possibleMoves = getPossibleMoves
    newPosition pos move = calcNewPosition pos move
    
data TTNode = TTNode {move :: Int, value :: Int, position :: TTPosition}

instance PositionNode TTNode TTPosition where
    getPosition = position 

instance TreeNode TTNode where
    getMove = move
    getValue = value
    
--------------------------------------------------------
-- calculate new position
--------------------------------------------------------
calcNewPosition :: TTPosition -> Int -> TTPosition
calcNewPosition pos move = 
    let value
            | move >=0  = 1
            | otherwise = -1
    in set (grid . ix (abs move)) value pos
  
---------------------------------------------------------
-- get list of possible moves from a given position
--------------------------------------------------------- 
getPossibleMoves :: TTPosition -> Int -> [Int]
getPossibleMoves pos color =  foldr f [] (zip (pos ^. grid) [0..8]) where
    f (x, idx) newList 
        | x == 0        = idx * color : newList
        | otherwise     = newList 
  
--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: TTPosition -> Int 
eval pos 
    | checkWins (_grid pos) 1 == True       = 100
    | checkWins (_grid pos) (-1) == True    = (-100)
    | otherwise                             = 0
