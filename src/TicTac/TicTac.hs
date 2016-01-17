{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module TicTac.TicTac (calcNewNode, getPossibleMoves, eval, TTPosition (..), TTNode (..)) where
  
import TicTac.Internal.TicTac  
import StratTree.TreeNode  
import Data.List.Lens
import Control.Lens

------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------    
data TTPosition = TTPosition {_grid :: [Int], _color :: Int}
makeLenses ''TTPosition
    
data TTNode = TTNode {move :: Int, value :: Int, position :: TTPosition}

instance PositionNode TTNode where
    newNode node move = calcNewNode node move 
    evaluate = eval 
    possibleMoves = getPossibleMoves
    color = _color . position
    
instance TreeNode TTNode where
    getMove = move
    getValue = value
    
--------------------------------------------------------
-- calculate new node from a previous node and a move
--------------------------------------------------------
calcNewNode :: TTNode -> Int -> TTNode
calcNewNode node mv = 
    let val
            | mv >=0    = 1
            | otherwise = -1
        newPos = set (grid . ix (abs mv)) val (position node)
    in  TTNode (move node) (value node) newPos
    
 
---------------------------------------------------------
-- get list of possible moves from a given position
--------------------------------------------------------- 
-- TODO extend the use of lens to navigate node -> position -> 
getPossibleMoves :: TTNode -> [Int]
getPossibleMoves n =  foldr f [] (zip ((position n) ^. grid) [0..8]) where
    f (x, idx) newList 
        | x == 0        = idx * (_color (position n)) : newList
        | otherwise     = newList 
  
--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: TTNode -> Int 
eval n 
    | checkWins (_grid (position n)) 1 == True      = 100
    | checkWins (_grid (position n)) (-1) == True   = (-100)
    | otherwise                                     = 0
