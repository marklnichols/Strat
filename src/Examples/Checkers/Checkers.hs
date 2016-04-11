{-# LANGUAGE TemplateHaskell #-}
module Checkers where

import Data.Tree
import StratTree.TreeNode hiding (Result, MoveScore)
import Data.List.Lens
import Control.Lens
import Data.List
import Data.List.Split

---------------------------------------------------------------------------------------------------
-- Data Types
---------------------------------------------------------------------------------------------------
data CkPosition = CkPosition {_grid :: [Int], _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''CkPosition

data CkNode = CkNode {_ckMove :: Int, _ckValue :: Int, _ckErrorValue :: Int, _ckPosition :: CkPosition} deriving (Show)
makeLenses ''CkNode

instance PositionNode CkNode where
    newNode = calcNewNode
    possibleMoves = getPossibleMoves
    color = _clr . _ckPosition
    final = _fin . _ckPosition

instance TreeNode CkNode where
    getMove = _ckMove
    getValue = _ckValue
    getErrorValue = _ckErrorValue

---------------------------------------------------------------------------------------------------
-- starting position,
---------------------------------------------------------------------------------------------------
getStartNode :: Int -> Tree CkNode
getStartNode bottomColor = Node CkNode {_ckMove = -1, _ckValue = 0, _ckErrorValue = 0, _ckPosition = CkPosition 
    {_grid = mkStartGrid bottomColor, _clr = 1, _fin = NotFinal}} []

{------------------ grid 0-45, valid indexes for pieces:
  37  38  39  40
 32  33  34  35
   28  29  30  31
 23  24  25  26
   19  20  21  22
 14  15  16  17
   10  11  12  13
 05  06  07  08  
------------------ offEdgePieces = [0, 1, 2, 3, 4, 9, 18, 27, 36, 41, 42, 43, 44, 45] --} 

mkStartGrid :: Int -> [Int] 
mkStartGrid bottomColor =  fmap (indexToValue bottomColor) [0..45]

indexToValue :: Int -> Int -> Int
indexToValue bottomColor idx 
    | idx < 5               = 99           -- off the edge
    | idx > 40              = 99           -- off the edge
    | idx `mod` 9 == 0      = 99           -- off the edge
    | idx > 22 && idx < 32  = 0            -- center, no initial pieces 
    | idx < 14              = bottomColor  -- player at bottom of board initial pieces
    | otherwise             = negate bottomColor -- player at top initial pieces
  
---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
compact :: [Int] -> [Int]
compact xs = filter (/= 99) xs 

rowSplit :: [Int] -> [[Int]]
rowSplit = chunksOf 4

format :: CkPosition -> String
format p = foldr f "" (rowSplit $ compact (p ^. grid)) where
               f :: [Int] -> String -> String
               f x r = r ++ ( concat $ fmap (\i -> " " ++ show i ++ " ") x) ++ "\n"   
 
tryIt :: String
tryIt = format $ rootLabel (getStartNode 1) ^. ckPosition
 
---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
--TODO: implement
calcNewNode :: CkNode -> Int -> CkNode
calcNewNode node mv = node

---------------------------------------------------------------------------------------------------
-- get list of possible moves from a given position
---------------------------------------------------------------------------------------------------
-- TODO implement
getPossibleMoves :: CkNode -> [Int]
getPossibleMoves n = [1, 2]  

---------------------------------------------------------------------------------------------------
-- diagonal moves on the board
---------------------------------------------------------------------------------------------------
upLeft :: Int -> Int
upLeft = (+4)

upRight :: Int -> Int
upRight = (+5)

downLeft :: Int -> Int
downLeft = (subtract 5)

downRight :: Int -> Int
downRight = (subtract 4)
