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
    --color = _clr . _ckPosition
    color = view (ckPosition . clr)
    --final = _fin . _ckPosition
    final = view (ckPosition . fin)
    showPosition = format

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

---------------------------------------------------------------------------------------------------
-- Grid layout - indexes 0-45
---------------------------------------------------------------------------------------------------
{-- valid indexes for pieces:    offEdgePieces:              how all 46 values are printed:
  37  38  39  40                 [00, 01, 02, 03, 04,        (indexes in parens are not displayed)    
 32  33  34  35                   09, 18, 27, 36,                 
   28  29  30  31                 41, 42, 43, 44, 45]         (37) (38) (39) (40)                       
 23  24  25  26                                                                
   19  20  21  22                                              32   33   34   35      (36)                        
 14  15  16  17                                                  28   29   30   31                       
   10  11  12  13                                              23   24   25   26      (27)                      
 05  06  07  08                                                  19   20   21   22                      
                                                               14   15   16   17      (18)                      
                                                                 10   11   12   13                      
                                                               05   06   07   08      (09)                      
                                                                                     
                                                              (00) (01) (02) (03) (04)                      
--}
---------------------------------------------------------------------------------------------------                                                                                    
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
{--
from index 0: 
Jump +5, print 4                        -- jump when index % 9 == 0
Jump +4, print offset spaces, print 4
Jump +5, print 4
Jump +4, print offset spaces, print 4
Jump +5, print 4
Jump +4, print offset spaces, print 4
Jump +5, print 4

print 7 rows as:
Jump plus, plus = 9 - plus  alternate 4/5
print nSpaces, nSpaces = 2 - nSpaces  alternate 0/2 -- replicate nSpaces ' '

--}

format :: CkNode -> String
format node =   let xs = node ^. ckPosition ^. grid
                in loop xs 5 "" where
                    loop :: [Int] -> Int -> String -> String
                    loop xs 36 result = result
                    loop xs n result =  let (newIdx, spaces) = case (n `mod` 9) of
                                                                    0 -> (n + 1, " ") 
                                                                    5 -> (n, "")
                                        in loop xs (newIdx + 4) (result ++ rowToStr xs newIdx spaces)
                
                {--
                    if idx == 36 
                        then result 
                        else    let (newIdx, spaces) = 
                                    if idx `mod` 9 == 0
                                        then (idx + 1, " ")
                                        else if idx `mod` 9 == 5
                                            then (idx, "")
                                            else "error"
                                in loop (newIdx + 4) (result ++ rowToStr newIdx spaces)
                   --}     
                            {--    
                            case (idx `mod` 9) of 
                            let pair = case fromIntegral (idx `mod` 9) of 
                                0 -> "abc" --(idx + 1, " ") 
                                5 -> "def" --(idx, "")
                            --in loop (newIdx + 4) (result ++ rowToStr newIdx spaces)
                            in "hello"    
                            --}
                          
                        
                            
                        {--
                        9   -> (10, " ", False)
                        14  -> (14, "", False)
                        18  -> (19, " ", False)
                        23  -> (23, "", False)
                        27  -> (28, " ", False)
                        32  -> (32, "", False)
                        36  -> (36, "", True)
                        
                    in if done result else loop (newIdx + 4) (result ++ rowToStr newIdx spaces)    
--}
                    
rowToStr :: [Int] -> Int -> String -> String
rowToStr xs i spaces = spaces ++ toXOs (xs !! i) ++ 
                                 toXOs (xs !! (i + 1)) ++ 
                                 toXOs (xs !! (i + 2)) ++ 
                                 toXOs (xs !! (i + 3)) ++ "\n"

toXOs :: Int -> String
toXOs 1 = "X "
toXOs (-1) = "O "
toXOs 0 = "- "
toXOs _ = "? "

{--
compact :: [Int] -> [Int]
compact xs = filter (/= 99) xs 

rowSplit :: [Int] -> [[Int]]
rowSplit = chunksOf 4

format :: CkNode -> String
format n =  let g = n ^. ckPosition ^. grid
                rows = rowSplit $ compact g
            in foldr f "" rows where
                f ns str = foldr h "" ns ++ "\n" ++ str where
                h 1    s = "X " ++ s
                h (-1) s = "O " ++ s
                h 0    s = "- " ++ s    
                h _    s = "  " ++ s
--}   
 
 

 
 
tryIt :: String
tryIt = format $ rootLabel (getStartNode 1)
 
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
