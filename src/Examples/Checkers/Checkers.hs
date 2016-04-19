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
    color = view (ckPosition . clr)
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
{-- how indexes relate to board position (indexes in parens are not displayed):
          
   (41) (42) (43) (44) (45)    
   
      37   38   39   40                          
    32   33   34   35      (36)        
      28   29   30   31                                
    23   24   25   26      (27)               
      19   20   21   22                              
    14   15   16   17      (18)              
      10   11   12   13                              
    05   06   07   08      (09)         
                                                     
   (00) (01) (02) (03) (04)    
--}                             

offBoard :: [Int]
offBoard = [0, 1, 2, 3, 4, 9, 18, 27, 36, 41, 42, 43, 44, 45]
---------------------------------------------------------------------------------------------------                                                                                    
mkStartGrid :: Int -> [Int] 
mkStartGrid bottomColor =  fmap (indexToValue bottomColor) [0..45]

indexToValue :: Int -> Int -> Int
indexToValue bottomColor idx 
    | idx < 5               = 99           -- off the edge
    | idx > 40              = 99           -- off the edge
    | idx `mod` 9 == 0      = 99           -- off the edge
    | idx > 18 && idx < 27  = 0            -- center, no initial pieces 
    | idx < 18              = bottomColor  -- player at bottom of board initial pieces
    | otherwise             = negate bottomColor -- player at top initial pieces
  
---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
format :: CkNode -> String
format node =   let xs = node ^. ckPosition ^. grid
                in loop xs 5 "" where
                    loop :: [Int] -> Int -> String -> String
                    loop xs 41 result = result
                    loop xs n result =  let (newIdx, spaces) = case (n `mod` 9) of
                                                                   0 -> (n + 1, " ") 
                                                                   5 -> (n, "")
                                        in loop xs (newIdx + 4) (result ++ rowToStr xs newIdx spaces)
       
                          
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
 
---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
--TODO: implement
calcNewNode :: CkNode -> Int -> CkNode
calcNewNode node mv = node

---------------------------------------------------------------------------------------------------
-- get list of possible moves from a given position
---------------------------------------------------------------------------------------------------
getPossibleMoves :: CkNode -> [Int]
getPossibleMoves n = foldr f [] (getPieceLocs n) where
                        f x r = r ++ pieceMoves n x

getPieceLocs :: CkNode -> [Int]
getPieceLocs node = 
    let pos = node ^. ckPosition
        c = pos ^. clr
    in filter (pMatch c) (pos ^. grid)
        where pMatch color piece = 
                  let av = abs piece 
                  in if av > 0 && av <3 && (piece * color) >0 then True else False

pieceMoves :: CkNode -> Int -> [Int]
pieceMoves node idx = 
    let pos = node ^. ckPosition
        g = pos ^. grid
        c = pos ^. clr
    in case (g ^? ix idx) of
        Nothing -> []
        Just p  -> if isKing p then kingMoves idx else forwardMoves idx c 

isKing :: Int -> Bool
isKing move = (abs move) > 1

possibleJumps :: CkNode -> Int -> [Int]
possibleJumps n idx =[8]

getJumps :: CkNode -> [Int]
getJumps n = [7]
---------------------------------------------------------------------------------------------------
-- diagonal moves on the board
---------------------------------------------------------------------------------------------------
forwardMoves :: Int -> Int -> [Int]
forwardMoves idx color = filter (\x -> x `notElem` offBoard) [idx * (color) + 4, idx * (color) +5]

kingMoves :: Int -> [Int]
kingMoves idx = forwardMoves idx (-1) ++ forwardMoves idx 1

