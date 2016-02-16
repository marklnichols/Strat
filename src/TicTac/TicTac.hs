{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module TicTac.TicTac (calcNewNode, getPossibleMoves, eval, checkWins, format, TTPosition (..), TTNode (..), getStartNode) where

import Data.Tree
import StratTree.TreeNode 
import Data.List.Lens
import Control.Lens
import Data.List

------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------
data TTPosition = TTPosition {_grid :: [Int], _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''TTPosition

data TTNode = TTNode {move :: Int, value :: Int, position :: TTPosition} deriving (Show)

instance PositionNode TTNode where
    newNode = calcNewNode
    evaluate = eval
    possibleMoves = getPossibleMoves
    color = _clr . position
    final = _fin . position

instance TreeNode TTNode where
    getMove = move
    getValue = value
    

---------------------------------------------------------
-- starting position,
---------------------------------------------------------
getStartNode :: Tree TTNode
getStartNode = Node TTNode {move=(-1), value=0, position= TTPosition {_grid = [0, 0, 0, 0, 0, 0, 0, 0, 0], _clr=1, _fin=NotFinal}} []


--------------------------------------------------------
-- format position as a string
--------------------------------------------------------
format :: TTPosition -> String
format p =
    let rows = (take 3 $ _grid p) : (take 3 $ drop 3 $ _grid p) : [take 3 $ drop 6 $ _grid p]
    in  foldr f "" rows where
        f ns str = (foldr g "" ns) ++ "\n" ++ str where
            g 1    s = "X " ++ s
            g (-1) s = "O " ++ s
            g 0    s = "- " ++ s

--------------------------------------------------------
-- calculate new node from a previous node and a move
--------------------------------------------------------
calcNewNode :: TTNode -> Int -> TTNode
calcNewNode node mv =
    let val
            | mv >=0    = 1
            | otherwise = -1
        gridSet = set (grid . ix (abs mv)) val (position node)
        oldColor = view clr gridSet
        colorFlipped = set clr (flipColor oldColor) gridSet
        (score, finalSt) = evalGrid $ _grid colorFlipped
        allSet = set fin finalSt colorFlipped
    in  TTNode mv score allSet
    
---------------------------------------------------------
-- get list of possible moves from a given position
---------------------------------------------------------
-- TODO extend the use of lens to navigate node -> position ->
getPossibleMoves :: TTNode -> [Int]
getPossibleMoves n =  foldr f [] (zip ((position n) ^. grid) [0..9]) where
    f (x, idx) newList
        | x == 0        = (idx) * (_clr (position n)) : newList
        | otherwise     = newList

--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: TTNode -> Int
eval n = fst $ evalGrid $ _grid (position n)

--evalGrid :: grid -> color -> (score, final state)
evalGrid :: [Int] -> (Int, FinalState)
evalGrid  grid   
    | checkWins grid 1    = (100, WWins)
    | checkWins grid (-1) = (-100, BWins)
    | checkDraw grid      = (0, Draw)
    | otherwise           = (0, NotFinal)      
    
---------------------------------------------------------------------
checkWins :: [Int] -> Int -> Bool
checkWins pos color = wins (sums $ applyMask pos masks) color

checkDraw :: [Int] -> Bool
checkDraw xs = not $ elem 0 xs

wins :: [Int] -> Int -> Bool
wins sums color = if elem (color * 3) sums then True else False

sums :: [[Int]] -> [Int]
sums masked = map (\xs -> sum xs) masked

applyMask :: [Int] -> [[Bool]] -> [[Int]]
applyMask pos msks = map f msks where
    f mask = map snd (filter (\(m, p) -> m == True) (zip mask pos))

masks :: [[Bool]]
masks = map g (modN 3 rhs) ++ map g (divN 3 rhs) ++ map g (modN 4 zeron) ++ map g (modN2AndNot 2  8 zeron)

g :: (Int -> Bool) -> [Bool]
g f = map f offsets

modN :: Int -> [Int] -> [Int -> Bool]
modN n values = map (\y x -> x `mod` n == y) values

modN2AndNot :: Int -> Int -> [Int] -> [Int -> Bool]
modN2AndNot n1 n2 values = map (\y x -> x `mod` n1 == y && x `mod` n2 /= y) values

divN :: Int -> [Int] -> [Int -> Bool]
divN n values = map (\y x -> x `div` n == y) values

offsets = [0, 1..8] :: [Int]
rhs = [0, 1, 2] :: [Int]
zeron = [0] :: [Int]


--------------------------------------------
-- Notes / comments
--------------------------------------------
{--
 let offsets = [0, 1..8] :: [Int]

 00 01 02
 03 04 05
 06 07 08

 filter (\n -> n `div` 3 == 0) offsets  -- 0, 1, 2
 filter (\n -> n `div` 3 == 1) offsets  -- 3, 4, 5
 filter (\n -> n `div` 3 == 2) offsets  -- 6, 7, 8

 filter (\n -> n `mod` 3 == 0) offsets  -- 0, 3, 6
 filter (\n -> n `mod` 3 == 1) offsets  -- 1, 4, 7
 filter (\n -> n `mod` 3 == 2) offsets  -- 2, 5, 8

 filter (\n -> n `mod` 4 == 0) offsets  -- 0, 4, 8
 filter (\n -> (n `mod` 2 == 0) && (n `mod` 8 /= 0)) offsets  -- 2, 4, 6
--}


{-- TODO: try -
    x `mod` 3 == 2 :: Int -> Bool

    filterFunct :: Int -> Int -> Int -> Bool
    \n y x -> x `mod` n == y is a filterFunct
    \n y x -> x `div` n == y is a filterFunct
    \n y x -> (x `mod` 2 ==0) && (x `mod` 8 /= 0)) is a filterFunct


    (x `mod` 2 == 0) && (x `mod` 8 /= 0)) :: Int -> Bool

    modNs :: n -> [zeroToTwoValues] -> funct to pass 0-8 to
    modNs :: Int -> [Int] -> [Int -> Bool]
    instead of:
    modNs n values = map (\y x -> x `mod` n == y) values
    how about:
    modNs n values = map (\y x -> filterFunct) values
--}
