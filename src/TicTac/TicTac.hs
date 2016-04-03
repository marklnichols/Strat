{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
--TODO reorganize the exports list -- move those only needed for testing elsewhere
module TicTac.TicTac (calcNewNode, getPossibleMoves, eval, evalGrid, checkWins, checkTwoWayWin, scorePos, format, 
       TTPosition (..), TTNode (..), getStartNode, sums, masks, applyMask) where

import Data.Tree
import StratTree.TreeNode hiding (Result, MoveScore)
import Data.List.Lens
import Control.Lens
import Data.List

------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------
data TTPosition = TTPosition {_grid :: [Int], _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''TTPosition

data TTNode = TTNode {_ttMove :: Int, _ttValue :: Int, _ttErrorValue :: Int, _ttPosition :: TTPosition} deriving (Show)

instance PositionNode TTNode where
    newNode = calcNewNode
    -- evaluate = eval
    -- errorEvaluate = errorEval
    possibleMoves = getPossibleMoves
    color = _clr . _ttPosition
    final = _fin . _ttPosition

instance TreeNode TTNode where
    getMove = _ttMove
    getValue = _ttValue
    getErrorValue = _ttErrorValue
    
---------------------------------------------------------
-- starting position,
---------------------------------------------------------
getStartNode :: Tree TTNode
getStartNode = Node TTNode {_ttMove = -1, _ttValue = 0, _ttErrorValue = 0, _ttPosition = TTPosition 
    {_grid = [0, 0, 0, 0, 0, 0, 0, 0, 0], _clr = 1, _fin = NotFinal}} []

--------------------------------------------------------
-- format position as a string
--------------------------------------------------------
format :: TTPosition -> String
format p =
    let rows = take 3 (_grid p) : take 3 (drop 3 $ _grid p) : [take 3 $ drop 6 $ _grid p]
    in  foldr f "" rows where
        f ns str = foldr g "" ns ++ "\n" ++ str where
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
        gridSet = set (grid . ix (mvToGridIx mv)) val (_ttPosition node)
        oldColor = view clr gridSet
        colorFlipped = set clr (flipColor oldColor) gridSet
        (score, finalSt) = evalGrid $ _grid colorFlipped
        errorScore = errorEvalGrid $ _grid colorFlipped
        allSet = set fin finalSt colorFlipped
    in  TTNode mv score errorScore allSet


----------------------------------------------------------
-- convert from move value to grid index
----------------------------------------------------------
mvToGridIx :: Int -> Int
mvToGridIx mv = abs mv -1     --moves are (+/-) 1-9 vs indexes 0-8 
    
---------------------------------------------------------
-- get list of possible moves from a given position
---------------------------------------------------------
-- TODO extend the use of lens to navigate node -> position ->
getPossibleMoves :: TTNode -> [Int]
getPossibleMoves n =  foldr f [] (zip (_ttPosition n ^. grid) [1..9]) where
    f (x, idx) newList
        | x == 0        = idx * _clr (_ttPosition n) : newList
        | otherwise     = newList

--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: TTNode -> Int
eval n = fst $ evalGrid $ _grid (_ttPosition n)

errorEval :: TTNode -> Int
errorEval n = errorEvalGrid $ _grid (_ttPosition n)
    
evalGrid :: [Int] ->  (Int, FinalState)
evalGrid grid   
    | checkWins grid 1        = (100, WWins)
    | checkWins grid (-1)     = (-100, BWins)
    | checkTwoWayWin grid 1    = (80, NotFinal)
    | checkTwoWayWin grid (-1) = (-80, NotFinal)
    | checkDraw grid          = (0, Draw)
    | otherwise               = (scorePos grid, NotFinal)
    
errorEvalGrid :: [Int] -> Int
errorEvalGrid grid   
    | checkTwoWayWin grid 1    = 121
    | checkTwoWayWin grid (-1) = -121
    | checkWins grid 1        = 101
    | checkWins grid (-1)     = -101
    | checkDraw grid          = 1 
    | otherwise               = 1 -- scorePos grid    
 
---------------------------------------------------------------------
-- Check positions for winning / losing conditions 
---------------------------------------------------------------------
--checkWins :: [grid] -> color -> Bool
checkWins :: [Int] -> Int -> Bool
checkWins pos = wins (sums $ applyMask pos masks) 

checkDraw :: [Int] -> Bool
checkDraw = notElem 0

checkTwoWayWin :: [Int] -> Int -> Bool
checkTwoWayWin pos color = 
    let theSums = sums $ applyMask pos masks
        count = foldr f 0 theSums where
                    f x r = if x == 2*color then r+1 else r
    in count >= 2
    
---------------------------------------------------------------------
-- Additional positions evaluation functions
---------------------------------------------------------------------
scorePos :: [Int] -> Int
scorePos xs = case (countEmpty xs, valCenter xs, sumCorners xs) of
    --(8, _, 1)   -> 20
    --(8, _, -1)  -> -20
    --(7, 1, -1)  -> 10
    --(7, -1, 1)  -> -10
    (_, _, _)   -> 0
    
--------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------    
--hasTwoOf :: [a] -> a -> Bool
--hasTwoOf xs x = length . (filter (\y -> y == x) xs) == 2

wins :: [Int] -> Int -> Bool
wins sums color = (color * 3) `elem` sums  

sums :: [[Int]] -> [Int]
sums = map sum

applyMask :: [Int] -> [[Bool]] -> [[Int]]
applyMask pos = map f where
    f mask = map snd (filter fst (zip mask pos))

masks :: [[Bool]]
masks = map g (modN 3 rhs) ++ map g (divN 3 rhs) ++ map g (modN 4 zeron) ++ map g (modN2AndNot 2  8 zeron)

g :: (Int -> Bool) -> [Bool]
g f = map f offsets

modN :: Int -> [Int] -> [Int -> Bool]
modN n = map (\y x -> x `mod` n == y)

modN2AndNot :: Int -> Int -> [Int] -> [Int -> Bool]
modN2AndNot n1 n2 = map (\y x -> x `mod` n1 == y && x `mod` n2 /= y)

divN :: Int -> [Int] -> [Int -> Bool]
divN n = map (\y x -> x `div` n == y)

offsets = [0, 1..8] :: [Int]
rhs = [0, 1, 2] :: [Int]
zeron = [0] :: [Int]

countEmpty :: [Int] -> Int
countEmpty xs = length $ filter (==0) xs

valCenter :: [Int] -> Int
valCenter xs = xs !! 4

sumCorners :: [Int] -> Int
sumCorners xs = head xs + (xs !! 2) + (xs !! 6) + (xs !! 8)

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
