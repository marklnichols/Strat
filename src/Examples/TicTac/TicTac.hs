{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
--TODO reorganize the exports list -- move those only needed for testing elsewhere
module TicTac (calcNewNode, getPossibleMoves, eval, evalGrid, checkWins, checkTwoWayWin, scorePos, format,
       TTPosition (..), TTNode (..), getStartNode, sums, masks, applyMask, wins, strToMove) where

import StratTree.TreeNode hiding (Result, MoveScore)
import qualified TTParser as Parser
import Control.Lens
import Data.Tree

------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------
data TTPosition = TTPosition {_grid :: [Int], _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''TTPosition

data TTNode = TTNode {_ttMove :: IntMove, _ttValue :: IntEval, _ttErrorValue :: IntEval, _ttPosition :: TTPosition} deriving (Show)
makeLenses ''TTNode

instance PositionNode TTNode IntMove IntEval where
    newNode = calcNewNode
    possibleMoves = getPossibleMoves
    --color = _clr . _ttPosition
    color n = n ^. (ttPosition . clr)
    --final = _fin . _ttPosition
    final n = n ^. (ttPosition . fin)
    parseMove n s = strToMove s (color n)

instance TreeNode TTNode IntMove IntEval where
    getMove t = t ^. ttMove
    getValue t = t ^. ttValue
    getErrorValue t = t ^. ttErrorValue

---------------------------------------------------------
-- starting position,
---------------------------------------------------------
getStartNode :: Tree TTNode
getStartNode = Node TTNode {_ttMove = IntMove (-1), _ttValue = IntEval 0, _ttErrorValue = IntEval 0, _ttPosition = TTPosition
    {_grid = [0, 0, 0, 0, 0, 0, 0, 0, 0], _clr = 1, _fin = NotFinal}} []

---------------------------------------------------------
-- parse string input to move
---------------------------------------------------------
strToMove :: String -> Int -> Either String IntMove
strToMove str colr =
    let x = Parser.run str
    in case x of
        Left err -> Left err
        Right n -> Right $ IntMove (colr * n)


--------------------------------------------------------
-- format position as a string
--------------------------------------------------------
format :: TTNode -> String
format n =
    let gr = n ^. ttPosition ^. grid
        rows = take 3 gr : take 3 (drop 3 gr) : [take 3 $ drop 6 gr]
    in  foldr f "" rows where
        f ns str = foldr h "" ns ++ "\n" ++ str where
            h 1    s = "X " ++ s
            h (-1) s = "O " ++ s
            h 0    s = "- " ++ s
            h _    s = "na" ++ s    --should never happen

--------------------------------------------------------
-- calculate new node from a previous node and a move
--------------------------------------------------------
calcNewNode :: TTNode -> IntMove -> TTNode
calcNewNode node mv =
    let val
            | theInt mv >= 0   =  1
            | otherwise = -1
        gridSet = set (grid . ix (mvToGridIx mv)) val (node ^. ttPosition)
        oldColor = view clr gridSet
        colorFlipped = set clr (negate oldColor) gridSet
        (scr, finalSt) = evalGrid colorFlipped
        errorScr = errorEvalGrid $ colorFlipped ^. grid
        allSet = set fin finalSt colorFlipped
    in  TTNode mv (IntEval scr) (IntEval errorScr) allSet


----------------------------------------------------------
-- convert from move value to grid index
----------------------------------------------------------
mvToGridIx :: IntMove -> Int
mvToGridIx mv = abs (theInt mv) -1     --moves are (+/-) 1-9 vs indexes 0-8

---------------------------------------------------------
-- get list of possible moves from a given position
---------------------------------------------------------
getPossibleMoves :: TTNode -> [IntMove]
getPossibleMoves n =  foldr f [] (zip (n ^. (ttPosition . grid)) [1..9]) where
    f (x, idx) newList
        | x == 0        = IntMove (idx * (n ^. ttPosition . clr)) : newList
        | otherwise     = newList

--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: TTNode -> Int
eval n = fst $ evalGrid $ n ^. ttPosition

evalGrid :: TTPosition ->  (Int, FinalState)
evalGrid pos
    | checkWins grd 1         = (100, colorToFinalState colr)
    | checkWins grd (-1)      = (-100, colorToFinalState (negate colr))
    | checkTwoWayWin grd 1    = (80, NotFinal)
    | checkTwoWayWin grd (-1) = (-80, NotFinal)
    | checkDraw grd           = (0, Draw)
    | otherwise               = (scorePos grd, NotFinal)
    where grd = pos ^. grid
          colr = pos ^. clr

colorToFinalState :: Int -> FinalState
colorToFinalState 1 = WWins
colorToFinalState _ = BWins


errorEvalGrid :: [Int] -> Int
errorEvalGrid grd
    | checkTwoWayWin grd 1    = 121
    | checkTwoWayWin grd (-1) = -121
    | checkWins grd 1        = 101
    | checkWins grd (-1)     = -101
    | checkDraw grd          = 1
    | otherwise               = 1 -- scorePos grid

---------------------------------------------------------------------
-- Check positions for winning / losing conditions
---------------------------------------------------------------------
checkWins :: [Int] -> Int -> Bool
checkWins pos = wins (sums $ applyMask pos masks)

checkDraw :: [Int] -> Bool
checkDraw = notElem 0

checkTwoWayWin :: [Int] -> Int -> Bool
checkTwoWayWin pos colr =
    let theSums = sums $ applyMask pos masks
        count = foldr f 0 theSums where
                    f :: Int -> Int -> Int
                    f x r = if x == 2*colr then r+1 else r
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
wins :: [Int] -> Int -> Bool
wins xs colr = (colr * 3) `elem` xs

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

offsets :: [Int]
offsets = [0, 1..8]

rhs :: [Int]
rhs = [0, 1, 2]

zeron :: [Int]
zeron = [0]

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
