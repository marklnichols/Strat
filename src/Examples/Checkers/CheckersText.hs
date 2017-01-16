{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersText where

import StratTree.TreeNode
import StratTree.StratTree
import StratTree.Trees
import Data.Tree
import Data.List
import System.Exit
import Control.Lens
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map
import Checkers


data CheckersText = CheckersText

instance Output CheckersText CkNode CkMove CkEval where
    out _ = printString
    updateBoard = showBoard
    showCompMove _ = printMoveChoiceInfo
    getPlayerMove _ = playerMove
    gameError = exitFail

-- class Output o n m | o -> n, n -> m where 
-- showCompMove :: o -> Tree n -> Result m e -> m -> IO ()    
    
printString :: String -> IO ()
printString = putStrLn
    
showBoard :: CheckersText -> CkNode -> IO ()
showBoard _ node = do  
    putStrLn $ formatBoard node 
    putStrLn ("Current position score: " ++ show (getValue node))
    putStrLn ""
 
printMoveChoiceInfo :: Tree CkNode -> [MoveScore CkMove CkEval] -> Result CkMove CkEval ->  CkMove -> IO ()
printMoveChoiceInfo tree finalChoices result mv = do
    putStrLn ("Choices from best: " ++ show (result^.moveScores))
    putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
    putStrLn ("Tree size: " ++ show (treeSize tree))
    putStrLn ("Equivalent best moves: " ++ show (result^.moveChoices))
    putStrLn ("Following moves: " ++ show ( result^.followingMoves))
    putStrLn ("Computer's move:\n (m:" ++ show mv ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    putStrLn ""

 
exitFail :: CheckersText -> String -> IO ()
exitFail _ s = do
    putStrLn s
    exitFailure
    
---------------------------------------------------------------------------------------------------
-- Get player move, parsed from text input
---------------------------------------------------------------------------------------------------
playerMove :: Tree CkNode -> Int -> IO CkMove
playerMove tree turn = do
    putStrLn ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    putStrLn ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            putStrLn err
            playerMove tree turn
        Right mv -> if not (isLegal tree mv)
                        then do
                            putStrLn "Not a legal move."
                            playerMove tree turn
                        else return mv                         
   
---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
formatBoard :: CkNode -> String
formatBoard node = loop (node^.ckPosition^.grid) 40 "" where
    loop _ 4 result = result ++ "\n" ++ colLabels 
    loop xs n result = loop xs (newIdx - 4) (result ++ rowToStr xs newIdx spaces) where
        (newIdx, spaces) = case n `mod` 9 of
            0 -> (n-1, "")
            4 -> (n, "   ")
            _ -> (n, "   ") --should never happen


rowToStr :: V.Vector Int -> Int -> String -> String
rowToStr xs i spaces =  Map.findWithDefault "??" i labelMap ++ "  " ++ spaces ++
                            toXOs (xs V.! (i-3)) ++ gap ++
                            toXOs (xs V.! (i-2)) ++ gap ++
                            toXOs (xs V.! (i-1)) ++ gap ++
                            toXOs (xs V.!    i)  ++ "\n"

gap :: String
gap = "     "

toXOs :: Int -> String
toXOs 1 = "x"
toXOs (-1) = "o"
toXOs (2) = "X"
toXOs (-2) = "O"
toXOs 0 = "-"
toXOs _ = "?"

labelMap :: Map.Map Int String
labelMap = Map.fromList [(40, "H"), (35, "G"), (31, "F"), (26, "E"), (22, "D"), (17, "C"), (13, "B"), (8, "A")]

colLabels :: String
colLabels = "   " ++ intercalate "  " ["1", "2", "3", "4", "5", "6", "7", "8"]