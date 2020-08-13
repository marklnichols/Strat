{-# LANGUAGE MultiParamTypeClasses #-}
module ChessText
    ( ChessText(..)
    ) where

import Chess
import Control.Lens
import Data.List.Extra
import Data.Tree
-- import Debug.Trace
import Strat.StratTree
import Strat.StratTree.TreeNode
import Strat.StratTree.Trees
import System.Exit
import qualified Data.Vector.Unboxed as V

data ChessText = ChessText

instance Output ChessText ChessNode ChessMove ChessEval where
    out _ = printString
    updateBoard = showBoard
    showCompMove _ = printMoveChoiceInfo
    getPlayerMove _ = playerMove
    gameError = exitFail

printString :: String -> IO ()
printString = putStrLn

showBoard :: ChessText -> ChessNode -> IO ()
showBoard _ node = do
    putStrLn $ formatBoard node
    putStrLn ("Current position score: " ++ show (getValue node))
    putStrLn ""

printMoveChoiceInfo :: Tree ChessNode -> [MoveScore ChessMove ChessEval]
  -> Result ChessMove ChessEval ->  ChessMove -> IO ()
printMoveChoiceInfo tree finalChoices result mv = do
    putStrLn ("Choices from best: " ++ show (result^.moveScores))
    putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
    putStrLn ("Tree size: " ++ show (treeSize tree))
    putStrLn ("Equivalent best moves: " ++ show (result^.moveChoices))
    putStrLn ("Following moves: " ++ show ( result^.followingMoves))
    putStrLn ("Computer's move:\n (m:" ++ show mv ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    putStrLn ""

exitFail :: ChessText -> String -> IO ()
exitFail _ s = do
    putStrLn s
    exitFailure

---------------------------------------------------------------------------------------------------
-- Get player move, parsed from text input
---------------------------------------------------------------------------------------------------
playerMove :: Tree ChessNode -> Int -> IO ChessMove
playerMove tree turn = do
    putStrLn ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    putStrLn ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            putStrLn err
            playerMove tree turn
        Right mv ->
            if not (isLegal tree mv)
                then do
                    putStrLn "Not a legal move."
                    playerMove tree turn
                else return mv

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
formatBoard :: ChessNode -> String
formatBoard node =
    let g = node ^. (chessPos . cpGrid)
    in (colLabels ++ "\n") ++ loop g 11 8 ""
  where
    loop :: V.Vector Char -> Int -> Int -> String -> String
    loop _src _nDrop 0 dest = dest
    loop src nDrop rows dest =
      let newHead = V.drop nDrop src
          pieces = padChars $ V.toList $ V.take 8 newHead
          newDest = "\n" ++ show (8 - rows + 1) ++ "  " ++ pieces ++ dest
      in loop newHead 10 (rows - 1) newDest

padChars :: String -> String
padChars src =
    concat $ repeatedly f src
  where
    f [] = ([], [])
    f (x:xs) = (x : "  ", xs)

colLabels :: String
colLabels = "   A  B  C  D  E  F  G  H"
