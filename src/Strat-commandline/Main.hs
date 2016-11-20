module Main where

import TicTac
import Checkers
import GameRunner
import CheckersText
import System.Environment

-- :set args "tictac"
-- :set args "checkers"
-- :set args "chess"
-- Main.main

main :: IO ()
main = do
    a <- getArgs
    parse a
    return ()
    
parse :: [String] -> IO ()
--parse ["tictac"]   = GameRunner.startGame  TicTacText TicTac.getStartNode 
parse ["checkers"] = GameRunner.startGame CheckersText Checkers.getStartNode
--parse ["chess"] =    GameRunner.startGame ChessText Chess.getStartNode
parse _            = putStrLn "Usage: main tictac | checkers | chess"    
    


