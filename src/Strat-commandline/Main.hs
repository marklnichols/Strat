module Main where

--import TicTac
import Checkers
import GameRunner
import CheckersText
import CheckersWeb
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
 
--TODO add real parser here, make web vs text an option, etc. 
parse :: [String] -> IO ()
--parse ["tictac"]   = GameRunner.startGame  TicTacText TicTac.getStartNode 
parse ["checkers"] = GameRunner.startGame CheckersText Checkers.getStartNode
parse ["checkersWeb"] = GameRunner.startGame CheckersWeb Checkers.getStartNode
--parse ["chess"] =    GameRunner.startGame ChessText Chess.getStartNode
parse _            = putStrLn "Usage: main tictac | checkers | chess"    
    


