module Main where

--import TicTac
import Checkers
import GameRunner
import CheckersText
import System.Environment
--import WebMain
import YesodMain

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
parse ["checkers"] = GameRunner.startGame CheckersText Checkers.getStartNode

--parse ["checkersWeb"] = WebMain.init 
parse ["checkersWeb"] = YesodMain.webInit 

--parse ["tictac"]   = GameRunner.startGame  TicTacText TicTac.getStartNode 
--parse ["chess"] =    GameRunner.startGame ChessText Chess.getStartNode

parse _            = putStrLn "Usage: main tictac | checkers | checkersWeb | chess"    
    


