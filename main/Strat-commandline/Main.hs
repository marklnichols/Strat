module Main where

import Checkers
import GameRunner
import CheckersText
import System.Environment
import StratWeb.YesodMain

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
parse ["checkers"]    = GameRunner.startGame CheckersText Checkers.getStartNode
parse ["checkersWeb"] = webInit 
parse _               = webInit -- default to checkers via web   
    


