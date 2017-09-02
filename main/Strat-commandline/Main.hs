module Main where

import Checkers
import GameRunner
import CheckersText
import System.Environment
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
parse ["checkers"]    = GameRunner.startGame CheckersText Checkers.getStartNode
parse ["checkersWeb"] = YesodMain.webInit 
parse _               = YesodMain.webInit -- default to checkers via web   
    


