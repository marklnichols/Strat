module Main where
import TicTac
import Checkers
import StratTree.TreeNode
import StratTree.StratTree
import StratTree.Trees

import StratIO.StratIO

import System.Environment
import System.Exit
import Data.Tree
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens

-- :set args "tictac"
-- :set args "checkers"
-- :set args "chess"
-- Main.main

Data CommandLine = CommandLine

instance Output CommandLine where
    out = putStrLn

main :: IO ()
main = do
    a <- getArgs
    startGame CommandLine a
    return ()

