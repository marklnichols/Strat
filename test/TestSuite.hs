{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import CheckersTest
import StratTree.StratTreeTest
import TicTac.TicTacTest
import Test.Hspec
--import Data.Tree

main = hspec $ do
    stratTreeTest
    ticTacTest
    checkersTest
    
