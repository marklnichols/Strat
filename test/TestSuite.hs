{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import StratTree.StratTreeTest
import TicTacTest
import CheckersTest
import Test.Hspec
--import Data.Tree

main = hspec $ do
    stratTreeTest
    ticTacTest
    checkersTest
