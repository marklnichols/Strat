{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import StratTreeTest
import TicTacTest
import CheckersTest
import ChessTest
import Test.Hspec
--import Data.Tree

main = hspec $ do
    stratTreeTest
    ticTacTest
    checkersTest
    --chessTest
