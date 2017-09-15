{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import CheckersTest
import ChessTest
import StratTreeTest
import Test.Hspec
import TicTacTest

main :: IO ()
main = hspec $ do
    stratTreeTest
    ticTacTest
    checkersTest
    chessTest
