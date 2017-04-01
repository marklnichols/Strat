{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import StratTreeTest
import TicTacTest
import CheckersTest
import ChessTest
import Test.Hspec

main :: IO ()
main = hspec $ do
    stratTreeTest
    ticTacTest
    checkersTest
    chessTest
