{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import CheckersTest
import ChessTest
import Test.Hspec
import TicTacTest
import PruningTest
import FenTest

main :: IO ()
main = hspec $ do
    ticTacTest
    checkersTest
    chessTest
    pruningTest
    fenTest
