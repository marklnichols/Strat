{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import CheckersTest
import ChessTest
import Test.Hspec
import TicTacTest

main :: IO ()
main = hspec $ do
    ticTacTest
    checkersTest
    chessTest
