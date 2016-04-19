{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import CheckersTest
import TicTac.TicTacTest
import Test.Hspec
--import Data.Tree

main = hspec $ do
    checkersTest
    ticTacTest
    
    
