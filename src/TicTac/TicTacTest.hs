{-# LANGUAGE MultiParamTypeClasses #-}
module TicTac.TicTacTest where

import TicTac.TicTac
import TicTac.Internal.TicTac
import Test.Hspec
 
main = hspec $ do
    describe "checkWins" $ do
        it "determines if there is a winner in the position" $ do
            checkWins aPos 1 `shouldBe` True
            checkWins aPos (-1) `shouldBe` False
            checkWins aPos2 1 `shouldBe` False
            checkWins aPos2 (-1) `shouldBe` True
            checkWins aPos3 1 `shouldBe` False
            checkWins aPos3 (-1) `shouldBe` False
    describe "eval" $ do
        it "evaluates the score for a position" $ do
            eval (TTNode 0 0 (TTPosition aPos 1)) `shouldBe` 100
            eval (TTNode 0 0 (TTPosition aPos2 1)) `shouldBe` (-100)
            eval (TTNode 0 0 (TTPosition aPos3 1)) `shouldBe` 0
    describe "calcNewNode" $ do
        it "creates a new node from a previous position and a move" $ do
            _grid (position (calcNewNode (TTNode 1 1 (TTPosition aPos 1)) 5)) `shouldBe` [1, 1, -1, 1, -1, 1, 1, 0, -1]
            _grid (position (calcNewNode (TTNode 1 1 (TTPosition aPos (-1))) (-7))) `shouldBe` [1, 1, -1, 1, -1, 0, 1, -1, -1]
    describe "getPossibleMoves" $ do
        it "gets a list of possible moves from a given position" $ do
            getPossibleMoves (TTNode 0 0 (TTPosition aPos 1)) `shouldBe` [5, 7] 
            getPossibleMoves (TTNode 0 0 (TTPosition aPos (-1))) `shouldBe` [-5, -7]
            getPossibleMoves (TTNode 0 0 (TTPosition aPos2 1)) `shouldBe` [1]
            getPossibleMoves (TTNode 0 0 (TTPosition aPos3 (-1))) `shouldBe` []
         
            
-------------------------------------------------
-- Sample test data
-------------------------------------------------
{--
01 01 -1    -- white wins
01 -1 00
01 00 -1
--}
aPos = [1, 1, -1, 1, -1, 0, 1, 0, -1] :: [Int] 

{--
01 00 -1   -- black wins
01 -1 01
-1 01 -1
--} 
aPos2 = [1, 0, -1, 1, -1, 1, -1, 1, 1] 

{--
01 -1 01    -- draw
01 -1 -1
-1 01 01
--} 
aPos3 = [1, -1, 1, 1, -1, -1, -1, 1, 1]

            