{-# LANGUAGE MultiParamTypeClasses #-}
module TicTac.TicTacTest where

import StratTree.TreeNode
import TicTac.TicTac
import Test.Hspec
 
-- main = hspec $ do
ticTacTest = do
    describe "checkWins" $
        it "determines if there is a winner in the position" $ do
            checkWins aPos 1 `shouldBe` True
            checkWins aPos (-1) `shouldBe` False
            checkWins aPos2 1 `shouldBe` False
            checkWins aPos2 (-1) `shouldBe` True
            checkWins aPos3 1 `shouldBe` False
            checkWins aPos3 (-1) `shouldBe` False
    describe "eval" $
        it "evaluates the score for a position" $ do
            eval (TTNode 0 0 0 (TTPosition aPos 1 NotFinal)) `shouldBe` 100
            eval (TTNode 0 0 0 (TTPosition aPos2 1 NotFinal)) `shouldBe` (-100)
            eval (TTNode 0 0 0 (TTPosition aPos3 1 NotFinal)) `shouldBe` 0
            --eval (TTNode 0 0 (TTPosition aPos4 1 NotFinal)) `shouldBe` 20
            --eval (TTNode 0 0 (TTPosition aPos5 1 NotFinal)) `shouldBe` (-20)
            --eval (TTNode 0 0 (TTPosition aPos6 1 NotFinal)) `shouldBe` 10
            --eval (TTNode 0 0 (TTPosition aPos7 1 NotFinal)) `shouldBe` (-10)
    describe "checkTwoWayWin" $
        it "Checks whether a position contains two in a row in two different places." $ do        
            checkTwoWayWin aPos3 1 `shouldBe` False
            checkTwoWayWin aPos3 (-1) `shouldBe` False
            checkTwoWayWin aPos8 1 `shouldBe` True
            
            checkTwoWayWin aPos8 (-1) `shouldBe` False
            
            checkTwoWayWin aPos9 (-1) `shouldBe` True
            checkTwoWayWin aPos9 1 `shouldBe` False
            checkTwoWayWin aPos10 1 `shouldBe` True
            checkTwoWayWin aPos10 (-1) `shouldBe` False
            checkTwoWayWin aPos11 (-1) `shouldBe` True
            checkTwoWayWin aPos11 1 `shouldBe` False
    describe "calcNewNode" $
        it "creates a new node from a previous position and a move" $ do
            _grid (_ttPosition (calcNewNode (TTNode 1 1 1 (TTPosition aPos 1 NotFinal)) 6)) `shouldBe` [1, 1, -1, 1, -1, 1, 1, 0, -1]
            _grid (_ttPosition (calcNewNode (TTNode 1 1 1 (TTPosition aPos (-1) NotFinal)) (-8))) `shouldBe` [1, 1, -1, 1, -1, 0, 1, -1, -1]
    describe "getPossibleMoves" $
        it "gets a list of possible moves from a given position" $ do
            getPossibleMoves (TTNode 0 0 0 (TTPosition aPos 1 NotFinal)) `shouldBe` [6, 8] 
            getPossibleMoves (TTNode 0 0 0 (TTPosition aPos (-1) NotFinal)) `shouldBe` [-6, -8]
            getPossibleMoves (TTNode 0 0 0 (TTPosition aPos2 1 NotFinal)) `shouldBe` [2]
            getPossibleMoves (TTNode 0 0 0 (TTPosition aPos3 (-1) NotFinal)) `shouldBe` []
    describe "format" $
        it "formats a position as a string for display" $
            format (TTNode 0 0 0 (TTPosition aPos 1 NotFinal)) `shouldBe` "X X O \nX O - \nX - O \n"      
                      
-------------------------------------------------
-- Sample test data
-------------------------------------------------
{--
00 00 00    -- empty board
00 00 00
00 00 00
--}
aPos0 = [0, 0, 0, 0, 0, 0, 0, 0, 0] :: [Int] 
ttNode0 = TTNode 0 0 0 (TTPosition aPos0 1 NotFinal)

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

{--
00 00 01    -- +20
00 00 00
00 00 00 
--}
aPos4 = [0, 0, 1, 0, 0, 0, 0, 0, 0]

{--
00 00 00    -- -20
00 00 00
-1 00 00 
--}
aPos5 = [0, 0, 0, 0, 0, 0, -1, 0, 0]    

{--
-1 00 00    -- +10
00 01 00
00 00 00 
--}
aPos6 = [-1, 0, 0, 0, 1, 0, 0, 0, 0]        

{--
00 00 00    -- -10
00 -1 00
00 00 01 
--}
aPos7 = [0, 0, 0, 0, -1, 0, 0, 0, 1]



{--
01 01 00    -- two-way win for +1 
01 -1 00
00 00 -1 
--}
aPos8 = [1, 1, 0, 1, -1, 0, 0, 0, -1]   

{--
-1 -1 00    -- two-way win for -1 (three way, actually)
01 -1 00
01 00 00 
--}
aPos9 = [-1, -1, 0, 0, -1, 0, 0, 0, 0]

{--
-1 00 01    -- two-way win for +1
-1 00 00
01 00 01 
--}
aPos10 = [-1, 0, 1, -1, 0, 0, 1, 0, 1]

{--
00 00 00    -- two-way win for -1 (three way, actualy)
00 -1 -1
01 01 -1 
--}
aPos11 = [0, -0, 0, 0, -1, -1, 1, 1, -1]