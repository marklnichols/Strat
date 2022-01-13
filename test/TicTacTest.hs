{-# LANGUAGE MultiParamTypeClasses #-}
module TicTacTest (ticTacTest) where

import Strat.StratTree.TreeNode
import Test.Hspec
import TicTac.TTGame

ticTacTest :: SpecWith ()
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
            eval (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos 1 NotFinal)) `shouldBe` 100
            eval (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos2 1 NotFinal)) `shouldBe` (-100)
            eval (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos3 1 NotFinal)) `shouldBe` 0
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
            _grid (_ttPosition (calcNewNode (TTNode (IntMove 1) (IntEval 1) (TTPosition aPos 1 NotFinal)) (IntMove 6))) `shouldBe` [1, 1, -1, 1, -1, 1, 1, 0, -1]
            _grid (_ttPosition (calcNewNode (TTNode (IntMove 1) (IntEval 1) (TTPosition aPos (-1) NotFinal)) (IntMove(-8)))) `shouldBe` [1, 1, -1, 1, -1, 0, 1, -1, -1]
    describe "getPossibleMoves" $
        it "gets a list of possible moves from a given position" $ do
            getPossibleMoves (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos 1 NotFinal)) `shouldBe` [IntMove 6, IntMove 8]
            getPossibleMoves (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos (-1) NotFinal)) `shouldBe` [IntMove (-6), IntMove (-8)]
            getPossibleMoves (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos2 1 NotFinal)) `shouldBe` [IntMove 2]
            getPossibleMoves (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos3 (-1) NotFinal)) `shouldBe` []
    describe "format" $
        it "formats a position as a string for display" $
            format (TTNode (IntMove 0) (IntEval 0) (TTPosition aPos 1 NotFinal)) `shouldBe` "X X O \nX O - \nX - O \n"
    describe "strToMove" $
        it "parses move input into an IntMove" $ do
            strToMove "2" 1 `shouldBe` Right (MoveEntry (IntMove 2))
            strToMove "3" (-1) `shouldBe` Right (MoveEntry (IntMove (-3)))

-------------------------------------------------
-- Sample test data
-------------------------------------------------
{--
01 01 -1    -- white wins
01 -1 00
01 00 -1
--}
aPos :: [Int]
aPos = [1, 1, -1, 1, -1, 0, 1, 0, -1] :: [Int]

{--
01 00 -1   -- black wins
01 -1 01
-1 01 -1
--}
aPos2 :: [Int]
aPos2 = [1, 0, -1, 1, -1, 1, -1, 1, 1]

{--
01 -1 01    -- draw
01 -1 -1
-1 01 01
--}
aPos3 :: [Int]
aPos3 = [1, -1, 1, 1, -1, -1, -1, 1, 1]

{--
01 01 00    -- two-way win for +1
01 -1 00
00 00 -1
--}
aPos8 :: [Int]
aPos8 = [1, 1, 0, 1, -1, 0, 0, 0, -1]

{--
-1 -1 00    -- two-way win for -1 (three way, actually)
01 -1 00
01 00 00
--}
aPos9 :: [Int]
aPos9 = [-1, -1, 0, 0, -1, 0, 0, 0, 0]

{--
-1 00 01    -- two-way win for +1
-1 00 00
01 00 01
--}
aPos10 :: [Int]
aPos10 = [-1, 0, 1, -1, 0, 0, 1, 0, 1]

{--
00 00 00    -- two-way win for -1 (three way, actualy)
00 -1 -1
01 01 -1
--}
aPos11 :: [Int]
aPos11 = [0, -0, 0, 0, -1, -1, 1, 1, -1]
