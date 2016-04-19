{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import Test.Hspec
import Data.Tree

checkersTest =  
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel (getStartNode 1)) `shouldBe` [19, 20, 21, 22] --white moves, white pieces at bottom
            getPossibleMoves (rootLabel (getStartNode (-1))) `shouldBe` [23, 24, 25, 26] --white moves, black pieces at bottom