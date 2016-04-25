{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import Test.Hspec
import Data.Tree
import Control.Lens
import Control.Lens.Setter

checkersTest =  
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel getStartNode) `shouldMatchList` [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getPossibleMoves blackFirstStartNode `shouldMatchList` [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves
            
---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------            
blackFirstStartNode :: CkNode
blackFirstStartNode = 
    let node = rootLabel getStartNode
    in node & ckPosition.clr .~ (-1)  -- or set ckPosition.clr (-1) node
    
    
    
    --in pos & clr .~ (-1)    -- or set clr (-1) pos

    
-- let pos = (rootLabel Checkers.getStartNode) ^. ckPosition
-- pos & clr .~ (-1)
