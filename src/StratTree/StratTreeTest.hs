{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
module StratTree.StratTreeTest (main, aTree, aTree2) where

import StratTree.StratTree
import StratTree.Internal.Trees
import StratTree.TreeNode
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Test.Hspec

------------------------------------------------------------------------------------------------
-- hspec tests
-------------------------------------------------------------------------------------------------
main = hspec $ do
    describe "best'" $ do
        it "calculates the best' moves" $ do
            best' aTree 1 1 `shouldBe` [2]
            best' aTree 1 (-1) `shouldBe` [1]
            best' aTree 2 1 `shouldBe` [1, 4]
            best' aTree 2 (-1) `shouldBe` [1, 3]
            best' aTree 3 1 `shouldBe` [1, 3, 8]
            best' aTree 3 (-1) `shouldBe` [2, 5, 12]
            best' aTree2 2 (-1) `shouldBe` [3, 10]
            best' aTree2 3 1 `shouldBe` [3, 10, 33]
            best' aTree2 3 (-1) `shouldBe` [1, 4, 15]
    describe "getChildren" $ do
            it "gets a list of child nodes" $ do
                fmap (\x -> getMove $ label x)(getChildren $ fromTree aTree) `shouldBe` [1,2]
                fmap (\x -> getMove $ label x)(getChildren $ fromJust $ firstChild $ fromTree aTree2) `shouldBe` [4, 5, 6]
    describe "getSiblings" $ do
            it "gets a list of sibling nodes" $ do
                fmap (\x -> getMove $ label x)(getSiblings $ fromJust $ firstChild $ fromTree aTree) `shouldBe` [1,2]
                fmap (\x -> getMove $ label x)(getSiblings $ fromJust $ firstChild $ fromTree aTree2) `shouldBe` [1, 2, 3]
    describe "childByMove" $ do
        it "finds a child of a tree matching a given move" $ do           
            case childByMove 1 (fromTree aTree) of 
                Nothing -> -1
                Just x -> getMove $ label x
            `shouldBe` 1
    describe "descendPath" $ do
        it "follows a path of moves to the tree corresponding to the last move in the list" $ do           
            descendPathTest [1, 4, 9] aTree `shouldBe` 5
            descendPathTest [2, 8, 25] aTree2 `shouldBe` 250
    describe "pruneChildrenExcept" $ do
        it "deletes all children except one matching the supplied move" $ do           
            pruneChildrenExcept aMiniTree 2 `shouldBe` prunedTree 
    describe "validPathCheck" $ do
        it "checks to see if the path of moves retured by best is valid and the node at the bottom contains the correct value" $ do
            validPathCheck aTree 1 `shouldBe` True
            validPathCheck aTree (-1) `shouldBe` True
            validPathCheck aTree2 1 `shouldBe` True
            validPathCheck aTree2 (-1) `shouldBe` True

           
-----------------------------------------------------------------------
-- hspec support functions
----------------------------------------------------------------------- 
descendPathTest xs tree = case descendPath xs (fromTree tree) of
                                        Nothing -> -1
                                        Just x  -> getValue $ label x
                                
validPathCheck tree color =
    let (path, bestValue) = best tree (-1) color
        mPathBottom = descendPath path (fromTree tree) 
    in case mPathBottom of 
        Nothing -> False
        Just tPos -> (getValue $ label tPos) * color == bestValue
      
  
------------------------------------------------------------------------
data TreeItem  = TreeItem { 
    move :: Int,
    value :: Int 
} deriving (Show, Eq)
--isExchanging :: Bool
--pieces :: [Piece] } deriving (Show)

data PosTreeItem  = PosTreeItem { 
    ptMove :: Int,
    ptValue :: Int, 
    ptPosition :: TreePosition 
} deriving (Show, Eq)
data TreePosition = TreePosition {
    tts :: [Int] 
} deriving (Show, Eq)    
      
instance Position TreePosition where 
    evaluate pos = -1
    possibleMoves pos color = []    --nop
    newPosition pos move = calcNewPosition pos move     

calcNewPosition :: TreePosition -> Int -> TreePosition
calcNewPosition pos move = pos  -- nop    
    
instance TreeNode TreeItem where
    getMove = move
    getValue = value   

instance TreeNode PosTreeItem where
    getMove = ptMove
    getValue = ptValue      

instance PositionNode PosTreeItem TreePosition where
    getPosition = ptPosition
  


-----------------------------------------------

aMiniTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 1, value = 1} [],
    Node TreeItem {move = 2, value = 2} [],
    Node TreeItem {move = 3, value = 3} []]
    
aMiniPosTree = Node PosTreeItem {ptMove = 0, ptValue = 0, ptPosition = TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove = 1, ptValue = 1, ptPosition = TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove = 2, ptValue = 2, ptPosition = TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove = 3, ptValue = 3, ptPosition = TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]
    
prunedTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 2, value = 2} []] 
    
aTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 1, value = -80} [
        Node TreeItem {move = 3, value = 20} [
            Node TreeItem {move = 8, value = 10} []], 
        Node TreeItem {move = 4, value = -40} [
            Node TreeItem {move = 9, value = 5} [], 
            Node TreeItem {move = 10, value = 50} []]], 
    Node TreeItem {move = 2, value = 70} [
        Node TreeItem {move = 5, value = 45} [
            Node TreeItem {move = 11, value = 0} [], 
            Node TreeItem {move = 12, value = -10} []], 
        Node TreeItem {move = 6, value = -60} [
            Node TreeItem {move = 13, value = -20} [], 
            Node TreeItem {move = 14, value = 0} []], 
        Node TreeItem {move = 7, value = 30} [
            Node TreeItem {move = 15, value= 80} [], 
            Node TreeItem {move = 16, value= -90} [], 
            Node TreeItem {move = 17, value = 10} []]]] 

aTree2 = Node TreeItem {move = 0, value = 0 } [
    Node TreeItem {move = 1, value = 10 } [
        Node TreeItem {move = 4, value = 40 } [
            Node TreeItem {move = 13, value = -130 } [], 
            Node TreeItem {move = 14, value =  -140 } [],
            Node TreeItem {move = 15, value = -150 } []],
        Node TreeItem {move = 5, value = 50 } [
            Node TreeItem {move = 16, value = -160 } [], 
            Node TreeItem {move = 17, value = -170 } [],
            Node TreeItem {move = 18, value = -180 }[]], 
        Node TreeItem {move = 6, value = 60 } [
            Node TreeItem {move = 19, value =  -190} [],
            Node TreeItem {move = 20, value = -200 } [],
            Node TreeItem {move = 21, value = -210 } []]], 
    Node TreeItem {move = 2, value = -20 } [
        Node TreeItem {move = 7, value = -70 } [
            Node TreeItem {move = 22, value = 220 } [], 
            Node TreeItem {move = 23, value = 230 } [],
            Node TreeItem {move = 24, value = 240 }[]], 
        Node TreeItem {move = 8, value = -80 } [
            Node TreeItem {move = 25, value = 250 } [], 
            Node TreeItem {move = 26, value = 260 } [],
            Node TreeItem {move = 27, value = 270 } []],
        Node TreeItem {move = 9, value = -90 } [
            Node TreeItem {move = 28, value = 280 } [], 
            Node TreeItem {move = 29, value = 290 } [],
            Node TreeItem {move = 30, value = 300 }[]]],
    Node TreeItem {move = 3, value = 30 } [
        Node TreeItem {move = 10, value = -100 } [
            Node TreeItem {move = 31, value = 310 } [],
            Node TreeItem {move = 32, value = 320 } [],
            Node TreeItem {move = 33, value = 330 }[]], 
        Node TreeItem {move = 11, value = -110 } [
            Node TreeItem {move = 34, value = 340 } [],
            Node TreeItem {move = 35, value = 350 } [],
            Node TreeItem {move = 36, value = 360 } []], 
        Node TreeItem {move = 12, value = -120 } [
            Node TreeItem {move = 37, value = 370 } [],
            Node TreeItem {move = 38, value = 380 } [],
            Node TreeItem {move = 39, value = 390 } []]]] 
