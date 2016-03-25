{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
module StratTree.StratTreeTest (main, aTree, aTree2, modTree, blunderTree, validPathCheck) where

import StratTree.StratTree
import StratTree.Internal.Trees
import StratTree.TreeNode
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Map
import Test.Hspec
import qualified Data.Map as Map
import Data.Tuple.Select

------------------------------------------------------------------------------------------------
-- hspec tests
-------------------------------------------------------------------------------------------------
main = hspec $ do
    describe "best" $ do
        it "calculates the best moves" $ do
            isJust (best aTree 1 1) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 1 1))) `shouldBe` 2
            _followingMoves (fromJust (best aTree 1 1)) `shouldBe` []
            
            isJust (best aTree 1 (-1)) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 1 (-1)))) `shouldBe` 1
            _followingMoves (fromJust (best aTree 1 (-1))) `shouldBe` []
            
            isJust (best aTree 2 1) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 2 1))) `shouldBe` 1
            _followingMoves (fromJust (best aTree 2 1)) `shouldBe` [4]
            
            isJust (best aTree 2 (-1)) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 2 (-1)))) `shouldBe` 1
            _followingMoves (fromJust (best aTree 2 (-1))) `shouldBe` [3]
            
            isJust (best aTree 3 1) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 3 1))) `shouldBe` 1
            _followingMoves (fromJust (best aTree 3 1)) `shouldBe` [3, 8]
            
            isJust (best aTree 3 (-1)) `shouldBe` True
            head (_moveChoices (fromJust (best aTree 3 (-1)))) `shouldBe` 2
            _followingMoves (fromJust (best aTree 3 (-1))) `shouldBe` [5, 12]
            
            isJust (best aTree2 2 (-1)) `shouldBe` True
            head (_moveChoices (fromJust (best aTree2 2 (-1)))) `shouldBe` 3
            _followingMoves (fromJust (best aTree2 2 (-1))) `shouldBe` [10]
            
            isJust (best aTree2 3 1) `shouldBe` True
            head (_moveChoices (fromJust (best aTree2 3 1))) `shouldBe` 3
            _followingMoves (fromJust (best aTree2 3 1)) `shouldBe` [10, 33]
            
            isJust (best aTree2 3 (-1)) `shouldBe` True
            head (_moveChoices (fromJust (best aTree2 3 (-1)))) `shouldBe` 1
            _followingMoves (fromJust (best aTree2 3 (-1))) `shouldBe` [4, 15]
        it "returns a list moves with equivalent scores" $ do
            _moveChoices (fromJust $ best modTree 2 1) `shouldBe` [1, 3, 2]
            _moveChoices (fromJust $ best aTree 3 1) `shouldBe` [1]
    describe "worstReply" $ 
        it "calculates the worst reply given a selected move" $ do
            isJust (worstReply aTree 3 1 1) `shouldBe` True
            head (_moveChoices (fromJust (worstReply aTree 3 1 1))) `shouldBe` 4
            _followingMoves (fromJust (worstReply aTree 3 1 1)) `shouldBe` [10]
            
            isJust (worstReply aTree 3 (-1) 2) `shouldBe` True
            head (_moveChoices (fromJust (worstReply aTree 3 (-1) 2))) `shouldBe` 7
            _followingMoves (fromJust (worstReply aTree 3 (-1) 2)) `shouldBe` [16]
    describe "checkBlunders" $ 
        it "takes a list of equivalent moves, and returns a subset of equivalent move\
           \representing the biggest mistake the oponent can make" $ do
            isJust (checkBlunders blunderTree 3 1 
                [MoveScore {_move=1, _score=10}, MoveScore {_move=2, _score=10}, MoveScore {_move=20, _score=10}]) 
                    `shouldBe` True
            fromJust (checkBlunders blunderTree 3 1 
                [MoveScore {_move=1, _score=10}, MoveScore {_move=2, _score=10}, MoveScore {_move=20, _score=10}]) 
                    `shouldBe` [MoveScore{ _move=2, _score=80}, MoveScore {_move=20, _score=80}]
    describe "getChildren" $ 
            it "gets a list of child nodes" $ do
                fmap (getMove . label) (getChildren $ fromTree aTree) `shouldBe` [1,2]
                fmap (getMove . label) (getChildren $ fromJust $ firstChild $ fromTree aTree2) `shouldBe` [4, 5, 6]
    describe "getSiblings" $ 
            it "gets a list of sibling nodes" $ do
                fmap (getMove . label) (getSiblings $ fromJust $ firstChild $ fromTree aTree) `shouldBe` [1,2]
                fmap (getMove . label) (getSiblings $ fromJust $ firstChild $ fromTree aTree2) `shouldBe` [1, 2, 3]
    describe "childByMove" $ 
        it "finds a child of a tree matching a given move" $             
            case childByMove 1 (fromTree aTree) of 
                Nothing -> -1
                Just x -> getMove $ label x
            `shouldBe` 1
    describe "descendPath" $ 
        it "follows a path of moves to the tree corresponding to the last move in the list" $ do           
            descendPathTest [1, 4, 9] aTree `shouldBe` 5
            descendPathTest [2, 8, 25] aTree2 `shouldBe` 250
    describe "pruneChildrenExcept" $ 
        it "deletes all children except one matching the supplied move" $            
            pruneChildrenExcept aMiniTree 2 `shouldBe` prunedTree 
    describe "processMove" $ 
        it "Prunes the tree of all the children except the one matching the supplied move" $ do
            processMove expandedTree 2 `shouldBe` prunedExpandedTree
            processMove rootOnly 2 `shouldBe` root2
    describe "validPathCheck" $ 
        it "checks to see if the path of moves retured by best is valid and the node at the bottom contains the correct value" $ do
            validPathCheck aTree 1 `shouldBe` True
            validPathCheck aTree (-1) `shouldBe` True
            validPathCheck aTree2 1 `shouldBe` True
            validPathCheck aTree2 (-1) `shouldBe` True
    describe "visitTree" $ 
        it "traverses the tree, potentially modifying nodes" $ 
            visitTree aMiniPosTree 1 testVisitor `shouldBe` modTree
    describe "expandTree" $ 
        it "adds a new level of tree nodes at the specified depth" $ do
            expandTree aMiniPosTree 2 `shouldBe` expandedTree     
            expandTree finalTestTree 2 `shouldBe` expandedFinalTree
    describe "isWorse" $ 
        it "finds the worse of two scores given a margin given the color inquiring" $ do   
            isWorse 50 100 0 1 `shouldBe` False
            isWorse (-100) (-50) 0 1 `shouldBe` False
        
            isWorse 50 100 10 1 `shouldBe` False
            isWorse (-100) (-50) 10 1 `shouldBe` False
            isWorse 95 100 10 1 `shouldBe` False
            isWorse (-100) (-95) 10 1 `shouldBe` False
        
            isWorse 50 100 0 (-1) `shouldBe` True
            isWorse (-100) (-50) 0 (-1) `shouldBe` True
            
            isWorse 50 100 10 (-1) `shouldBe` True
            isWorse (-100) (-50) 10 (-1) `shouldBe` True
            isWorse 95 100 10 (-1) `shouldBe` False
            isWorse (-100) (-95) 10 (-1) `shouldBe` False
        
-----------------------------------------------------------------------
-- hspec support functions
----------------------------------------------------------------------- 
descendPathTest xs tree = case descendPath xs (fromTree tree) of
                                        Nothing -> -1
                                        Just x  -> getValue $ label x

--check that the path of moves retured by best is valid & the node at the bottom contains the correct --value                                        
validPathCheck :: TreeNode t => Tree t -> Int -> Bool
validPathCheck tree color =
        case best tree (-1) color of 
            Nothing -> False
            Just r -> let path = head (_moveChoices r) : _followingMoves r
                          bestValue = _score (head (_moveScores r))
                          mPathBottom = descendPath path (fromTree tree)
                      in case mPathBottom of 
                          Nothing -> False
                          Just tPos -> getValue (label tPos) * color == bestValue
      
testVisitor :: TreePos Full PosTreeItem -> Int -> Int -> TreePos Full PosTreeItem
testVisitor tPos depth max
    | depth == max  = modifyTree addBranch tPos
    | otherwise     = tPos
    
addBranch :: Tree PosTreeItem -> Tree PosTreeItem
addBranch tree = Node (rootLabel tree) [newBranch] 
 
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
    ptColor :: Int,
    ptFinal :: FinalState,
    ptPosition :: TreePosition 
} deriving (Show, Eq)

data TreePosition = TreePosition {
    tts :: [Int] 
} deriving (Show, Eq)    

instance TreeNode TreeItem where
    getMove = move
    getValue = value   

instance TreeNode PosTreeItem where
    getMove = ptMove
    getValue = ptValue     

instance PositionNode PosTreeItem where
    newNode = calcNewNode
    color = ptColor            
    evaluate n = -1    
    possibleMoves = calcPossibleMoves
    final = ptFinal

calcPossibleMoves :: PosTreeItem -> [Int]
calcPossibleMoves node = case ptMove node of
    1 -> [4, 5]
    2 -> [6, 7]
    3 -> [8]
    _ -> []

calcNewNode :: PosTreeItem -> Int -> PosTreeItem
calcNewNode tp mv = fromJust $ Map.lookup mv mvToNode

mvToNode :: Map Int PosTreeItem
mvToNode  = Map.fromList [
    (2, PosTreeItem {ptMove=2, ptValue = 2, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 1, 0]}}),
    (4, PosTreeItem {ptMove=4, ptValue = 4, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 0]}}),
    (5, PosTreeItem {ptMove=5, ptValue = 5, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 1]}}),
    (6, PosTreeItem {ptMove=6, ptValue=6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}}),
    (7, PosTreeItem {ptMove=7, ptValue=7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}}),
    (8, PosTreeItem {ptMove=8, ptValue=8, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 1, 0, 0, 0]}})]

-----------------------------------------------
rootOnly = Node PosTreeItem {ptMove=0, ptValue=0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} []

root2 = Node PosTreeItem {ptMove=2, ptValue=2, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 1, 0]}} []

aMiniTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 1, value = 1} [],
    Node TreeItem {move = 2, value = 2} [],
    Node TreeItem {move = 3, value = 3} []]

newBranch =  Node PosTreeItem {ptMove=4, ptValue=4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} [] 
    
modTree = Node PosTreeItem {ptMove=0, ptValue=0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=1, ptValue=1, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=4, ptValue=4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []],
    Node PosTreeItem {ptMove=2, ptValue=2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=4, ptValue=4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []],
    Node PosTreeItem {ptMove=3, ptValue=3, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=4, ptValue=4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []]]
  
aMiniPosTree = Node PosTreeItem {ptMove=0, ptValue=0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=1, ptValue=1, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=2, ptValue=2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=3, ptValue=3, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]

expandedTree = 
    Node PosTreeItem {ptMove=0, ptValue=0, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=1, ptValue=1, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=4, ptValue=4, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 0]}} [],
            Node PosTreeItem {ptMove=5, ptValue=5, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 1]}} []],
        Node PosTreeItem {ptMove=2, ptValue=2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=6, ptValue=6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
            Node PosTreeItem {ptMove=7, ptValue=7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []],
        Node PosTreeItem {ptMove=3, ptValue=3, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} [ 
            Node PosTreeItem {ptMove=8, ptValue=8, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 1, 0, 0, 0]}} []]]
 
prunedExpandedTree = 
    Node PosTreeItem {ptMove=2, ptValue=2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=6, ptValue=6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
        Node PosTreeItem {ptMove=7, ptValue=7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []]
 
finalTestTree = Node PosTreeItem {ptMove=0, ptValue=0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=1, ptValue=1, ptColor=1, ptFinal=BWins, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=2, ptValue=2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=3, ptValue=3, ptColor=1, ptFinal=WWins, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []] 
 
expandedFinalTree = 
    Node PosTreeItem {ptMove=0, ptValue=0, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=1, ptValue=1, ptColor = 1, ptFinal=BWins, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
        Node PosTreeItem {ptMove=2, ptValue=2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=6, ptValue=6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
            Node PosTreeItem {ptMove=7, ptValue=7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []],
        Node PosTreeItem {ptMove=3, ptValue=3, ptColor = 1, ptFinal=WWins, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]

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

--bestMove applied to blunderTree with depth 3 color 1 is tie of moves 1, 2, and 20 
-- with scores of 10 for each
-- _followingMoves (best blunderTree 3 1) === [1, 2, 20]
--checking worstReply against those three moves should give...
--for move 1, worst = 4, following [10] score = 50
--    move 2, worst = 7, following [15] score = 80
--    move 20, worst = 22, following [26] score = 80
--so checkBlunder would take [1, 2, 20] and return [2, 20]
blunderTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 1, value = -80} [
        Node TreeItem {move = 3, value = 20} [
            Node TreeItem {move = 8, value = 10} []], 
        Node TreeItem {move = 4, value = -40} [
            Node TreeItem {move = 9, value = 5} [], 
            Node TreeItem {move = 10, value = 50} []]], 
    Node TreeItem {move = 2, value = 70} [
        Node TreeItem {move = 5, value = 45} [
            Node TreeItem {move = 11, value = 10} [], 
            Node TreeItem {move = 12, value = -10} []], 
        Node TreeItem {move = 6, value = -60} [
            Node TreeItem {move = 13, value = -20} [], 
            Node TreeItem {move = 14, value = 10} []], 
        Node TreeItem {move = 7, value = 30} [
            Node TreeItem {move = 15, value= 80} [], 
            Node TreeItem {move = 16, value= -90} [], 
            Node TreeItem {move = 17, value = 10} []]],   
    Node TreeItem {move = 20, value = -80} [
        Node TreeItem {move = 21, value = 20} [
            Node TreeItem {move = 23, value = 10} [], 
            Node TreeItem {move = 24, value = 7} []],
        Node TreeItem {move = 22, value = -40} [
            Node TreeItem {move = 25, value = 5} [], 
            Node TreeItem {move = 26, value = 80} []]]]             
            
prunedToChild = Node TreeItem {move = 2, value = 70} [
        Node TreeItem {move = 5, value = 45} [
            Node TreeItem {move = 11, value = 0} [], 
            Node TreeItem {move = 12, value = -10} []], 
        Node TreeItem {move = 6, value = -60} [
            Node TreeItem {move = 13, value = -20} [], 
            Node TreeItem {move = 14, value = 0} []], 
        Node TreeItem {move = 7, value = 30} [
            Node TreeItem {move = 15, value= 80} [], 
            Node TreeItem {move = 16, value= -90} [], 
            Node TreeItem {move = 17, value = 10} []]]

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
