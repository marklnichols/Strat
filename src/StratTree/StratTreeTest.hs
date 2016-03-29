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
import Control.Monad.Reader

------------------------------------------------------------------------------------------------
-- hspec tests
-------------------------------------------------------------------------------------------------
main = hspec $ do
    describe "best" $ do
        it "calculates the best moves" $ do
            isJust (runReader (best aTree 1) testEnv1) `shouldBe` True 
            head (_moveChoices (fromJust (runReader (best aTree 1) testEnv1))) `shouldBe` 2
            _followingMoves (fromJust (runReader (best aTree 1) testEnv1)) `shouldBe` []
            
            isJust (runReader (best aTree (-1)) testEnv1) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree (-1)) testEnv1))) `shouldBe` 1
            _followingMoves (fromJust (runReader (best aTree (-1)) testEnv1)) `shouldBe` []
            
            isJust (runReader (best aTree 1) testEnv2) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree 1) testEnv2))) `shouldBe` 1
            _followingMoves (fromJust (runReader (best aTree 1) testEnv2)) `shouldBe` [4]
            
            isJust (runReader (best aTree (-1)) testEnv2) `shouldBe`  True
            head (_moveChoices (fromJust (runReader (best aTree (-1)) testEnv2))) `shouldBe` 1
            _followingMoves (fromJust (runReader (best aTree (-1)) testEnv2)) `shouldBe` [3]
            
            isJust (runReader (best aTree 1) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree 1) testEnv3))) `shouldBe` 1
            _followingMoves (fromJust (runReader (best aTree 1) testEnv3)) `shouldBe` [3, 8]
            
            isJust (runReader (best aTree (-1)) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree (-1)) testEnv3))) `shouldBe` 2
            _followingMoves (fromJust (runReader (best aTree (-1)) testEnv3)) `shouldBe` [5, 12]
            
            isJust (runReader (best aTree2 (-1)) testEnv2) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree2 (-1)) testEnv2))) `shouldBe` 3
            _followingMoves (fromJust (runReader (best aTree2 (-1)) testEnv2)) `shouldBe` [10]
            
            isJust (runReader (best aTree2 1) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree2 1) testEnv3))) `shouldBe` 3
            _followingMoves (fromJust (runReader (best aTree2 1) testEnv3)) `shouldBe` [10, 33]
            
            isJust (runReader (best aTree2 (-1)) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (runReader (best aTree2 (-1)) testEnv3))) `shouldBe` 1
            _followingMoves (fromJust (runReader (best aTree2 (-1)) testEnv3)) `shouldBe` [4, 15]
        it "returns a list moves with equivalent scores" $ do
            _moveChoices (fromJust (runReader (best modTree 1) testEnv2)) `shouldBe` [1, 3, 2]
            _moveChoices (fromJust (runReader (best aTree 1) testEnv2)) `shouldBe` [1]
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
            isJust (runReader (checkBlunders blunderTree 1 
                [MoveScore {_move=1, _score=10}, MoveScore {_move=2, _score=10}, MoveScore {_move=20, _score=10}]) 
                    testEnv3) `shouldBe` True
            fromJust (runReader (checkBlunders blunderTree 1 
                [MoveScore {_move=1, _score=10}, MoveScore {_move=2, _score=10}, MoveScore {_move=20, _score=10}]) 
                    testEnv3) `shouldBe` [MoveScore{ _move=2, _score=80}, MoveScore {_move=20, _score=80}]
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
            runReader (expandTree aMiniPosTree) testEnv2 `shouldBe` expandedTree     
            runReader (expandTree finalTestTree) testEnv2 `shouldBe` expandedFinalTree
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
        --case best tree (-1) color of
        case runReader (best tree color) testEnvMax of     
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
    _tiMove :: Int,
    _tiValue :: Int 
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
    getMove = _tiMove
    getValue = _tiValue   

instance TreeNode PosTreeItem where
    getMove = ptMove
    getValue = ptValue     

instance PositionNode PosTreeItem where
    newNode = calcNewNode
    color = ptColor            
    evaluate n = -1    
    errorEvaluate n = -1
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
testEnv1 = Env {_depth =1, _errorDepth = 1, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}
     
testEnv2 = Env {_depth = 2, _errorDepth = 2, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}

testEnv3 = Env {_depth = 3, _errorDepth = 3, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}

testEnvMax = Env {_depth = -1, _errorDepth = -1, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}     
     
rootOnly = Node PosTreeItem {ptMove=0, ptValue=0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} []

root2 = Node PosTreeItem {ptMove=2, ptValue=2, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 1, 0]}} []

aMiniTree = Node TreeItem {_tiMove = 0, _tiValue = 0} [
    Node TreeItem {_tiMove = 1, _tiValue = 1} [],
    Node TreeItem {_tiMove = 2, _tiValue = 2} [],
    Node TreeItem {_tiMove = 3, _tiValue = 3} []]

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

prunedTree = Node TreeItem {_tiMove = 0, _tiValue = 0} [
    Node TreeItem {_tiMove = 2, _tiValue = 2} []] 
    
aTree = Node TreeItem {_tiMove = 0, _tiValue = 0} [
    Node TreeItem {_tiMove = 1, _tiValue = -80} [
        Node TreeItem {_tiMove = 3, _tiValue = 20} [
            Node TreeItem {_tiMove = 8, _tiValue = 10} []], 
        Node TreeItem {_tiMove = 4, _tiValue = -40} [
            Node TreeItem {_tiMove = 9, _tiValue = 5} [], 
            Node TreeItem {_tiMove = 10, _tiValue = 50} []]], 
    Node TreeItem {_tiMove = 2, _tiValue = 70} [
        Node TreeItem {_tiMove = 5, _tiValue = 45} [
            Node TreeItem {_tiMove = 11, _tiValue = 0} [], 
            Node TreeItem {_tiMove = 12, _tiValue = -10} []], 
        Node TreeItem {_tiMove = 6, _tiValue = -60} [
            Node TreeItem {_tiMove = 13, _tiValue = -20} [], 
            Node TreeItem {_tiMove = 14, _tiValue = 0} []], 
        Node TreeItem {_tiMove = 7, _tiValue = 30} [
            Node TreeItem {_tiMove = 15, _tiValue= 80} [], 
            Node TreeItem {_tiMove = 16, _tiValue= -90} [], 
            Node TreeItem {_tiMove = 17, _tiValue = 10} []]]]   

blunderTree = Node TreeItem {_tiMove = 0, _tiValue = 0} [
    Node TreeItem {_tiMove = 1, _tiValue = -80} [
        Node TreeItem {_tiMove = 3, _tiValue = 20} [
            Node TreeItem {_tiMove = 8, _tiValue = 10} []], 
        Node TreeItem {_tiMove = 4, _tiValue = -40} [
            Node TreeItem {_tiMove = 9, _tiValue = 5} [], 
            Node TreeItem {_tiMove = 10, _tiValue = 50} []]], 
    Node TreeItem {_tiMove = 2, _tiValue = 70} [
        Node TreeItem {_tiMove = 5, _tiValue = 45} [
            Node TreeItem {_tiMove = 11, _tiValue = 10} [], 
            Node TreeItem {_tiMove = 12, _tiValue = -10} []], 
        Node TreeItem {_tiMove = 6, _tiValue = -60} [
            Node TreeItem {_tiMove = 13, _tiValue = -20} [], 
            Node TreeItem {_tiMove = 14, _tiValue = 10} []], 
        Node TreeItem {_tiMove = 7, _tiValue = 30} [
            Node TreeItem {_tiMove = 15, _tiValue= 80} [], 
            Node TreeItem {_tiMove = 16, _tiValue= -90} [], 
            Node TreeItem {_tiMove = 17, _tiValue = 10} []]],   
    Node TreeItem {_tiMove = 20, _tiValue = -80} [
        Node TreeItem {_tiMove = 21, _tiValue = 20} [
            Node TreeItem {_tiMove = 23, _tiValue = 10} [], 
            Node TreeItem {_tiMove = 24, _tiValue = 7} []],
        Node TreeItem {_tiMove = 22, _tiValue = -40} [
            Node TreeItem {_tiMove = 25, _tiValue = 5} [], 
            Node TreeItem {_tiMove = 26, _tiValue = 80} []]]]             
            
prunedToChild = Node TreeItem {_tiMove = 2, _tiValue = 70} [
        Node TreeItem {_tiMove = 5, _tiValue = 45} [
            Node TreeItem {_tiMove = 11, _tiValue = 0} [], 
            Node TreeItem {_tiMove = 12, _tiValue = -10} []], 
        Node TreeItem {_tiMove = 6, _tiValue = -60} [
            Node TreeItem {_tiMove = 13, _tiValue = -20} [], 
            Node TreeItem {_tiMove = 14, _tiValue = 0} []], 
        Node TreeItem {_tiMove = 7, _tiValue = 30} [
            Node TreeItem {_tiMove = 15, _tiValue= 80} [], 
            Node TreeItem {_tiMove = 16, _tiValue= -90} [], 
            Node TreeItem {_tiMove = 17, _tiValue = 10} []]]

aTree2 = Node TreeItem {_tiMove = 0, _tiValue = 0 } [
    Node TreeItem {_tiMove = 1, _tiValue = 10 } [
        Node TreeItem {_tiMove = 4, _tiValue = 40 } [
            Node TreeItem {_tiMove = 13, _tiValue = -130 } [], 
            Node TreeItem {_tiMove = 14, _tiValue =  -140 } [],
            Node TreeItem {_tiMove = 15, _tiValue = -150 } []],
        Node TreeItem {_tiMove = 5, _tiValue = 50 } [
            Node TreeItem {_tiMove = 16, _tiValue = -160 } [], 
            Node TreeItem {_tiMove = 17, _tiValue = -170 } [],
            Node TreeItem {_tiMove = 18, _tiValue = -180 }[]], 
        Node TreeItem {_tiMove = 6, _tiValue = 60 } [
            Node TreeItem {_tiMove = 19, _tiValue =  -190} [],
            Node TreeItem {_tiMove = 20, _tiValue = -200 } [],
            Node TreeItem {_tiMove = 21, _tiValue = -210 } []]], 
    Node TreeItem {_tiMove = 2, _tiValue = -20 } [
        Node TreeItem {_tiMove = 7, _tiValue = -70 } [
            Node TreeItem {_tiMove = 22, _tiValue = 220 } [], 
            Node TreeItem {_tiMove = 23, _tiValue = 230 } [],
            Node TreeItem {_tiMove = 24, _tiValue = 240 }[]], 
        Node TreeItem {_tiMove = 8, _tiValue = -80 } [
            Node TreeItem {_tiMove = 25, _tiValue = 250 } [], 
            Node TreeItem {_tiMove = 26, _tiValue = 260 } [],
            Node TreeItem {_tiMove = 27, _tiValue = 270 } []],
        Node TreeItem {_tiMove = 9, _tiValue = -90 } [
            Node TreeItem {_tiMove = 28, _tiValue = 280 } [], 
            Node TreeItem {_tiMove = 29, _tiValue = 290 } [],
            Node TreeItem {_tiMove = 30, _tiValue = 300 }[]]],
    Node TreeItem {_tiMove = 3, _tiValue = 30 } [
        Node TreeItem {_tiMove = 10, _tiValue = -100 } [
            Node TreeItem {_tiMove = 31, _tiValue = 310 } [],
            Node TreeItem {_tiMove = 32, _tiValue = 320 } [],
            Node TreeItem {_tiMove = 33, _tiValue = 330 }[]], 
        Node TreeItem {_tiMove = 11, _tiValue = -110 } [
            Node TreeItem {_tiMove = 34, _tiValue = 340 } [],
            Node TreeItem {_tiMove = 35, _tiValue = 350 } [],
            Node TreeItem {_tiMove = 36, _tiValue = 360 } []], 
        Node TreeItem {_tiMove = 12, _tiValue = -120 } [
            Node TreeItem {_tiMove = 37, _tiValue = 370 } [],
            Node TreeItem {_tiMove = 38, _tiValue = 380 } [],
            Node TreeItem {_tiMove = 39, _tiValue = 390 } []]]] 
