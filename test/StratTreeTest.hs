{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module StratTreeTest (stratTreeTest) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper
import Strat.StratTree
import Strat.StratTree.TreeNode
import Strat.StratTree.Trees
import Test.Hspec
import qualified Data.Map as Map

------------------------------------------------------------------------------------------------
-- hspec tests
-------------------------------------------------------------------------------------------------
stratTreeTest :: SpecWith ()
stratTreeTest = do
    describe "best" $ do
        it "calculates the best moves" $ do
            --isJust (runReader (best aTree 1) testEnv1) `shouldBe` True
            isJust (fromTransformer (best aTree 1) testEnv1) `shouldBe` True

            head (_moveChoices (fromJust (fromTransformer (best aTree 1) testEnv1))) `shouldBe` IntMove 2
            _followingMoves (fromJust (fromTransformer (best aTree 1) testEnv1)) `shouldBe` []

            isJust (fromTransformer (best aTree (-1)) testEnv1) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree (-1)) testEnv1))) `shouldBe` IntMove 1
            _followingMoves (fromJust (fromTransformer (best aTree (-1)) testEnv1)) `shouldBe` []

            isJust (fromTransformer (best aTree 1) testEnv2) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree 1) testEnv2))) `shouldBe` IntMove 1
            _followingMoves (fromJust (fromTransformer (best aTree 1) testEnv2)) `shouldBe` [IntMove 4]

            isJust (fromTransformer (best aTree (-1)) testEnv2) `shouldBe`  True
            head (_moveChoices (fromJust (fromTransformer (best aTree (-1)) testEnv2)))
                `shouldBe` IntMove 1
            _followingMoves (fromJust (fromTransformer (best aTree (-1)) testEnv2))
                `shouldBe` [IntMove 3]

            isJust (fromTransformer (best aTree 1) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree 1) testEnv3))) `shouldBe` IntMove 1
            _followingMoves (fromJust (fromTransformer (best aTree 1) testEnv3))
                `shouldBe` [IntMove 3, IntMove 8]

            isJust (fromTransformer (best aTree (-1)) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree (-1)) testEnv3)))
                `shouldBe` IntMove 2
            _followingMoves (fromJust (fromTransformer (best aTree (-1)) testEnv3))
                `shouldBe` [IntMove 5, IntMove 12]

            isJust (fromTransformer (best aTree2 (-1)) testEnv2) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree2 (-1)) testEnv2)))
                `shouldBe` IntMove 3
            _followingMoves (fromJust (fromTransformer (best aTree2 (-1)) testEnv2))
                `shouldBe` [IntMove 10]

            isJust (fromTransformer (best aTree2 1) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree2 1) testEnv3)))
                `shouldBe` IntMove 3
            _followingMoves (fromJust (fromTransformer (best aTree2 1) testEnv3))
                `shouldBe` [IntMove 10, IntMove 33]

            isJust (fromTransformer (best aTree2 (-1)) testEnv3) `shouldBe` True
            head (_moveChoices (fromJust (fromTransformer (best aTree2 (-1)) testEnv3)))
                `shouldBe` IntMove 1
            _followingMoves (fromJust (fromTransformer (best aTree2 (-1)) testEnv3))
                `shouldBe` [IntMove 4, IntMove 15]
        it "returns a list moves with equivalent scores" $ do
            _moveChoices (fromJust (fromTransformer (best modTree 1) testEnv2))
                `shouldBe` [IntMove 1, IntMove 3, IntMove 2]
            _moveChoices (fromJust (fromTransformer (best aTree 1) testEnv2)) `shouldBe` [IntMove 1]
    describe "worstReply" $
        it "calculates the worst reply given a selected move" $ do
            isJust (worstReply aTree 3 1 (IntMove 1)) `shouldBe` True
            head (_moveChoices (fromJust (worstReply aTree 3 1 (IntMove 1)))) `shouldBe` IntMove 4
            _followingMoves (fromJust (worstReply aTree 3 1 (IntMove 1))) `shouldBe` [IntMove 10]

            isJust (worstReply aTree 3 (-1) (IntMove 2)) `shouldBe` True
            head (_moveChoices (fromJust (worstReply aTree 3 (-1) (IntMove 2))))
                `shouldBe` IntMove 7
            _followingMoves (fromJust (worstReply aTree 3 (-1) (IntMove 2))) `shouldBe` [IntMove 16]
    describe "checkBlunders" $
        it "takes a list of equivalent moves, and returns a subset of equivalent move\
           \representing the biggest mistake the oponent can make" $ do
            isJust (fromTransformer (checkBlunders blunderTree 1
                [mkMoveScore (IntMove 1) (IntEval 10), mkMoveScore (IntMove 2) (IntEval 10),
                 mkMoveScore (IntMove 20) (IntEval 10)]) testEnv3) `shouldBe` True
            fromJust (fromTransformer (checkBlunders blunderTree 1
                [mkMoveScore (IntMove 1) (IntEval 10), mkMoveScore (IntMove 2) (IntEval 10),
                 mkMoveScore (IntMove 20) (IntEval 10)]) testEnv3) `shouldBe`
                    [mkMoveScore (IntMove 2) (IntEval 80), mkMoveScore (IntMove 20) (IntEval 80)]
            --make sure it returns Just something for only one item in the list:
            isJust (fromTransformer (checkBlunders blunderTree 1
                [mkMoveScore (IntMove 1) (IntEval 10)]) testEnv3) `shouldBe` True
    describe "getChildren" $
            it "gets a list of child nodes" $ do
                fmap (getMove . label) (getChildren $ fromTree aTree)
                    `shouldBe` [IntMove 1,IntMove 2]
                fmap (getMove . label) (getChildren $ fromJust $ firstChild $ fromTree aTree2)
                    `shouldBe` [IntMove 4, IntMove 5, IntMove 6]
    describe "getSiblings" $
            it "gets a list of sibling nodes" $ do
                fmap (getMove . label) (getSiblings $ fromJust $ firstChild $ fromTree aTree)
                    `shouldBe` [IntMove 1,IntMove 2]
                fmap (getMove . label) (getSiblings $ fromJust $ firstChild $ fromTree aTree2)
                    `shouldBe` [IntMove 1, IntMove 2, IntMove 3]
    describe "childByMove" $
        it "finds a child of a tree matching a given move" $
            case childByMove (IntMove 1) (fromTree aTree) of
                Nothing -> IntMove (-1)
                Just x -> getMove $ label x
            `shouldBe` IntMove 1
    describe "descendPath" $
        it "follows a path of moves to the tree corresponding to the last move in the list" $ do
            descendPathTest [IntMove 1, IntMove 4, IntMove 9] aTree `shouldBe` 5
            descendPathTest [IntMove 2, IntMove 8, IntMove 25] aTree2 `shouldBe` 250
    describe "pruneChildrenExcept" $
        it "deletes all children except one matching the supplied move" $
            pruneChildrenExcept aMiniTree (IntMove 2) `shouldBe` prunedTree
    describe "processMove" $
        it "Prunes the tree of all the children except the one matching the supplied move" $ do
            processMove expandedTree (IntMove 2) `shouldBe` prunedExpandedTree
            processMove rootOnly (IntMove 2) `shouldBe` root2
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
            fromTransformer (expandTree aMiniPosTree) testEnv2 `shouldBe` expandedTree
            fromTransformer (expandTree finalTestTree) testEnv2 `shouldBe` expandedFinalTree
    describe "isWorse" $
        it "finds the worse of two scores given a margin given the color inquiring" $ do
            isWorse (IntEval 50) (IntEval 100) 0 1 `shouldBe` False
            isWorse (IntEval (-100)) (IntEval (-50)) 0 1 `shouldBe` False

            isWorse (IntEval 50) (IntEval 100) 10 1 `shouldBe` False
            isWorse (IntEval (-100)) (IntEval (-50)) 10 1 `shouldBe` False
            isWorse (IntEval 95) (IntEval 100) 10 1 `shouldBe` False
            isWorse (IntEval (-100)) (IntEval (-95)) 10 1 `shouldBe` False

            isWorse (IntEval 50) (IntEval 100) 0 (-1) `shouldBe` True
            isWorse (IntEval (-100)) (IntEval (-50)) 0 (-1) `shouldBe` True

            isWorse (IntEval 50) (IntEval 100)10 (-1) `shouldBe` True
            isWorse (IntEval (-100)) (IntEval (-50)) 10 (-1) `shouldBe` True
            isWorse (IntEval 95) (IntEval 100) 10 (-1) `shouldBe` False
            isWorse (IntEval (-100)) (IntEval (-95)) 10 (-1) `shouldBe` False

-----------------------------------------------------------------------
-- hspec support functions
-----------------------------------------------------------------------
fromTransformer :: RST a -> Env -> a
fromTransformer rst env = evalState (runReaderT (unRST rst) env) (GameState 0)

descendPathTest :: [IntMove] -> Tree TreeItem -> Int
descendPathTest xs t = case descendPath xs (fromTree t) of
                            Nothing -> -1
                            Just x  -> getInt $ getValue $ label x

--check that the path of moves retured by best is valid & the node at the bottom contains the correct --value
validPathCheck :: TreeNode t m e => Tree t -> Int -> Bool
validPathCheck t colr =
        case fromTransformer (best t colr) testEnvMax of
            Nothing -> False
            Just r -> let path = head (_moveChoices r) : _followingMoves r
                          bestValue = _score (head (_moveScores r))
                          mPathBottom = descendPath path (fromTree t)
                      in case mPathBottom of
                          Nothing -> False
                          Just tPos -> getInt (getValue (label tPos)) == getInt bestValue

testVisitor :: TreePos Full PosTreeItem -> Int -> Int -> TreePos Full PosTreeItem
testVisitor tPos depth maxi
    | depth == maxi  = modifyTree addBranch tPos
    | otherwise     = tPos

addBranch :: Tree PosTreeItem -> Tree PosTreeItem
addBranch t = Node (rootLabel t) [newBranch]

------------------------------------------------------------------------
data TreeItem  = TreeItem {
    _tiMove :: IntMove,
    _tiValue :: IntEval,
    _tiErrorValue :: IntEval
} deriving (Show, Eq)

data PosTreeItem  = PosTreeItem {
    ptMove :: IntMove,
    ptValue :: IntEval,
    ptColor :: Int,
    ptFinal :: FinalState,
    ptPosition :: TreePosition
} deriving (Show, Eq)

newtype TreePosition = TreePosition {
    tts :: [Int]
} deriving (Show, Eq)

instance TreeNode TreeItem IntMove IntEval where
    getMove = _tiMove
    getValue = _tiValue
    getErrorValue = _tiErrorValue

instance TreeNode PosTreeItem IntMove IntEval where
    getMove = ptMove
    getValue = ptValue
    getErrorValue = ptValue

instance PositionNode PosTreeItem IntMove IntEval where
    newNode = calcNewNode
    color = ptColor
    possibleMoves = calcPossibleMoves
    final = ptFinal
    parseMove _ _ = Left "Should not be called."

calcPossibleMoves :: PosTreeItem -> [IntMove]
calcPossibleMoves node = case ptMove node of
    IntMove 1 -> [IntMove 4, IntMove 5]
    IntMove 2 -> [IntMove 6, IntMove 7]
    IntMove 3 -> [IntMove 8]
    _ -> []

calcNewNode :: PosTreeItem -> IntMove -> PosTreeItem
calcNewNode _ mv = fromJust $ Map.lookup mv mvToNode

mvToNode :: Map IntMove PosTreeItem
mvToNode  = Map.fromList [
    (IntMove 2, PosTreeItem {ptMove=IntMove 2, ptValue = IntEval 2, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 1, 0]}}),
    (IntMove 4, PosTreeItem {ptMove=IntMove 4, ptValue = IntEval 4, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 0]}}),
    (IntMove 5, PosTreeItem {ptMove=IntMove 5, ptValue = IntEval 5, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 1]}}),
    (IntMove 6, PosTreeItem {ptMove=IntMove 6, ptValue=IntEval 6, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}}),
    (IntMove 7, PosTreeItem {ptMove=IntMove 7, ptValue=IntEval 7, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}}),
    (IntMove 8, PosTreeItem {ptMove=IntMove 8, ptValue=IntEval 8, ptColor = -1, ptFinal=NotFinal,
        ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 1, 0, 0, 0]}})]

-----------------------------------------------
testEnv1 :: Env
testEnv1 = Env { _depth =1, _errorDepth = 1, _equivThreshold = 0, _errorEquivThreshold = 10
               , _p1Comp = True, _p2Comp = False }

testEnv2 :: Env
testEnv2 = Env { _depth = 2, _errorDepth = 2, _equivThreshold = 0, _errorEquivThreshold = 10
               , _p1Comp = True, _p2Comp = False }

testEnv3 :: Env
testEnv3 = Env { _depth = 3, _errorDepth = 3, _equivThreshold = 0, _errorEquivThreshold = 10
               , _p1Comp = True, _p2Comp = False }

testEnvMax :: Env
testEnvMax = Env { _depth = -1, _errorDepth = -1, _equivThreshold = 0, _errorEquivThreshold = 10
                 , _p1Comp = True, _p2Comp = False }

rootOnly :: Tree PosTreeItem
rootOnly = Node PosTreeItem {ptMove=IntMove 0, ptValue = IntEval 0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} []

root2 :: Tree PosTreeItem
root2 = Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 1, 0]}} []

aMiniTree :: Tree TreeItem
aMiniTree = Node TreeItem {_tiMove = IntMove 0, _tiValue = IntEval 0, _tiErrorValue = IntEval 0 } [
    Node TreeItem {_tiMove = IntMove 1, _tiValue = IntEval 1, _tiErrorValue = IntEval 1} [],
    Node TreeItem {_tiMove = IntMove 2, _tiValue = IntEval 2, _tiErrorValue = IntEval 2} [],
    Node TreeItem {_tiMove = IntMove 3, _tiValue = IntEval 3, _tiErrorValue = IntEval 3} []]

newBranch :: Tree PosTreeItem
newBranch =  Node PosTreeItem {ptMove=IntMove 4, ptValue=IntEval 4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []

modTree :: Tree PosTreeItem
modTree = Node PosTreeItem {ptMove=IntMove 0, ptValue=IntEval 0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=IntMove 1, ptValue=IntEval 1, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 4 , ptValue=IntEval 4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []],
    Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 4, ptValue=IntEval 4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []],
    Node PosTreeItem {ptMove=IntMove 3, ptValue=IntEval 3, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 4, ptValue=IntEval 4, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 4, 0, 0]}} []]]

aMiniPosTree :: Tree PosTreeItem
aMiniPosTree = Node PosTreeItem {ptMove=IntMove 0, ptValue=IntEval 0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=IntMove 1, ptValue=IntEval 1, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=IntMove 3, ptValue=IntEval 3, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]

expandedTree :: Tree PosTreeItem
expandedTree =
    Node PosTreeItem {ptMove=IntMove 0, ptValue=IntEval 0, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 1, ptValue=IntEval 1, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=IntMove 4, ptValue=IntEval 4, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 0]}} [],
            Node PosTreeItem {ptMove=IntMove 5, ptValue=IntEval 5, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 1, 0, 1]}} []],
        Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=IntMove 6, ptValue=IntEval 6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
            Node PosTreeItem {ptMove=IntMove 7, ptValue=IntEval 7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []],
        Node PosTreeItem {ptMove=IntMove 3, ptValue=IntEval 3, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=IntMove 8, ptValue=IntEval 8, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 1, 0, 0, 0]}} []]]

prunedExpandedTree :: Tree PosTreeItem
prunedExpandedTree =
    Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 6, ptValue=IntEval 6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
        Node PosTreeItem {ptMove=IntMove 7, ptValue=IntEval 7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []]

finalTestTree :: Tree PosTreeItem
finalTestTree = Node PosTreeItem {ptMove=IntMove 0, ptValue= IntEval 0, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
    Node PosTreeItem {ptMove=IntMove 1, ptValue=IntEval 1, ptColor=1, ptFinal=BWins, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor=1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [],
    Node PosTreeItem {ptMove=IntMove 3, ptValue=IntEval 3, ptColor=1, ptFinal=WWins, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]

expandedFinalTree :: Tree PosTreeItem
expandedFinalTree =
    Node PosTreeItem {ptMove=IntMove 0, ptValue=IntEval 0, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 0, 0, 0, 0, 0, 0, 0, 0]}} [
        Node PosTreeItem {ptMove=IntMove 1, ptValue=IntEval 1, ptColor = 1, ptFinal=BWins, ptPosition=TreePosition {tts = [1, 0, 0, 0, 0, 0, 0, 0, 0]}} [],
        Node PosTreeItem {ptMove=IntMove 2, ptValue=IntEval 2, ptColor = 1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 0, 0, 0]}} [
            Node PosTreeItem {ptMove=IntMove 6, ptValue=IntEval 6, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} [],
            Node PosTreeItem {ptMove=IntMove 7, ptValue=IntEval 7, ptColor = -1, ptFinal=NotFinal, ptPosition=TreePosition {tts = [0, 1, 0, 0, 0, 0, 1, 1, 0]}} []],
        Node PosTreeItem {ptMove=IntMove 3, ptValue=IntEval 3, ptColor = 1, ptFinal=WWins, ptPosition=TreePosition {tts = [0, 0, 1, 0, 0, 0, 0, 0, 0]}} []]

prunedTree :: Tree TreeItem
prunedTree = Node TreeItem {_tiMove = IntMove 0, _tiValue = IntEval 0, _tiErrorValue = IntEval 0} [
    Node TreeItem {_tiMove = IntMove 2, _tiValue = IntEval 2, _tiErrorValue = IntEval 2} []]

aTree :: Tree TreeItem
aTree = Node TreeItem {_tiMove = IntMove 0, _tiValue = IntEval 0, _tiErrorValue = IntEval 0} [
    Node TreeItem {_tiMove = IntMove 1, _tiValue = IntEval (-80), _tiErrorValue = IntEval (-80)} [
        Node TreeItem {_tiMove = IntMove 3, _tiValue = IntEval 20, _tiErrorValue = IntEval 20} [
            Node TreeItem {_tiMove = IntMove 8, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} []],
        Node TreeItem {_tiMove = IntMove 4, _tiValue = IntEval (-40), _tiErrorValue = IntEval (-40)} [
            Node TreeItem {_tiMove = IntMove 9, _tiValue = IntEval 5, _tiErrorValue = IntEval 5} [],
            Node TreeItem {_tiMove = IntMove 10, _tiValue = IntEval 50, _tiErrorValue = IntEval 50} []]],
    Node TreeItem {_tiMove = IntMove 2, _tiValue = IntEval 70, _tiErrorValue = IntEval 70} [
        Node TreeItem {_tiMove = IntMove 5, _tiValue = IntEval 45, _tiErrorValue = IntEval 45} [
            Node TreeItem {_tiMove = IntMove 11, _tiValue = IntEval 0, _tiErrorValue = IntEval 0} [],
            Node TreeItem {_tiMove = IntMove 12, _tiValue = IntEval (-10), _tiErrorValue = IntEval (-10) } []],
        Node TreeItem {_tiMove = IntMove 6, _tiValue = IntEval (-60), _tiErrorValue = IntEval (-60)} [
            Node TreeItem {_tiMove = IntMove 13, _tiValue = IntEval (-20), _tiErrorValue = IntEval (-20)} [],
            Node TreeItem {_tiMove = IntMove 14, _tiValue = IntEval 0, _tiErrorValue = IntEval 0} []],
        Node TreeItem {_tiMove = IntMove 7, _tiValue = IntEval 30, _tiErrorValue = IntEval 30} [
            Node TreeItem {_tiMove = IntMove 15, _tiValue= IntEval 80, _tiErrorValue = IntEval 80} [],
            Node TreeItem {_tiMove = IntMove 16, _tiValue= IntEval (-90), _tiErrorValue = IntEval (-90)} [],
            Node TreeItem {_tiMove = IntMove 17, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} []]]]

blunderTree :: Tree TreeItem
blunderTree = Node TreeItem {_tiMove = IntMove 0, _tiValue = IntEval 0, _tiErrorValue = IntEval 0} [
    Node TreeItem {_tiMove = IntMove 1, _tiValue = IntEval (-80), _tiErrorValue = IntEval (-80)} [
        Node TreeItem {_tiMove = IntMove 3, _tiValue = IntEval 20, _tiErrorValue = IntEval 20} [
            Node TreeItem {_tiMove = IntMove 8, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} []],
        Node TreeItem {_tiMove = IntMove 4, _tiValue = IntEval (-40), _tiErrorValue = IntEval (-40)} [
            Node TreeItem {_tiMove = IntMove 9, _tiValue = IntEval 5, _tiErrorValue = IntEval 5} [],
            Node TreeItem {_tiMove = IntMove 10, _tiValue = IntEval 50, _tiErrorValue = IntEval 50} []]],
    Node TreeItem {_tiMove = IntMove 2, _tiValue = IntEval 70, _tiErrorValue = IntEval 70} [
        Node TreeItem {_tiMove = IntMove 5, _tiValue = IntEval 45, _tiErrorValue = IntEval 45} [
            Node TreeItem {_tiMove = IntMove 11, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} [],
            Node TreeItem {_tiMove = IntMove 12, _tiValue = IntEval (-10), _tiErrorValue = IntEval (-10)} []],
        Node TreeItem {_tiMove = IntMove 6, _tiValue = IntEval (-60), _tiErrorValue = IntEval (-40)} [
            Node TreeItem {_tiMove = IntMove 13, _tiValue = IntEval (-20), _tiErrorValue = IntEval (-20)} [],
            Node TreeItem {_tiMove = IntMove 14, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} []],
        Node TreeItem {_tiMove = IntMove 7, _tiValue = IntEval 30, _tiErrorValue = IntEval 30} [
            Node TreeItem {_tiMove = IntMove 15, _tiValue= IntEval 80, _tiErrorValue = IntEval 80} [],
            Node TreeItem {_tiMove = IntMove 16, _tiValue= IntEval (-90), _tiErrorValue = IntEval(-90)} [],
            Node TreeItem {_tiMove = IntMove 17, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} []]],
    Node TreeItem {_tiMove = IntMove 20, _tiValue = IntEval (-80), _tiErrorValue = IntEval (-80)} [
        Node TreeItem {_tiMove = IntMove 21, _tiValue = IntEval 20, _tiErrorValue = IntEval 20} [
            Node TreeItem {_tiMove = IntMove 24, _tiValue = IntEval 7, _tiErrorValue = IntEval 7} []],
            Node TreeItem {_tiMove = IntMove 23, _tiValue = IntEval 10, _tiErrorValue = IntEval 10} [],
        Node TreeItem {_tiMove = IntMove 22, _tiValue = IntEval (-40), _tiErrorValue = IntEval (-40)} [
            Node TreeItem {_tiMove = IntMove 25, _tiValue = IntEval 5, _tiErrorValue = IntEval 5} [],
            Node TreeItem {_tiMove = IntMove 26, _tiValue = IntEval 80, _tiErrorValue = IntEval 80} []]]]

aTree2 :: Tree TreeItem
aTree2 = Node TreeItem {_tiMove = IntMove 0, _tiValue = IntEval 0, _tiErrorValue = IntEval 0 } [
    Node TreeItem {_tiMove = IntMove 1, _tiValue = IntEval 10, _tiErrorValue = IntEval  10} [
        Node TreeItem {_tiMove = IntMove 4, _tiValue = IntEval 40, _tiErrorValue = IntEval 40 } [
            Node TreeItem {_tiMove = IntMove 13, _tiValue = IntEval (-130), _tiErrorValue = IntEval (-130) } [],
            Node TreeItem {_tiMove = IntMove 14, _tiValue = IntEval (-140), _tiErrorValue = IntEval  (-140)} [],
            Node TreeItem {_tiMove = IntMove 15, _tiValue = IntEval (-150), _tiErrorValue = IntEval (-150) } []],
        Node TreeItem {_tiMove = IntMove 5, _tiValue = IntEval 50, _tiErrorValue = IntEval 50 } [
            Node TreeItem {_tiMove = IntMove 16, _tiValue = IntEval (-160), _tiErrorValue = IntEval (-160) } [],
            Node TreeItem {_tiMove = IntMove 17, _tiValue = IntEval (-170), _tiErrorValue = IntEval (-170) } [],
            Node TreeItem {_tiMove = IntMove 18, _tiValue = IntEval (-180), _tiErrorValue = IntEval (-180) }[]],
        Node TreeItem {_tiMove = IntMove 6, _tiValue = IntEval 60, _tiErrorValue = IntEval 60 } [
            Node TreeItem {_tiMove = IntMove 19, _tiValue = IntEval  (-190), _tiErrorValue = IntEval (-190)} [],
            Node TreeItem {_tiMove = IntMove 20, _tiValue = IntEval (-200), _tiErrorValue = IntEval  (-200)} [],
            Node TreeItem {_tiMove = IntMove 21, _tiValue = IntEval (-210), _tiErrorValue = IntEval  (-210)} []]],
    Node TreeItem {_tiMove = IntMove 2, _tiValue = IntEval (-20), _tiErrorValue = IntEval (-20)} [
        Node TreeItem {_tiMove = IntMove 7, _tiValue = IntEval (-70), _tiErrorValue = IntEval (-70) } [
            Node TreeItem {_tiMove = IntMove 22, _tiValue = IntEval 220, _tiErrorValue = IntEval 220 } [],
            Node TreeItem {_tiMove = IntMove 23, _tiValue = IntEval 230, _tiErrorValue = IntEval 230 } [],
            Node TreeItem {_tiMove = IntMove 24, _tiValue = IntEval 240, _tiErrorValue = IntEval 240 }[]],
        Node TreeItem {_tiMove = IntMove 8, _tiValue = IntEval (-80), _tiErrorValue = IntEval (-80) } [
            Node TreeItem {_tiMove = IntMove 25, _tiValue = IntEval 250, _tiErrorValue = IntEval 250 } [],
            Node TreeItem {_tiMove = IntMove 26, _tiValue = IntEval 260, _tiErrorValue = IntEval 260 } [],
            Node TreeItem {_tiMove = IntMove 27, _tiValue = IntEval 270, _tiErrorValue = IntEval 270 } []],
        Node TreeItem {_tiMove = IntMove 9, _tiValue = IntEval (-90), _tiErrorValue = IntEval (-90) } [
            Node TreeItem {_tiMove = IntMove 28, _tiValue = IntEval 280, _tiErrorValue = IntEval 280 } [],
            Node TreeItem {_tiMove = IntMove 29, _tiValue = IntEval 290, _tiErrorValue = IntEval 290 } [],
            Node TreeItem {_tiMove = IntMove 30, _tiValue = IntEval 300, _tiErrorValue = IntEval 300 }[]]],
    Node TreeItem {_tiMove = IntMove 3, _tiValue = IntEval 30, _tiErrorValue = IntEval 30 } [
        Node TreeItem {_tiMove = IntMove 10, _tiValue = IntEval (-100), _tiErrorValue = IntEval (-100) } [
            Node TreeItem {_tiMove = IntMove 31, _tiValue = IntEval 310, _tiErrorValue = IntEval 310 } [],
            Node TreeItem {_tiMove = IntMove 32, _tiValue = IntEval 320, _tiErrorValue = IntEval 320 } [],
            Node TreeItem {_tiMove = IntMove 33, _tiValue = IntEval 330, _tiErrorValue = IntEval 330 }[]],
        Node TreeItem {_tiMove = IntMove 11, _tiValue = IntEval (-110), _tiErrorValue = IntEval (-110)} [
            Node TreeItem {_tiMove = IntMove 34, _tiValue = IntEval 340, _tiErrorValue = IntEval 340 } [],
            Node TreeItem {_tiMove = IntMove 35, _tiValue = IntEval 350, _tiErrorValue = IntEval 350 } [],
            Node TreeItem {_tiMove = IntMove 36, _tiValue = IntEval 360, _tiErrorValue = IntEval 360 } []],
        Node TreeItem {_tiMove = IntMove 12, _tiValue = IntEval (-120), _tiErrorValue = IntEval (-120) } [
            Node TreeItem {_tiMove = IntMove 37, _tiValue = IntEval 370, _tiErrorValue = IntEval 370 } [],
            Node TreeItem {_tiMove = IntMove 38, _tiValue = IntEval 380, _tiErrorValue = IntEval 380 } [],
            Node TreeItem {_tiMove = IntMove 39, _tiValue = IntEval 390, _tiErrorValue = IntEval 390 } []]]]
