{-# LANGUAGE MultiParamTypeClasses #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, MoveScore (..), Result (..)) where
 
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class TreeNode t where
    getMove :: t -> Int
    getValue :: t -> Int
 
class TreeNode n => PositionNode n where
    newNode :: n -> Int -> n
    color :: n -> Int
    evaluate :: n -> Int    
    possibleMoves :: n -> [Int]
    final :: n -> FinalState
    
data FinalState = WWins | BWins | Draw | NotFinal deriving (Enum, Show, Eq)

data MoveScore = MoveScore {_move :: Int, _score :: Int}

data Result = Result { _moveChoices :: [Int], _followingMoves :: [Int], _moveScores ::[MoveScore] }

-------------------------------------------------------------------------------
flipColor :: Int -> Int
flipColor n = 0 - n     --alternate 1 / -1
