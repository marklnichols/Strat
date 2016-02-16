{-# LANGUAGE MultiParamTypeClasses #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor) where
 
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

-------------------------------------------------------------------------------
flipColor :: Int -> Int
flipColor n = 0 - n     --alternate 1 / -1
