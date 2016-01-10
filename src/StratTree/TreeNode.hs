{-# LANGUAGE MultiParamTypeClasses #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), Position(..)) where
 
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class TreeNode t where
    getMove :: t -> Int
    getValue :: t -> Int
 
class Position p => PositionNode n p where
    getPosition :: n -> p
    
class Position p where
    evaluate :: p -> Int    --TODO add color? or should positionNode contain color?
    possibleMoves :: p -> Int -> [Int]
    newPosition :: p -> Int -> p  
    