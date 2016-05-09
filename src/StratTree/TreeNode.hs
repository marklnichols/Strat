{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, 
       MoveScore (..), move, score, Result (..), Env (..)) where

import Control.Lens
       
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
-- New Attempt:
{-- 
class Move m where 
    showMove :: m -> String
    equals :: m -> Bool
  
class Move m => TreeNode t m where
    getMoveNode :: t -> m
    getValue :: t -> Int
    getErrorValue :: t -> Int
  
class (TreeNode n m, Show n, Move m) => PositionNode n m where
    newNode :: n -> m -> n
    color :: n -> Int
    possibleMoves :: n -> [m]
    final :: n -> FinalState
    showPosition :: n -> String
--}
-- Original
class TreeNode t where
    getMove :: t -> Int
    getValue :: t -> Int
    getErrorValue :: t -> Int
  
class (TreeNode n, Show n) => PositionNode n where
    newNode :: n -> Int -> n
    color :: n -> Int
    possibleMoves :: n -> [Int]
    final :: n -> FinalState
    showPosition :: n -> String
 
data FinalState = WWins | BWins | Draw | NotFinal deriving (Enum, Show, Eq)

data Env = Env 
    {_depth :: Int, _errorDepth :: Int, _equivThreshold :: Int, _errorEquivThreshold :: Int,
     _p1Comp :: Bool, _p2Comp :: Bool } deriving (Show)

data MoveScore = MoveScore {_move :: Int, _score :: Int} deriving (Eq)

instance Show MoveScore where                                                                                       
    show (MoveScore m s ) = "(m:" ++ show m ++ ", s:" ++ show s ++ ")"     

data Result = Result {_moveChoices :: [Int], _followingMoves :: [Int], _moveScores ::[MoveScore]} 
                deriving(Show, Eq)
           
$(makeLenses ''MoveScore)
makeLenses ''Env
makeLenses ''Result      
                
-------------------------------------------------------------------------------
flipColor :: Int -> Int
flipColor = negate      --alternate 1 / -1
