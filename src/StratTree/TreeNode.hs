{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, 
       MoveScore (..), move, score, Result (..), Env (..)) where

import Control.Lens
       
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class TreeNode t where
    getMove :: t -> Int
    getValue :: t -> Int
    getErrorValue :: t -> Int
 
class TreeNode n => PositionNode n where
    newNode :: n -> Int -> n
    color :: n -> Int
    -- evaluate :: n -> Int 
    --errorEvaluate :: n -> Int
    possibleMoves :: n -> [Int]
    final :: n -> FinalState
    
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
