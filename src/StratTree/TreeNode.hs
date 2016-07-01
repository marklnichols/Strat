{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, 
       mkMoveScore, MoveScore (_move, _score) , move, score, Result (..), moveScores, Env (..), Move (..), IntMove (..)) where

import Control.Lens
       
-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class (Show m, Eq m, Ord m) => Move m 

-------------------------------------------------
-- Predefined instance of Move for Int
-------------------------------------------------
data IntMove = IntMove {theInt :: Int} 

instance Show IntMove where
    show m = show $ theInt m

instance Eq IntMove where
    (==) m1 m2 = theInt m1 == theInt m2
    
instance Ord IntMove where
    (<=) m1 m2 = theInt m1 <= theInt m2
    
instance Move IntMove
-------------------------------------------------

--TODO: getValue, getErrorValue return a Reader monad so scores can depend on
--depth, skill level settings, etc.
class Move m => TreeNode t m | t -> m where
    getMove :: t -> m
    getValue :: t -> Int
    getErrorValue :: t -> Int
  
class (TreeNode n m, Show n, Move m) => PositionNode n m | n -> m where
    newNode :: n -> m -> n      -- TODO: make this return Maybe n
    color :: n -> Int
    possibleMoves :: n -> [m]
    final :: n -> FinalState
    showPosition :: n -> String
    parseMove :: n -> String -> m -- TODO: make this return Maybe m

data FinalState = WWins | BWins | Draw | NotFinal deriving (Enum, Show, Eq)

data Env = Env 
    {_depth :: Int, _errorDepth :: Int, _equivThreshold :: Int, _errorEquivThreshold :: Int,
     _p1Comp :: Bool, _p2Comp :: Bool } deriving (Show)

data MoveScore m = MoveScore {_move :: m, _score :: Int} deriving (Eq, Show)

mkMoveScore :: Move m => m -> Int -> MoveScore m
mkMoveScore = MoveScore

--instance Show MoveScore where                                                                                       
--    show (MoveScore m s ) = "(m:" ++ show m ++ ", s:" ++ show s ++ ")"     

data Result m = Result {_moveChoices :: [m], _followingMoves :: [m], _moveScores ::[MoveScore m]} 
                deriving(Show, Eq)
           
$(makeLenses ''MoveScore)
makeLenses ''Env
makeLenses ''Result      
                
-------------------------------------------------------------------------------
flipColor :: Int -> Int
flipColor = negate      --alternate 1 / -1
