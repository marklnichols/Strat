{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StratTree.TreeNode (TreeNode (..), PositionNode (..), FinalState (..), flipColor, keepColor,
       mkMoveScore, MoveScore (_move, _score) , move, score, Result (..), moveChoices,
       followingMoves, moveScores, Env (..), Move, Eval (..), IntMove (..), IntEval (..),
       RST (..), RSTransformer, GameState(..)) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

-------------------------------------------------------------
-- Data types
-------------------------------------------------------------
class (Show m, Eq m, Ord m) => Move m

class (Show e, Eq e, Ord e) => Eval e where
    getInt :: e -> Int
    setInt :: e -> Int -> e
    fromInt :: Int -> e

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
-- Predefined instance of Eval for Int
-------------------------------------------------
data IntEval = IntEval {theVal :: Int}

instance Show IntEval where
    show m = show $ theVal m

instance Eq IntEval where
    (==) m1 m2 = theVal m1 == theVal m2

instance Ord IntEval where
    (<=) m1 m2 = theVal m1 <= theVal m2

instance Eval IntEval where
    getInt = theVal
    setInt _ = IntEval
    fromInt = IntEval

-------------------------------------------------
--TODO: getValue, getErrorValue return a Reader monad so scores can depend on
--depth, skill level settings, etc.
class (Move m, Eval e) => TreeNode t m e | t -> m, t -> e where
     getMove :: t -> m
     getValue :: t -> e
     getErrorValue :: t -> e

class (TreeNode n m e, Show n, Move m, Eval e) => PositionNode n m e | n -> m, n -> e where
    newNode :: n -> m -> n      -- TODO: make this return Maybe n
    color :: n -> Int
    possibleMoves :: n -> [m]
    final :: n -> FinalState
    showPosition :: n -> String
    parseMove :: n -> String -> Either String m

data FinalState = WWins | BWins | Draw | NotFinal deriving (Enum, Show, Eq)

data Env = Env
    {_depth :: Int, _errorDepth :: Int, _equivThreshold :: Int, _errorEquivThreshold :: Int,
     _p1Comp :: Bool, _p2Comp :: Bool } deriving (Show)

data MoveScore m e = MoveScore {_move :: m, _score :: e} deriving (Eq)

$(makeLenses ''MoveScore)

instance (Show m, Show e) => Show (MoveScore m e) where
    show ms = "(m:" ++ show (ms^.move) ++ " s:" ++ show (ms^.score) ++ ")"

mkMoveScore :: m -> e -> MoveScore m e
mkMoveScore = MoveScore

data Result m e = Result {_moveChoices :: [m], _followingMoves :: [m], _moveScores ::[MoveScore m e]}
                deriving(Show, Eq)

makeLenses ''Result

data GameState = GameState {_movesConsidered :: Integer} deriving (Show, Eq)

---------------------------------------------------------------------------------------------------
-- Monad Transformer stack
---------------------------------------------------------------------------------------------------
type RSTransformer a = ReaderT Env (State GameState) a

newtype RST a = RST { unRST :: RSTransformer a }
  deriving (Monad, Applicative, Functor, MonadReader Env, MonadState GameState)

-------------------------------------------------------------------------------
flipColor :: Eval e => e -> e
flipColor e = setInt e (negate (getInt e))

keepColor :: e -> e
keepColor = id
