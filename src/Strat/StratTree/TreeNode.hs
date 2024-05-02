{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Strat.StratTree.TreeNode
    (
      Entry(..)
    , Env (..)
    , Eval (..)
    , FinalState (..)
    , GameState(..)
    , mkMoveScore
    , mkMoveScores
    , move
    , Move
    , MoveResults(..)
    , MoveScore (_move, _score)
    , Output(..)
    , RST (..)
    , RSTransformer
    , score
    , TreeLocation(..)
    , TreeNode (..)
    , wrapRST
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Tree
import Data.Hashable
import GHC.Generics
import qualified Strat.ZipTree as Z

----------------------------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------------------------
data FinalState = WWins | BWins | Draw | NotFinal
    deriving (Generic, Enum, Show, Eq, Ord)

instance Hashable FinalState where
  -- hashWithSalt = hashWithSalt

data Env = Env
    { equivThreshold :: Float } deriving (Show)

data MoveScore m e = MoveScore {_move :: m, _score :: e} deriving (Show, Eq)
$(makeLenses ''MoveScore)

data MoveResults t m = MoveResults
  { mrResult :: Z.NegaResult t
  , mrMoveScores :: [MoveScore m t]
  , mrMove :: m
  , mrNewTree :: Tree t }

newtype GameState = GameState {_movesConsidered :: Integer} deriving (Show, Eq)

newtype TreeLocation  = TreeLocation { tlDepth :: Int }
  deriving (Generic, Hashable, Show, Eq)

data Entry m s where
  MoveEntry :: (Move m) => m -> Entry m s
  CmdEntry :: String-> Entry m s

instance Show m => Show (Entry m s) where
  show (MoveEntry m) = show m
  show (CmdEntry s) = s

instance Eq m => Eq (Entry m s) where
  (==) (MoveEntry m1) (MoveEntry m2) = m1 == m2
  (==) (CmdEntry s1) (CmdEntry s2) = s1 == s2
  (==) (MoveEntry _) (CmdEntry _) = False
  (==) (CmdEntry _) (MoveEntry _) = False

----------------------------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------------------------
class (Show m, Eq m, Ord m) => Move m

class (Show e, Eq e, Ord e) => Eval e where
    evaluate :: e -> Float
    isEvaluated :: e -> Bool
    setFloat :: e -> Float -> e

class (Move m, Eval t) => TreeNode t m | t -> m where
    newNode :: t -> m -> TreeLocation -> t
    color :: t -> Int
    possibleMoves :: t -> [m]
    final :: t -> FinalState
    critical :: t -> Bool
    parseEntry :: t -> String -> Either String (Entry m s)
    getMove :: t -> m
    treeLoc :: t -> TreeLocation
    undoMove :: t -> m -> t
    moveNum :: t -> Int

class Output o n m | o -> n, n -> m where
    out :: o -> String -> IO ()
    updateBoard :: o -> n -> IO ()
    --TODO: replace this with showMoveInfo
    showCompMove :: o -> Tree n -> Z.NegaResult n -> Bool -> IO ()
    getPlayerEntry :: o -> Tree n -> [m] -> IO (Entry m s)
    gameError :: o -> String -> IO ()
    showAs :: o -> String -> n -> IO ()
    showAs o _ node = updateBoard o node


mkMoveScores :: (TreeNode n m, Eval n) => [n] -> [MoveScore m n]
mkMoveScores tns = map (\x -> mkMoveScore (getMove x) x) tns

mkMoveScore :: m -> n -> MoveScore m n
mkMoveScore = MoveScore

---------------------------------------------------------------------------------------------------
-- Monad Transformer stack
---------------------------------------------------------------------------------------------------
type RSTransformer a = ReaderT Env (State GameState) a

wrapRST :: a -> RSTransformer a
wrapRST = return

newtype RST a = RST { unRST :: RSTransformer a }
  deriving (Monad, Applicative, Functor, MonadReader Env, MonadState GameState)
