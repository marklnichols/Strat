{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module StratWeb.WebRunner
    ( Jsonable(..)
    , NodeWrapper(..)
    , processComputerMove
    , processPlayerMove
    , processStartGame
    ) where

import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Data.Aeson
import Data.Hashable
import Data.Text (pack)
import Data.Tree
import Strat.Helpers
import Strat.StratTree.TreeNode
import qualified Strat.ZipTree as Z
import System.Random
import qualified Checkers as Ck
import qualified CheckersJson as J

data CheckersEnv = CheckersEnv
    { ceDepth :: Int, ceCritDepth ::Int,  ceEquivThreshold :: Float, ceP1Comp :: Bool ,ceP2Comp :: Bool } deriving (Show)

gameEnv :: Z.ZipTreeEnv
gameEnv = Z.ZipTreeEnv
        { verbose = False
        , enablePruning = True
        , singleThreaded = True -- multi threaded has yet to be tested here
        , enablePruneTracing = False
        , enableCmpTracing = False
        , enableRandom = True
        , maxRandomChange = 0.05
        , enablePreSort = False
        , moveTraceStr = pack ""
        , maxDepth = 6
        , maxCritDepth = 10
        , aiPlaysWhite = False
        , aiPlaysBlack = True
        }

newtype FakeState = FakeState {unFake :: String}

fakeState :: FakeState
fakeState = FakeState {unFake = "Just a fake state"}

instance Z.PositionState FakeState where
  toString = unFake
  combineTwo x _ = x

data Jsonable = forall j. ToJSON j => Jsonable j

data NodeWrapper = NodeWrapper { getNode :: Tree Ck.CkNode
                                   , getLastMove :: Maybe (MoveScore Ck.CkMove Ck.CkNode)
                                   , getJsonable :: Jsonable }


----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------
--start game request received
processStartGame :: Tree Ck.CkNode -> Bool -> IO (NodeWrapper, StdGen)
processStartGame node bComputerResponse = do
    gen <- getStdGen
    if bComputerResponse
        then do
            resp <- computerResponse node gen
            return (resp, gen)
        else return (createUpdate "Score for Player's position: 0"  node node Nothing, gen)

processComputerMove :: RandomGen g => Tree Ck.CkNode -> g -> IO NodeWrapper
processComputerMove tree gen = do
    let posColor = color $ rootLabel tree
    liftIO $ putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree gen

--player move (web request) received
processPlayerMove :: RandomGen g => Tree Ck.CkNode -> Ck.CkMove -> Bool -> g -> IO NodeWrapper
processPlayerMove tree mv bComputerResponse rnds = do
    -- let processed = findMove tree mv
    case (findMove tree mv) of
        Left s -> error s -- this shouldn't happen
        Right processed -> do
            let done = checkGameOver processed
            if fst done
                then return $ createMessage (snd done) processed
                else if bComputerResponse
                    then do
                        let posColor = color $ rootLabel processed
                        liftIO $ putStrLn $ "Computer move (In processPlayerMove), turn = "
                                          ++ show (colorToTurn posColor)
                        computerResponse processed rnds
                    else return $ createUpdate "No computer move" processed processed Nothing

----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------
-- TODO: move this
testEnv :: Z.ZipTreeEnv
testEnv = Z.ZipTreeEnv
        { verbose = False
        , enablePruning = True
        , singleThreaded = True -- multi threaded has yet to be tested here
        , enablePruneTracing = False
        , enableCmpTracing = False
        , enableRandom = False
        , maxRandomChange = 0.0
        , enablePreSort = True
        , moveTraceStr = pack ""
        , maxDepth = 5
        , maxCritDepth = 5
        , aiPlaysWhite = True
        , aiPlaysBlack = True
        }

computerResponse :: RandomGen g => Tree Ck.CkNode -> g -> IO NodeWrapper
computerResponse prevNode gen = do
    eitherNode <- computerMove prevNode gen
    case eitherNode of
        Left s -> return $ createError s prevNode
        Right MoveResults{..} ->
            let (b, currentPosStr) = checkGameOver mrNewTree
                bestScoreStr = if not b -- if the game is not over...
                    then "Player's score, computer's best move found:<br/>"
                         ++ show (evaluate (rootLabel mrNewTree))
                    else ""
                ms = case mrMoveScores of
                  [] -> Nothing
                  (x:_xs) -> Just x
            in return $ createUpdate (currentPosStr ++ "<br/><br/>" ++ bestScoreStr) prevNode mrNewTree ms

computerMove :: RandomGen g => Tree Ck.CkNode -> g
                -> IO (Either String (MoveResults Ck.CkNode Ck.CkMove))
computerMove t gen = do
   (newTree, _, _) <- runRWST (Z.expandTo t 1 (Z.maxDepth gameEnv) (Z.maxCritDepth gameEnv)) testEnv fakeState
   (res@Z.NegaResult{..}, _, _) <- runRWST (Z.negaMax newTree (Just gen)) testEnv fakeState
   let bestMv = getMove $ Z.nmNode picked
   let moveScores = mkMoveScores ((last (Z.nmMovePath picked)) : ((last . Z.nmMovePath)  <$> alternatives))
   return $ Right ( MoveResults
      { mrResult = res
      , mrMoveScores = moveScores
      , mrMove = bestMv
      , mrNewTree = newTree } )

searchTo :: (Z.ZipTreeNode n, Hashable n, Ord n, Show n, Eval n, RandomGen g)
         => Tree n -> Maybe g -> Int -> Int -> Z.ZipTreeM p (Tree n, Z.NegaResult n)
searchTo t gen maxDepth maxCritDepth = do
    env <- ask
    expanded <- Z.expandTo t 1 (Z.maxDepth env) (Z.maxCritDepth env)
    res <- Z.negaMax expanded gen
    return (t, res)

checkGameOver :: Tree Ck.CkNode -> (Bool, String)
checkGameOver node =
    case final $ rootLabel node of
        WWins -> (True, "White wins.")
        BWins -> (True, "Black wins.")
        Draw  -> (True, "Draw.")
        _     ->
            let evalScore = rootLabel node
            in (False, "Player's score, current position:<br/>" ++ show evalScore)

--convert +1, -1 to 1, 2
colorToTurn :: Int -> Int
colorToTurn 1 = 1
colorToTurn _ = 2

createMessage :: String -> Tree Ck.CkNode -> NodeWrapper
createMessage s node = NodeWrapper {getNode = node, getLastMove = Nothing,
                                    getJsonable = Jsonable (J.jsonMessage s)}

createUpdate :: String -> Tree Ck.CkNode -> Tree Ck.CkNode
             -> Maybe (MoveScore Ck.CkMove Ck.CkNode) -> NodeWrapper
createUpdate msg prevN newN ms =
    NodeWrapper {getNode = newN,
                 getLastMove =  ms,
                 getJsonable = Jsonable $ J.jsonUpdate msg (rootLabel prevN) (rootLabel newN)
                                          (Ck.getAllowedMoves (rootLabel newN) ) ms}

createError :: String -> Tree Ck.CkNode-> NodeWrapper
createError s node = NodeWrapper {getNode = node, getLastMove = Nothing,
                                  getJsonable = Jsonable (J.jsonError s)}
