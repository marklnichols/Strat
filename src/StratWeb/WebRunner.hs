{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module StratWeb.WebRunner
    ( Jsonable(..)
    , NodeWrapper(..)
    , processComputerMove
    , processPlayerMove
    , processStartGame
    ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Tree
import Strat.Helpers
import Strat.StratTree.TreeNode
import Strat.ZipTree
import System.Random
import qualified Checkers as Ck
import qualified CheckersJson as J

data CheckersEnv = CheckersEnv
    { ceDepth :: Int, ceCritDepth ::Int,  ceEquivThreshold :: Float, ceP1Comp :: Bool ,ceP2Comp :: Bool } deriving (Show)

gameEnv :: CheckersEnv
gameEnv = CheckersEnv { ceDepth = 6, ceCritDepth = 10, ceEquivThreshold = 0.0
              , ceP1Comp = False, ceP2Comp = True }

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
    rnd <- getStdGen
    if bComputerResponse
        then do
            resp <- computerResponse node rnd
            return (resp, rnd)
        else return (createUpdate "Score for Player's position: 0"  node node Nothing, rnd)

processComputerMove :: (RandomGen g) => Tree Ck.CkNode -> g -> IO NodeWrapper
processComputerMove tree gen = do
    let posColor = color $ rootLabel tree
    liftIO $ putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree gen

--player move (web request) received
processPlayerMove :: RandomGen g => Tree Ck.CkNode -> Ck.CkMove -> Bool -> g -> IO NodeWrapper
processPlayerMove tree mv bComputerResponse gen = do
    let processed = findMove tree mv
    let done = checkGameOver processed
    if fst done
        then return $ createMessage (snd done) processed
        else if bComputerResponse
            then do
                let posColor = color $ rootLabel processed
                liftIO $ putStrLn $ "Computer move (In processPlayerMove), turn = " ++ show (colorToTurn posColor)
                computerResponse processed gen
            else return $ createUpdate "No computer move" processed processed Nothing

----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------  11
computerResponse :: (RandomGen g) => Tree Ck.CkNode -> g -> IO NodeWrapper
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

computerMove :: (RandomGen g) => Tree Ck.CkNode -> g
                -> IO (Either String (MoveResults Ck.CkNode Ck.CkMove))
computerMove t gen = do
    let newTree = expandTo t (ceDepth gameEnv) (ceCritDepth gameEnv)

    let res@NegaResult{..} = negaRnd newTree gen (ceEquivThreshold gameEnv) True

    let bestMv = getMove $ head $ moveSeq best

    let moveScores = mkMoveScores (branchScore best : (branchScore <$> alternatives))
    return $ Right ( MoveResults
      { mrResult = res
      , mrMoveScores = moveScores
      , mrMove = bestMv
      , mrNewTree = newTree } )

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
                                          (Ck.getAllowedMoves (rootLabel newN)) ms}

createError :: String -> Tree Ck.CkNode-> NodeWrapper
createError s node = NodeWrapper {getNode = node, getLastMove = Nothing,
                                  getJsonable = Jsonable (J.jsonError s)}
