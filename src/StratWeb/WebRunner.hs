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
import Data.Text (pack)
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as V
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

--TODO: move these into the env
maxRndPercent :: Float
maxRndPercent = 0.05

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------
--start game request received
processStartGame :: Tree Ck.CkNode -> Bool -> IO (NodeWrapper, Vector Float)
processStartGame node bComputerResponse = do
    rnd <- getStdGen
    let xs = randomRs (-maxRndPercent, maxRndPercent) rnd
    let rnds = V.fromList xs
    if bComputerResponse
        then do
            resp <- computerResponse node rnds
            return (resp, rnds)
        else return (createUpdate "Score for Player's position: 0"  node node Nothing, rnds)

processComputerMove :: Tree Ck.CkNode -> Vector Float -> IO NodeWrapper
processComputerMove tree rnds = do
    let posColor = color $ rootLabel tree
    liftIO $ putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree rnds

--player move (web request) received
processPlayerMove :: Tree Ck.CkNode -> Ck.CkMove -> Bool -> Vector Float -> IO NodeWrapper
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
testEnv :: ZipTreeEnv
testEnv = ZipTreeEnv
        { verbose = False
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

computerResponse :: Tree Ck.CkNode -> Vector Float -> IO NodeWrapper
computerResponse prevNode rnds = do
    eitherNode <- computerMove prevNode rnds
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

computerMove :: Tree Ck.CkNode -> Vector Float
                -> IO (Either String (MoveResults Ck.CkNode Ck.CkMove))
computerMove t rnds = do
   newTree <- runReaderT (expandTo t (ceDepth gameEnv) (ceCritDepth gameEnv)) testEnv
   res@NegaResult{..} <- runReaderT (negaRnd newTree rnds True) testEnv
   let bestMv = getMove $ moveNode picked
   let moveScores = mkMoveScores (evalNode picked : (evalNode <$> alternatives))
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
