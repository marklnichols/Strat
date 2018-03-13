{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module StratWeb.WebRunner 
    ( Jsonable(..)
    , NodeWrapper(..)    
    , processComputerMove
    , processError 
    , processPlayerMove
    , processStartGame
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson
import Data.Tree
import Strat.StratTree.TreeNode
import Strat.StratIO
import Strat.StratTree
import qualified Checkers as Ck
import qualified CheckersJson as J

gameEnv :: Env
gameEnv = Env { _depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0
              , _p1Comp = False, _p2Comp = True }

data Jsonable = forall j. ToJSON j => Jsonable j 

data NodeWrapper = NodeWrapper { getNode :: Tree Ck.CkNode
                               , getLastMove :: Maybe (MoveScore Ck.CkMove Ck.CkEval)
                               , getJsonable :: Jsonable }

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------         
--start game request received  
processStartGame :: Tree Ck.CkNode -> Bool -> IO NodeWrapper
processStartGame node bComputerResponse = 
    if bComputerResponse
        then computerResponse node 1 
        else return $ createUpdate "Score for Player's position: 0"  node node Nothing    

processComputerMove :: Tree Ck.CkNode -> IO NodeWrapper
processComputerMove tree = do
    let posColor = rootLabel tree ^. Ck.ckPosition . Ck.clr
    liftIO $ putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree (colorToTurn posColor)
      
--player move (web request) received
processPlayerMove :: Tree Ck.CkNode -> Ck.CkMove -> Bool -> IO NodeWrapper
processPlayerMove tree mv bComputerResponse = do
    let processed = processMove tree mv
    let done = checkGameOver processed
    if fst done
        then return $ createMessage (snd done) processed
        else if bComputerResponse
            then do 
                let posColor = rootLabel processed ^. (Ck.ckPosition . Ck.clr)
                liftIO $ putStrLn $ "Computer move (In processPlayerMove), turn = " ++ show (colorToTurn posColor)
                computerResponse processed (colorToTurn posColor) 
            else return $ createUpdate "No computer move" processed processed Nothing
 
processError :: String -> Tree Ck.CkNode -> IO NodeWrapper
processError str tree = return $ createError str tree
 
----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------  11
computerResponse :: Tree Ck.CkNode -> Int -> IO NodeWrapper
computerResponse prevNode turn = do
    eitherNode <- computerMove prevNode turn
    case eitherNode of
        Left s -> return $ createError s prevNode
        Right (n, ms) -> 
            let (b, currentPosStr) = checkGameOver n
                bestScoreStr = if not b -- if the game is not over...
                    then "Player's score, computer's best move found:<br/>" ++ show (_score ms)
                    else ""    
            in return $createUpdate (currentPosStr ++ "<br/><br/>" ++ bestScoreStr) prevNode n (Just ms)
        
computerMove :: Tree Ck.CkNode -> Int ->  
                IO (Either String (Tree Ck.CkNode, MoveScore Ck.CkMove Ck.CkEval))
computerMove node turn = do
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> return $ Left "Invalid result returned from best"
        Just result -> do
            moveScoreM <- resolveRandom (result^.moveScores)
            case moveScoreM of
                Nothing -> return $ Left "Invalid result from resolveRandom"
                Just ms -> return $ Right (processMove newTree (ms^.move), ms)
                                                   
checkGameOver :: Tree Ck.CkNode -> (Bool, String)
checkGameOver node =              
    case final $ rootLabel node of
        WWins -> (True, "White wins.")
        BWins -> (True, "Black wins.")
        Draw  -> (True, "Draw.")
        _     -> 
            let evalScore = rootLabel node ^. Ck.ckValue
            in (False, "Player's score, current position:<br/>" ++ show evalScore)

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1

--convert +1, -1 to 1, 2
colorToTurn :: Int -> Int
colorToTurn 1 = 1
colorToTurn _ = 2

createMessage :: String -> Tree Ck.CkNode -> NodeWrapper
createMessage s node = NodeWrapper {getNode = node, getLastMove = Nothing, 
                                    getJsonable = Jsonable (J.jsonMessage s)}

createUpdate :: String -> Tree Ck.CkNode -> Tree Ck.CkNode -> Maybe (MoveScore Ck.CkMove Ck.CkEval) -> NodeWrapper
createUpdate msg prevN newN ms = 
    NodeWrapper {getNode = newN, 
                 getLastMove =  ms, 
                 getJsonable = Jsonable $ J.jsonUpdate msg (rootLabel prevN) (rootLabel newN)
                                          (Ck.getAllowedMoves (rootLabel newN)) ms}
                 
createError :: String -> Tree Ck.CkNode -> NodeWrapper
createError s node = NodeWrapper {getNode = node, getLastMove = Nothing, 
                                  getJsonable = Jsonable (J.jsonError s)}