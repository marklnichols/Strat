{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module WebRunner where

import StratTree.TreeNode
import StratTree.StratTree
import StratIO.StratIO
import Data.Tree
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens
import Data.Aeson
import qualified CheckersJson as J
import qualified Checkers as Ck
import qualified Web.Scotty.Trans as T
import Data.Text.Lazy (Text) 
import GlobalState

--TODO: fix -- to get things working, for the moment this web piece is checkers specific

--TODO: move to common...
gameEnv :: Env
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

data Jsonable = forall j. ToJSON j => Jsonable j 

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------         
jsonableToActionT :: Jsonable -> T.ActionT Text WebM ()
jsonableToActionT j = 
    case j of
        Jsonable x -> T.json x
  
--start game request received  
--TODO new session key, put node after optional move into session
processStartGame :: Tree Ck.CkNode -> Bool -> IO Jsonable
processStartGame node bComputerResponse = 
    if bComputerResponse
        then computerResponse node 1 
        else return $ createUpdate "New Game, player moves first" (rootLabel node)                  
 
--player move (web request) received
processPlayerMove :: Tree Ck.CkNode -> Ck.CkMove -> Bool -> IO Jsonable
processPlayerMove tree mv bComputerResponse = do
    let processed = processMove tree mv
    let done = checkGameOver processed
    if fst done
        then return $ createMessage (snd done) 
        else if bComputerResponse
            then do 
                let posColor = rootLabel tree ^. (Ck.ckPosition . Ck.clr)
                computerResponse processed (colorToTurn posColor) 
            else return $ createUpdate "No computer move" (rootLabel processed)
            

processComputerMove :: Tree Ck.CkNode -> IO Jsonable
processComputerMove tree = do
    let posColor = rootLabel tree ^. Ck.ckPosition . Ck.clr
    computerResponse tree (colorToTurn posColor)
        
----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------                 
computerResponse :: Tree Ck.CkNode -> Int -> IO Jsonable
computerResponse node turn = do
    eitherNode <- computerMove node turn
    case eitherNode of
        Left s -> return $ createError s
        Right n -> return $ createUpdate (snd (checkGameOver n)) (rootLabel n)        
        
computerMove :: Tree Ck.CkNode -> Int -> IO (Either String (Tree Ck.CkNode))
computerMove node turn = do
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> return $ Left "Invalid result returned from best"
        Just result -> do
            --let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            --let badMoves = evalState (runReaderT (unRST badMovesM) gameEnv) (GameState 0)
            --let finalChoices = fromMaybe (result ^. moveScores) badMoves
            
            --moveM <- resolveRandom finalChoices
            moveM <- resolveRandom (result^.moveScores)
            case moveM of
                Nothing -> return $ Left "Invalid result from resolveRandom"
                Just mv -> return $ Right (processMove newTree mv)
                                                   
compMoveNext :: Tree Ck.CkNode -> Int -> Bool
compMoveNext _ turn = evalState (runReaderT (unRST (isCompTurn turn)) gameEnv) (GameState 0)                

checkGameOver :: Tree Ck.CkNode -> (Bool, String)
checkGameOver node =              
    case final $ rootLabel node of
        WWins -> (True, "White wins.")
        BWins -> (True, "Black wins.")
        Draw  -> (True, "Draw.")
        _     -> (False, "Message TBD")
                                                                                                      
isCompTurn :: Int -> RST Bool
isCompTurn turn = do
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    return $ if turn == 1 then p1 else p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

swapTurns :: Int -> Int
swapTurns t = 3-t   -- alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1

--convert +1, -1 to 1, 2
colorToTurn :: Int -> Int
colorToTurn 1 = 1
colorToTurn _ = 2

createMessage :: String -> Jsonable
createMessage s = Jsonable (J.jsonMessage s)

createUpdate :: String -> Ck.CkNode -> Jsonable
createUpdate msg node =  Jsonable $ J.jsonUpdate msg node (Ck.getPossibleMoves node)

createError :: String -> Jsonable
createError s = Jsonable (J.jsonError s)
