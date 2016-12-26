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

--TODO: fix -- to get things working, for the moment this web piece is checkers specific

{-  Outline of flow -- to be deleted
 /newGame
    create new board / tree
    respond with new board, 1st move if computer plays first
 /playerMove
    update board with move
    if game over, send message

    swapTurn (e.g. was loop next (swapTurns turn))
    and:
      make computer move
      update board again
      return all with new board, move, etc.
         (if game over, include in message inside update)
-}

--TODO: move to common...
gameEnv :: Env
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------         
data Jsonable = forall j. ToJSON j => Jsonable j 
  
--start game request received  
processStartGame :: Tree Ck.CkNode -> IO Jsonable
processStartGame node = 
    if compMoveNext node 1
        then computerResponse node 1 
        else return $ createUpdate "New Game, player moves first" (rootLabel node)                  
                
--play move (web request) received
processPlayerMove :: Tree Ck.CkNode -> Ck.CkMove -> IO Jsonable
processPlayerMove tree mv = do
    let processed = processMove tree mv
    let done = checkGameOver processed
    if fst done
        then return $ createMessage (snd done) 
        else computerResponse processed 1 --TODO: replace turn swapping

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
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1

createMessage :: String -> Jsonable
createMessage s = Jsonable (J.jsonMessage s)

createUpdate :: String -> Ck.CkNode -> Jsonable
createUpdate msg node =  Jsonable $ J.jsonUpdate msg node (Ck.getPossibleMoves node)

createError :: String -> Jsonable
createError s = Jsonable (J.jsonError s)
