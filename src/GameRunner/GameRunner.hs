{-# LANGUAGE MultiParamTypeClasses #-}

module GameRunner where

import StratTree.TreeNode
import StratTree.StratTree
import StratTree.Trees
import StratIO.StratIO
import Data.Tree
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens

gameEnv :: Env  
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

startGame :: (Output o n m, PositionNode n m e) => o -> Tree n -> IO ()
startGame o node = loop o node 1

loop :: (Output o n m, PositionNode n m e) => o -> Tree n -> Int -> IO ()
loop o node turn = do    
    
    --putStrLn showPosition $ rootLabel node
    updateBoard o $ rootLabel node
    --putStrLn ("Current position score: " ++ show (getValue (rootLabel node)))
    --putStrLn ""
    
    theNext <- case final $ rootLabel node of
        WWins -> do
            out o "White wins."
            return Nothing
        BWins -> do
            out o "White wins."
            return Nothing
        Draw -> do
            out o "Draw."
            return Nothing
        _ -> do
            nextNode <- if evalState (runReaderT (unRST (isCompTurn turn)) gameEnv) (GameState 0)
                            then computerMove o node turn
                            else playerMove o node turn
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop o next (swapTurns turn)

{-
playerMove :: PositionNode n m e => Tree n -> Int -> IO (Tree n)
playerMove tree turn = do
    out ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    out ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            out err
            playerMove tree turn
        Right mv -> if not (isLegal tree mv)
            then do
                out "Not a legal move."
                playerMove tree turn
            else return (processMove tree mv)
-}
playerMove :: (Output o n m, PositionNode n m e) => o -> Tree n -> Int -> IO (Tree n)
playerMove o tree turn = do
    mv <- getPlayerMove o tree turn
    return (processMove tree mv)
            
{-          
computerMove :: PositionNode n m e => Tree n -> Int -> IO (Tree n)
computerMove node turn = do
    putStrLn "Calculating computer move..."
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> do
            putStrLn "Invalid result returned from best"
            exitFailure
        Just result -> do
            let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            let badMoves = evalState (runReaderT (unRST badMovesM) gameEnv) (GameState 0)
            let finalChoices = fromMaybe (result ^. moveScores) badMoves
            putStrLn ("Choices from best: " ++ show (result^.moveScores))
            putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
            moveM <- resolveRandom finalChoices
            case moveM of
                Nothing -> do
                    putStrLn "Invalid result from resolveRandom"
                    exitFailure
                Just mv -> do
                    printMoveChoiceInfo newTree result mv
                    return (processMove newTree mv)
-}
computerMove :: (Output o n m, PositionNode n m e) => o -> Tree n -> Int -> IO (Tree n)
computerMove o node turn = do
    out o "Calculating computer move..."
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> do 
            gameError o "Invalid result returned from best"
            return newTree
        Just result -> do
            let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            let badMoves = evalState (runReaderT (unRST badMovesM) gameEnv) (GameState 0)
            let finalChoices = fromMaybe (result ^. moveScores) badMoves
            out o ("Choices from best: " ++ show (result^.moveScores))
            out o ("Choices after checkBlunders: " ++ show finalChoices)
            moveM <- resolveRandom finalChoices
            case moveM of
                Nothing -> do
                    gameError o "Invalid result from resolveRandom"
                    return newTree
                Just mv -> do
                    printMoveChoiceInfo o newTree result mv
                    return (processMove newTree mv)
                     

printMoveChoiceInfo :: (Output o n m, PositionNode n m e) => o -> Tree n -> Result m e -> m -> IO ()
printMoveChoiceInfo o tree result mv = do
    out o ("Tree size: " ++ show (treeSize tree))
    out o ("Equivalent best moves: " ++ show (result^.moveChoices))
    out o ("Following moves: " ++ show ( result^.followingMoves))
    out o ("Computer's move:\n (m:" ++ show mv ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    out o ""

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
