module Main where
import TicTac
import Checkers
import StratTree.TreeNode
import StratTree.StratTree
import StratTree.Trees

import StratIO.StratIO

import System.Environment
import System.Exit
import Data.Tree
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens

-- :set args "tictac"
-- :set args "checkers"
-- :set args "chess"
-- Main.main

gameEnv :: Env
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0,               _errorEquivThreshold = 0, _p1Comp = False, _p2Comp = True}

main :: IO ()
main = do
    a <- getArgs
    parse a
    return ()

parse :: [String] -> IO ()
parse ["tictac"]   = loop getTicTacStart 1
parse ["checkers"] = loop getCheckersStart 1
parse _            = putStrLn "Usage: main tictac | checkers"

getTicTacStart :: Tree TTNode
getTicTacStart = TicTac.getStartNode

getCheckersStart :: Tree CkNode
getCheckersStart = Checkers.getStartNode

loop :: PositionNode n m e => Tree n -> Int -> IO ()
loop node turn = do
    putStrLn $ showPosition $ rootLabel node
    putStrLn ("Current position score: " ++ show (getValue (rootLabel node)))
    putStrLn ""
    theNext <- case final $ rootLabel node of
        WWins -> do
            putStrLn "White wins."
            return Nothing
        BWins -> do
            putStrLn "White wins."
            return Nothing
        Draw -> do
            putStrLn "Draw."
            return Nothing
        _ -> do
            nextNode <- if evalState (runReaderT (unRST (isCompTurn turn)) gameEnv) (GameState 0)
                            then computerMove node turn
                            else  playerMove node turn
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop next (swapTurns turn)

playerMove :: PositionNode n m e => Tree n -> Int -> IO (Tree n)
playerMove tree turn = do
    putStrLn ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    putStrLn ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            putStrLn err
            playerMove tree turn
        Right mv -> if not (isLegal tree mv)
            then do
                putStrLn "Not a legal move."
                playerMove tree turn
            else return (processMove tree mv)

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

--printMoveChoiceInfo :: (Eval e, Move m) => Result m e -> m -> IO ()
printMoveChoiceInfo :: PositionNode n m e => Tree n -> Result m e -> m -> IO ()
printMoveChoiceInfo tree result mv = do
    putStrLn ("Tree size: " ++ show (treeSize tree))
    putStrLn ("Equivalent best moves: " ++ show (result^.moveChoices))
    putStrLn ("Following moves: " ++ show ( result^.followingMoves))
    putStrLn ("Computer's move:\n (m:" ++ show mv ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    putStrLn ""

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
