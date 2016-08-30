module Main where
import TicTac.TicTac
import Checkers
import StratTree.TreeNode
import StratTree.StratTree
import StratIO.StratIO

import System.Environment
import System.Exit
import Data.Tree
import Data.Maybe
import Control.Monad.Reader
import Control.Lens

-- :set args "tictac"
-- :set args "checkers"
-- Main.main

gameEnv = Env {_depth = 5, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

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
getTicTacStart = TicTac.TicTac.getStartNode

getCheckersStart :: Tree CkNode
getCheckersStart = Checkers.getStartNode

loop :: PositionNode n m => Tree n -> Int -> IO ()
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
            nextNode <- if runReader (isCompTurn turn) gameEnv
                            then computerMove node turn
                            else playerMove node turn
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop next (swapTurns turn)

playerMove :: PositionNode n m => Tree n -> Int -> IO (Tree n)
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

computerMove :: PositionNode n m => Tree n -> Int -> IO (Tree n)
computerMove node turn = do
    putStrLn "Calculating computer move..."
    let newTree = runReader (expandTree node) gameEnv
    let resultM = runReader (best newTree (turnToColor turn)) gameEnv
    case resultM of
        Nothing -> do
            putStrLn "Invalid result returned from best"
            exitFailure
        Just result -> do
            let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            let badMoves = runReader badMovesM gameEnv
            let finalChoices = fromMaybe (result ^. moveScores) badMoves
            putStrLn ("Choices from best: " ++ show (result^.moveScores))
            putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
            moveM <- resolveRandom finalChoices
            case moveM of
                Nothing -> do
                    putStrLn "Invalid result from resolveRandom"
                    exitFailure
                Just move -> do
                    printMoveChoiceInfo result move
                    return (processMove newTree move)

printMoveChoiceInfo :: Move m => Result m -> m -> IO ()
printMoveChoiceInfo result move = do
    putStrLn ("Equivalent best moves: " ++ show (result^.moveChoices))
    putStrLn ("Following moves: " ++ show ( result^.followingMoves))
    putStrLn ("Computer's move:\n (m:" ++ show move ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    putStrLn ""

isCompTurn :: Int -> Reader Env Bool
isCompTurn turn = do
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    if turn == 1 then return p1 else return p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1
