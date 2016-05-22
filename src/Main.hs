module Main where 
import TicTac.TicTac
import Checkers
import TicTac.TicTacEnv
import System.Environment
import System.IO
import System.Exit
import StratTree.TreeNode
import StratTree.StratTree
import Data.Tree
import Data.Tree.Zipper
import Data.List
import Data.Maybe
import Control.Monad
import Data.Tuple.Select
import StratIO.StratIO
import Control.Monad.Reader

-- :set prompt "ghci>"
-- StratTree.StratTreeTest.main
-- TicTac.TicTacTest.main
-- :set args "tictac"
-- Main.main

--TODO: delete TicTacEnv class
gameEnv = Env {_depth = 5, _errorDepth = 5, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}

--TODO move command line args to reader monad
main :: IO () 
main = do 
    a <- getArgs
    parse a 
    return ()
    --loop getStartNode 1
 
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
            nextNode <- if runReader (isCompTurn turn) ticTacEnv 
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
    --let mv = posToMove (read line) turn
    let node = rootLabel tree
    let mv = parseMove node line
    let legal = isLegal tree mv
    if not legal 
        then do 
            putStrLn "Not a legal move."
            playerMove tree turn 
        else return (processMove tree mv) 
  
computerMove :: PositionNode n m => Tree n -> Int -> IO (Tree n)
computerMove node turn = do 
    putStrLn "Calculating computer move..."
    let newTree = runReader (expandTree node) ticTacEnv 
    let resultM = runReader (best newTree (turnToColor turn)) ticTacEnv
    case resultM of 
        Nothing -> do
            putStrLn "Invalid result returned from best"
            exitFailure
        Just result -> do
            let badMovesM = checkBlunders newTree (turnToColor turn) (_moveScores result)
            let finalChoices = runReader badMovesM ticTacEnv
            putStrLn ("Choices from best: " ++ show (_moveScores result))
            putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
            moveM <- resolveRandom finalChoices
            case moveM of
                Nothing -> do
                    putStrLn "Invalid result from resolveRandom"
                    exitFailure
                Just move -> do    
                    --let processed = 
                    printMoveChoiceInfo result move 
                    return (processMove newTree move)

printMoveChoiceInfo :: Move m => Result m -> m -> IO ()
printMoveChoiceInfo result move = do
    putStrLn ("Computer's move : (m:" ++ show move ++
                  ", s:" ++ show (_score $ head $ _moveScores result) ++ ")")
    --putStrLn ("Move value is: " ++ show (_score $ head $ _moveScores result))
    putStrLn ("Equivalent best moves: " ++ show (_moveChoices result))
    putStrLn ("Following moves: " ++ show ( _followingMoves result ))
    putStrLn ""

isCompTurn :: Int -> Reader Env Bool
isCompTurn turn = do 
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    if turn == 1 then return p1 else return p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

--TODO: this needs to be generalized beyond TTT
-- convert input player move 1-9 to (+/-) as per player 1/2
--posToMove :: input position -> turn -> move
--posToMove :: Int -> Int -> IntMove
--posToMove index turn = IntMove turnToColor turn * index 

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1
