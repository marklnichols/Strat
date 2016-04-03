module Main where 
import TicTac.TicTac
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
-- Main.main

--TODO move command line args to reader monad
main :: IO ()
main = loop getStartNode 1
    {--
    args <- getArgs
    case args of
        [p1Type, p2Type, dpth] | (p1, p2, depth) <- (toBool p1Type, toBool p2Type, toInt dpth)  ->
            loop getStartNode 1 p1 p2 
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <isP1Computer :: Bool> <isP2Computer :: Bool> <depth :: Int>"
    --}

loop :: Tree TTNode -> Int -> IO ()
loop node turn = do
    putStrLn $ format $ _ttPosition $ rootLabel node
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
                then do
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
                            putStrLn ("Choices before checkBlunders: " ++ show (_moveScores result))
                            putStrLn ("Choices after checkBlunders: " ++ show finalChoices)
                            --moveM <- resolveRandom $ _moveChoices result
                            moveM <- resolveRandom finalChoices
                            case moveM of
                                Nothing -> do
                                    putStrLn "Invalid result from resolveRandom"
                                    exitFailure
                                Just move -> do    
                                    let processed = processMove newTree move
                                    putStrLn ("Move is ready: " ++ show move)
                                    putStrLn ("Move value is: " ++ show (_score $ head $ _moveScores result))
                                    putStrLn ("List of equivalent best moves: " ++ show (_moveChoices result))
                                    putStrLn ("Following moves are: " ++ show ( _followingMoves result ))
                                    putStrLn "Press return to continue..."
                                    getLine
                                    return processed
                else do
                    putStrLn ("Enter player " ++ show turn ++ "'s move:")
                    line <- getLine
                    putStrLn ""
                    let n = posToMove (read line) turn 
                    let processed = processMove node n
                    return processed    
            return (Just nextNode)
    case theNext of 
        Nothing -> return ()
        Just next -> loop next (swapTurns turn)
    
isCompTurn :: Int -> Reader Env Bool
isCompTurn turn = do 
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    if turn == 1 then return p1 else return p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

-- convert input player move 1-9 to (+/-) as per player 1/2
--posToMove :: input position -> turn -> move
posToMove :: Int -> Int -> Int
posToMove index turn = turnToColor turn * index 

toInt :: String -> Int
toInt = read

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1
