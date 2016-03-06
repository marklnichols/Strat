module Main where 
import TicTac.TicTac
import System.Environment
import System.IO
import System.Exit
import StratTree.TreeNode
import StratTree.StratTree
import Data.Tree
import Data.Tree.Zipper
import Control.Monad
import Data.Tuple.Select

main :: IO ()
main = do
    args <- getArgs
    case args of
        [p1Type, p2Type, dpth] | (p1, p2, depth) <- (toBool p1Type, toBool p2Type, toInt dpth)  ->
            loop getStartNode 1 p1 p2 depth
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <isP1Computer :: Bool> <isP2Computer :: Bool> <depth :: Int>"
  
-- :set prompt "ghci>"

-- StratTree.StratTreeTest.main
-- TicTac.TicTacTest.main

-- :set args c h 4
-- Main.main
   
--loop :: Starting node -> currentTurn -> isP1Computer -> isP2Computer -> depth -> IO ()
loop :: Tree TTNode -> Int -> Bool -> Bool -> Int -> IO ()
loop node turn p1 p2 depth = do
    putStrLn $ format $ position $ rootLabel node
    theNext <- case (final $ rootLabel node) of  
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
                nextNode <- if isCompTurn turn p1 p2
                            then do
                                putStrLn "Calculating computer move..."
                                let newTree = expandTree node depth
                                let resultM = best newTree depth (turnToColor turn)
                                
                                case resultM of 
                                    Nothing     -> do
                                        putStrLn "Invalid result returned from best"
                                        exitFailure
                                    Just result -> do
                                        let move = head $ _moveChoices result
                                        let processed = processMove newTree move
                                        putStrLn ("Move is ready: " ++ show move)
                                        
                                        putStrLn ("Move value is: " ++ show (_score $ head $ _moveScores result))
                                        putStrLn ("Following moves are: " ++ show ( _followingMoves result ))
                                        putStrLn "Press return to continue..."
                                        getLine
                                        return processed
                            else do
                                putStrLn ("Enter player " ++ show turn ++ "'s move:")
                                line <- getLine
                                let n = posToMove (read line) turn 
                                let processed = processMove node n
                                return processed    
                return (Just nextNode)
    --TBD no need to pass p1 p2 depth around...
    case theNext of 
        Nothing -> return ()
        Just next -> loop next (swapTurns turn) p1 p2 depth
    
--isCompTurn :: current Turn -> isP1Computer -> isP2Computer -> True if current turn is computer generated
isCompTurn :: Int -> Bool -> Bool -> Bool
isCompTurn turn p1 p2 = if turn == 1 then p1 else p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = (s == "c" || s == "C")

-- convert input player move 1-9 to (+/-) as per player 1/2
--posToMove :: input position -> turn -> move
posToMove :: Int -> Int -> Int
posToMove index turn = (turnToColor turn) * index 

toInt :: String -> Int
toInt = read

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1
