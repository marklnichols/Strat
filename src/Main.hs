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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [isP1Comp, isP2Comp, dpth] | (p1, p2, depth) <- (toBool isP1Comp, toBool isP2Comp, toInt dpth)  ->
            loop getStartNode 1 p1 p2 depth
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <isP1Computer :: Bool> <isP2Computer :: Bool> <depth :: Int>"
  
-- :set prompt "ghci>"
-- :set args F F 4
-- Main.main
      
-- let node = getStartNode
-- let newTree = expandTree node 2 

-- let lenA = length $ subForest newTree
-- let lenB = length $ subForest ((subForest newTree) !! 0)
-- 
-- let lenD = length $ subForest ((subForest newTree) !! 2) 
-- let lenE = length $ subForest (subForest ((subForest newTree) !! 0) !! 0) 
   
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
                                putStrLn ("Enter player " ++ show turn ++ "'s move:")
                                line <- getLine
                                let n = read line
                                let processed = processMove node n
                                return processed
                            else do
                                putStrLn "Calculating computer move..."
                                let newTree = expandTree node depth
                                let moves = best newTree depth (turnToColor turn)
                                putStrLn "Move is ready, press a key to continue..."
                                let move = head $ fst moves
                                getLine
                                putStrLn ("Turn is: " ++ show turn)
                                putStrLn ("Move is: " ++ show move)
                                putStrLn ("Move value is: " ++ show (snd moves))
                                putStrLn ("Full move list is: " ++ show (fst moves))
                                let processed = processMove newTree move
                                --putStrLn $ format $ position $ getLabel processed
                                return processed
                return (Just nextNode)
    --TBD no need to pass p1 p2 depth around...
    case theNext of 
        Nothing -> return ()
        Just next -> loop next (swapTurns turn) p1 p2 depth
    

--isCompTurn :: current Turn -> isP1Computer -> isP2Computer -> True if current turn is computer generated
isCompTurn :: Int -> Bool -> Bool -> Bool
isCompTurn turn p1 p2 = if turn == 1 then p1 else p2

toBool :: String -> Bool
toBool s = not (s == "f" || s == "F")

toInt :: String -> Int
toInt = read

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1

{--
--getNextMove isHumanPlayer -> depth -> color -> Tree
getNextMove :: Bool -> Int -> Int -> Tree
getNextMove depth color True = humanMove color
getNextMove depth color False = computerMove depth color

--computerMove :: Tree -> depth -> color -> Tree
computerMove :: TreeNode n => Tree n -> Int -> Int -> Tree n
computerMove tree =
    let moves = best' tree depth color
        putStrLn "Computer move ready, hit return"
    etc...

--playerMove :: Tree -> color -> [Int]
playerMove :: TreeNode n => Tree n -> Int -> Tree n
       putStrLn "Input player move..."
       l <- getLine
       etc...
--}


{--
run :: (PositionNode n, Game n g) => g -> IO()
run g = do
    loop (startNode g) where
        loop :: PositionNode p => p -> IO()
        loop n = do
            putStrLn $ renderPosition g Node
            l <- getLine
            putStrLn $ "You typed: " ++ map toUpper l
            loop n
            --expTree = expandTree (Node n) depth, etc.



run2 posNode startNode depth = do
    tree = expandTree (Node startNode)

    loop do
        display position
        nextMove
        expandTree
        loop

    nextMove
        get move input
        or calc via best tree
--}



{--
from stack overflow:
main :: IO ()
main = do
  args <- getArgs
  case args of
    [aString, aInteger] | [(n,_)] <- reads aInteger  -> doStuffWith aString n
    _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <string> <integer>"
            exitFailure
--}


{--
re: Pattern guards in Haskell 2010:
From the GHC user's guide,

lookup :: FiniteMap -> Int -> Maybe Int

addLookup env var1 var2
   | Just val1 <- lookup env var1
   , Just val2 <- lookup env var2
   = val1 + val2
{-...other equations...-}
--}
