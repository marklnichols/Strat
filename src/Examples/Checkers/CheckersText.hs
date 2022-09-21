{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersText
    ( CheckersText(..)
    ) where

import Checkers
import Control.Lens
import Control.Monad
import Data.List
import Data.Tree
import Strat.Helpers
import Strat.ZipTree
import Strat.StratTree.TreeNode
import System.Exit
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as V

data CheckersText = CheckersText

instance Output CheckersText CkNode CkMove where
    out _ = printString
    updateBoard = showBoard
    showCompMove _ = printMoveChoiceInfo
    getPlayerEntry _ = playerEntry
    gameError = exitFail

printString :: String -> IO ()
printString = putStrLn

showBoard :: CheckersText -> CkNode -> IO ()
showBoard _ node = do
    putStrLn $ formatBoard node
    putStrLn ("Current position score: " ++ show (evaluate node))
    putStrLn ("Current position score: \n" ++ showScoreDetails (_ckValue node))
    putStrLn "\n--------------------------------------------------\n"

printMoveChoiceInfo :: Tree CkNode -> NegaResult CkNode -> Bool -> IO ()
printMoveChoiceInfo tree result verbose = do
    putStrLn ("Tree size: " ++ show (treeSize tree))
    putStrLn ("Computer's move: " ++ showNegaMoves (picked result))
    when verbose $ do
        putStrLn ("score details: \n"
                 ++ showScoreDetails (_ckValue (evalNode (picked result))))
        putStrLn ("Alternative moves:\n" ++ intercalate "\n"
                 (showNegaMoves <$> alternatives result))
        putStrLn ""

exitFail :: CheckersText -> String -> IO ()
exitFail _ s = do
    putStrLn s
    exitFailure

---------------------------------------------------------------------------------------------------
-- Get player move, parsed from text input
---------------------------------------------------------------------------------------------------
playerEntry :: Tree CkNode -> [CkMove] -> IO (Entry CkMove s)
playerEntry tree exclusions = do
    let node = rootLabel tree
    putStrLn ("Enter player's move:")
    line <- getLine
    putStrLn ""
    case parseEntry node line of
        Left err -> do
            putStrLn err
            playerEntry tree exclusions
        Right (CmdEntry s) -> do
                    putStrLn ("Command!: " ++ s)
                    playerEntry tree exclusions
        Right me@(MoveEntry mv) ->
            if not (isLegal tree mv exclusions)
                then do
                    putStrLn "Not a legal move."
                    playerEntry tree exclusions
                else return me

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
formatBoard :: CkNode -> String
formatBoard node = loop (unGrid (node ^. (ckPosition . grid))) 40 "" where
    loop _ 4 result = result ++ "\n" ++ colLabels
    loop xs n result = loop xs (newIdx - 4) (result ++ rowToStr xs newIdx spaces) where
        (newIdx, spaces) = case n `mod` 9 of
            0 -> (n-1, "")
            4 -> (n, "   ")
            _ -> (n, "   ") --should never happen

rowToStr :: V.Vector Int -> Int -> String -> String
rowToStr xs i spaces =  Map.findWithDefault "??" i labelMap ++ "  " ++ spaces ++
                            toXOs (xs V.! (i-3)) ++ gap ++
                            toXOs (xs V.! (i-2)) ++ gap ++
                            toXOs (xs V.! (i-1)) ++ gap ++
                            toXOs (xs V.!    i)  ++ "\n"

gap :: String
gap = "     "

toXOs :: Int -> String
toXOs 1 = "x"
toXOs (-1) = "o"
toXOs 2 = "X"
toXOs (-2) = "O"
toXOs 0 = "-"
toXOs _ = "?"

labelMap :: Map.Map Int String
labelMap = Map.fromList [(40, "8"), (35, "7"), (31, "6"), (26, "5"), (22, "4"), (17, "3"), (13, "2"), (8, "1")]

colLabels :: String
colLabels = "   " ++ intercalate "  " ["A", "B", "C", "D", "E", "F", "G", "H"]
