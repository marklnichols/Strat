{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module ChessText
    ( ChessText(..)
    ) where

import Control.Lens
import Control.Monad
import Data.List.Extra
import Data.Tree
import Strat.Helpers
import Strat.ZipTree
import Strat.StratTree.TreeNode
import System.Exit
import qualified Data.Vector.Unboxed as V
import Text.Printf

import Chess

data ChessText = ChessText

instance Output ChessText ChessNode ChessMove where
    out _ = printString
    updateBoard = showBoard
    showCompMove _ = printMoveChoiceInfo
    getPlayerEntry _ = playerEntryText
    gameError = exitFail

printString :: String -> IO ()
printString = putStrLn

showBoard :: ChessText -> ChessNode -> IO ()
showBoard _ node = do
    putStrLn $ formatBoard node
    putStrLn ("Board hash: " ++ show (nodeHash node))
    putStrLn ("Current position score: \n" ++ showScoreDetails (_chessVal node))

printMoveChoiceInfo :: Tree ChessNode -> NegaResult ChessNode -> Bool -> IO ()
printMoveChoiceInfo tree result loud = do
    let (tSize, tLevels)  = treeSize tree
    let evaluated = evalCount result
    let percentSaved = 1.0 - fromIntegral evaluated / fromIntegral (tSize-1) :: Float
    putStrLn ("Tree size: " ++ show tSize)
    putStrLn (show tLevels)
    putStrLn $ printf "Evaluated: %d (percent saved by pruning: %f)"
                      evaluated percentSaved
    putStrLn ("Computer's move: " ++ showNegaMoves (picked result))

    let mv = getMove (moveNode(picked result))
    let node = rootLabel tree
    when (moveChecksOpponent node mv) $ do
        putStrLn " (check)"
    when loud $ do
        putStrLn ("Score details: \n"
                 ++ showScoreDetails (_chessVal (evalNode (picked result))))
        putStrLn ("Move with best score: " ++ showNegaMoves (bestScore result))
        putStrLn ("Alternative moves:\n" ++ intercalate "\n"
                 (showNegaMoves <$> alternatives result))
        putStrLn ""

exitFail :: ChessText -> String -> IO ()
exitFail _ s = do
    putStrLn s
    exitFailure

---------------------------------------------------------------------------------------------------
-- Get player move, parsed from text input
---------------------------------------------------------------------------------------------------
playerEntryText :: Tree ChessNode -> [ChessMove] -> IO (Entry ChessMove s)
playerEntryText tree exclusions = do
    let node = rootLabel tree
    putStrLn "Enter player's move:"
    line <- getLine
    putStrLn ""
    case parseEntry node line of
        Left err -> do
            putStrLn err
            playerEntryText tree exclusions
        Right ce@(CmdEntry _) -> return ce
        Right me@(MoveEntry mv) ->
            if not (isLegal tree mv exclusions)
                then do
                    putStrLn "Not a legal move."
                    playerEntryText tree exclusions
                else do
                    when (moveChecksOpponent node mv) $
                        putStrLn " (check)"
                    return me

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
formatBoard :: ChessNode -> String
formatBoard node =
    let g = node ^. (chessPos . cpGrid)
        g' = unGrid g
    in "\n" ++ (colLabels ++ loop g' 11 8 "") ++ "\n"
  where
    loop :: V.Vector Char -> Int -> Int -> String -> String
    loop _ _ 0 dest = dest
    loop src nDrop rows dest =
      let newHead = V.drop nDrop src
          pieces = padChars $ V.toList $ V.take 8 newHead
          newDest = "\n" ++ show (8 - rows + 1) ++ "  " ++ pieces ++ dest
      in loop newHead 10 (rows - 1) newDest

padChars :: String -> String
padChars src =
    concat $ repeatedly f src
  where
    f [] = ([], [])
    f (x:xs) = (x : "  ", xs)

colLabels :: String
colLabels = "   A  B  C  D  E  F  G  H"

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   k   q   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   P   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   K   Q   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}
