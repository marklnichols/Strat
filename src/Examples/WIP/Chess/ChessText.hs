{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module ChessText
    ( ChessText(..)
    ) where

import Control.Lens
import Control.Logger.Simple
import Control.Monad
import Data.List.Extra
import Data.Tree
import Data.Text (append, pack)
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
    updateBoard = showBoardBrief
    showCompMove _ = printMoveChoiceInfo
    getPlayerEntry _ = playerEntryText
    gameError = exitFail
    showAs _ showType node = showChessPosAs showType (_chessPos node)

printString :: String -> IO ()
printString = putStrLn

showBoardBrief :: ChessText -> ChessNode -> IO ()
showBoardBrief _ n = showBoard n

showBoard :: ChessNode -> IO ()
showBoard n = do
  let board = formatBoard n
  putStrLn $ "\n" ++ board
  logInfo $ "Board hash: " `append` pack (show (nodeHash n))

printMoveChoiceInfo :: Tree ChessNode -> NegaResult ChessNode -> Bool -> IO ()
printMoveChoiceInfo tree result loud = do
    let (tSize, tLevels)  = treeSize tree
    let evaluated = evalCount result
    let percentSaved = 1.0 - fromIntegral evaluated / fromIntegral (tSize-1) :: Float
    logInfo $ "Tree size: " `append` pack (show tSize)
    logInfo $ pack $ show tLevels
    logInfo $ pack $ printf "Evaluated: %d (percent saved by pruning: %f)"
                      evaluated percentSaved
    logInfo $ "Move with best score: " `append` pack (show (bestScore result))
    logInfo $ "(*) Computer's move: " `append` pack (show (picked result))
    putStrLn ("(*) Computer's move: " ++ show (picked result))

    let mv = getMove (nmNode (picked result))
    let n = rootLabel tree
    when (moveIsCheck (_chessPos n) mv) $ do
        putStrLn " (check)"
        logInfo " (check)"
    -- FIX: For the selcted move, this does NOT show the details of the score of the deepest node!
    logInfo $ "Score details: \n" `append` pack (showScoreDetails (_chessVal (last (nmMovePath (picked result)))))
    logInfo $ "Alternative moves:\n" `append` pack (intercalate "\n" (show <$> alternatives result))
    putStrLn ""

exitFail :: ChessText -> String -> IO ()
exitFail _ s = do
    logInfo $ pack s
    exitFailure

showChessPosAs :: String -> ChessPos -> IO ()
showChessPosAs showType cPos = do
    let str = case showType of
                  "FEN" -> "FEN representation of current position: \n" ++ toFen cPos ++ "\n"
                  s     -> "Unknown format: " ++ s
    putStrLn str
    logInfo $ pack str

---------------------------------------------------------------------------------------------------
-- Get player move, parsed from text input
---------------------------------------------------------------------------------------------------
playerEntryText :: Tree ChessNode -> [ChessMove] -> IO (Entry ChessMove s)
playerEntryText tree exclusions = do
    let n = rootLabel tree
    putStrLn "\n--------------------------------------------------\n"
    putStrLn "Enter player's move:"
    logInfo "Enter player's move:"
    line <- getLine
    -- logInfo $ pack line
    putStrLn ""
    case parseEntry n line of
        Left err -> do
            putStrLn err
            playerEntryText tree exclusions
        Right ce@(CmdEntry _) -> return ce
        Right me@(MoveEntry mv) ->
            if not (isLegal tree mv exclusions)
                then do
                    putStrLn "Not a legal move."
                    logInfo "Not a legal move."
                    playerEntryText tree exclusions
                else do
                    when (moveIsCheck (_chessPos n) mv) $ do
                        putStrLn "* (check) *"
                        logInfo "* (check) *"
                    return me

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
formatBoard :: ChessNode -> String
formatBoard n =
    let g = n ^. (chessPos . cpGrid)
        g' = unGrid g
    in (colLabels ++ loop g' 11 8 "") ++ "\n"
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
