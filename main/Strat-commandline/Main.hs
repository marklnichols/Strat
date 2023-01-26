{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Checkers
import Chess
import ChessText
import GameRunner
import CheckersText
import Data.Text(pack)
import System.Console.CmdArgs.Implicit
import StratWeb.YesodMain

main :: IO ()
main = do
    theArgs@StratArgs{..} <- cmdArgs stratArgs
    verbose <- isLoud
    case exampleName of
      "chess" -> do
          let start = Chess.getStartNode restoreGame nextMoveColor
          GameRunner.startGame ChessText start depth critDepth aiPlaysWhite aiPlaysBlack preSortOn
                         (not noRandom) (not noPruning) singleThreaded verbose pruneTracing cmpTracing (pack traceStr)
      "checkers" -> do
          let start = Checkers.getStartNode restoreGame
          GameRunner.startGame CheckersText start depth critDepth aiPlaysWhite aiPlaysBlack
            preSortOn (not noRandom) (not noPruning) singleThreaded verbose pruneTracing cmpTracing (pack traceStr)
      "checkersWeb" -> webInit
      _ -> print theArgs


-- TODO: remove pruneTracing and cmpTracing and implement via Verbosity
data StratArgs = StratArgs
  { exampleName :: String
  , depth :: Int
  , critDepth :: Int
  , noRandom :: Bool
  , noPruning :: Bool
  , restoreGame :: String
  , nextMoveColor :: Color
  , aiPlaysWhite :: Bool
  , aiPlaysBlack :: Bool
  , preSortOn :: Bool
  , pruneTracing :: Bool
  , cmpTracing :: Bool
  , traceStr :: String
  , singleThreaded :: Bool
  }
  deriving (Show, Data, Typeable)

stratArgs :: StratArgs
stratArgs = StratArgs
  { exampleName = "chess" &= name "n" &= help "The example to run"
  , depth = 4 &= help "Tree search depth"
  , critDepth = 6 &= help "Tree search depth for critical moves"
  , noRandom = False &= name "nr" &= help "Turn off randomness used in the computer's move selection"
  , noPruning = False &= name "p" &= help "Turn off alpha-beta pruning (for debugging only)"
  , restoreGame = "newgame" &= help "Game name to restore"
  , nextMoveColor = White &= name "c" &= help "Color to move next (White | Black)"
  , aiPlaysWhite = False &= name "white-ai" &= help "The computer plays the white pieces"
  , aiPlaysBlack = True &= name "black-ai" &= help "The computer plays the black pieces"
  , preSortOn = False &= name "presort" &= help "Enable shallow tree sort before the full eval - (experimental)"
  , pruneTracing = False &= name "ptracing" &= help "Output prune tracing if traceStr is found in the trace output"
  , cmpTracing = False &= name "ctracing" &= help "Output negaMax cmp tracing if traceStr is found in the trace output"
  , traceStr = "" &= name "tracestr" &= help "String to search for in trace output"
  , singleThreaded = False &= name "st" &= help "Turn off multi threading (for debugging only)"
  } &=
    verbosity
