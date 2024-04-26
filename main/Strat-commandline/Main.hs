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
import Text.Printf

main :: IO ()
main = do
    theArgs@StratArgs{..} <- cmdArgs stratArgs
    verbose <- isLoud
    case exampleName of
      "chess" -> do
          printf "restoreGame: %s\n" restoreGame
          printf "loadFromFed: %s\n" loadFromFen
          if restoreGame /= "newgame" && loadFromFen /= ""
            then do
              putStrLn "Choose either --restoreGame or --fen, but not both"
              print theArgs
            else do
              pairMay <-
                  if loadFromFen == ""
                    then return $ Just $ Chess.getStartNode restoreGame
                    else do
                      case Chess.getNodeFromFen loadFromFen of
                        Left err -> do
                          putStrLn "Invalid FEN format"
                          print err
                          return Nothing
                        Right (startNode, startState) -> do
                          putStrLn "FEN seems ok? (A)"
                          return $ Just (startNode, startState)
              case pairMay of
                Nothing -> do
                  putStrLn "Invalid FEN format"
                  print theArgs
                Just (startNode, startState) -> do
                  putStrLn "FEN seems ok? (b)"
                  GameRunner.startGame ChessText startNode startState depth critDepth aiPlaysWhite
                      aiPlaysBlack preSortOn (not noRandom) (not noPruning) singleThreaded verbose
                      pruneTracing cmpTracing (pack traceStr)
      "checkers" -> do
          let (startNode, startState) = Checkers.getStartNode restoreGame
          GameRunner.startGame CheckersText startNode startState depth critDepth aiPlaysWhite aiPlaysBlack
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
  , loadFromFen :: String
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
  , loadFromFen = "" &= name "fen" &= help "Load a game using FEN (Forsyth-Edwards Notation)"
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
