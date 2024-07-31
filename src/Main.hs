{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Logger.Simple
import Data.Text(Text, append, pack)
import System.Console.CmdArgs.Implicit
-- import Text.Printf

import Checkers
import Chess
import ChessText
import GameRunner
import CheckersText
import StratWeb.YesodMain

logConfig :: LogConfig
logConfig = LogConfig
    { lc_file = Just "strat.log"
    , lc_stderr = True }

main :: IO ()
main = do
    withGlobalLogging logConfig $ do
      theArgs@StratArgs{..} <- cmdArgs stratArgs
      setLogLevelStr $ pack logLevel
      case exampleName of
        "chess" -> do
            -- printf "restoreGame: %s\n" restoreGame
            -- printf "loadFromFed: %s\n" loadFromFen
            if restoreGame /= "newgame" && loadFromFen /= ""
              then do
                putStrLn "Choose either --restoreGame or --fen, but not both"
                print theArgs
              else do
                pairMay <-
                    if loadFromFen == ""
                      then do
                          logInfo $ "Starting game: " `append` pack restoreGame
                          return $ Just $ Chess.getStartNode restoreGame
                      else do
                        logInfo $ "Loading from FEN: " `append` pack loadFromFen
                        case Chess.getNodeFromFen loadFromFen of
                          Left err -> do
                            putStrLn "Invalid FEN format"
                            print err
                            return Nothing
                          Right (startNode, startState) -> do
                            return $ Just (startNode, startState)
                case pairMay of
                  Nothing -> do
                    putStrLn "Invalid FEN format"
                    print theArgs
                  Just (startNode, startState) -> do
                    GameRunner.startGame ChessText startNode startState depth critDepth aiPlaysWhite
                        aiPlaysBlack preSortOn (not noRandom) (not noPruning) singleThreaded
                        pruneTracing cmpTracing (pack traceStr)
        "checkers" -> do
            let (startNode, startState) = Checkers.getStartNode restoreGame
            GameRunner.startGame CheckersText startNode startState depth critDepth aiPlaysWhite aiPlaysBlack
              preSortOn (not noRandom) (not noPruning) singleThreaded pruneTracing cmpTracing (pack traceStr)
        "checkersWeb" -> webInit
        s ->  putStrLn (s ++ " is not a valid example name. Valid choices: chess, checkers, and checkersWeb")

setLogLevelStr :: Text -> IO ()
setLogLevelStr s
  | s == (pack . show) LogTrace = setLevel LogTrace
  | s == (pack . show) LogDebug = setLevel LogDebug
  | s == (pack . show) LogInfo  = setLevel LogInfo
  | s == (pack . show) LogNote  = setLevel LogNote
  | s == (pack . show) LogWarn  = setLevel LogWarn
  | s == (pack . show) LogError = setLevel LogError
  | otherwise                   = return ()

setLevel :: LogLevel -> IO ()
setLevel l = do
  putStrLn $ "setting log level: " ++ show l
  setLogLevel l

-- TODO: remove pruneTracing and cmpTracing and implement via logging
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
  , logLevel :: String
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
  , logLevel = "LogError" &= name "log" &= help "The logging level. One of LogTrace, LogDebug, LogInfo, LogNote, LogWarn, or LogError (LogError is default)"
  }
