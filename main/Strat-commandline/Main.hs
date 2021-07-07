{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Checkers
import Chess
import ChessText
import GameRunner
import CheckersText
import System.Console.CmdArgs.Implicit
-- import System.Environment
import StratWeb.YesodMain

main :: IO ()
main = do
    theArgs@StratArgs{..} <- cmdArgs stratArgs
    case exampleName of
      "chess" -> do
          let start = Chess.getStartNode restoreGame
          GameRunner.startGame ChessText start depth
      "checkers" -> do
          let start = Checkers.getStartNode restoreGame
          GameRunner.startGame CheckersText start depth

      "checkersWeb" -> webInit
      _ -> print theArgs

data StratArgs = StratArgs
  { exampleName :: String
  , depth :: Int
  , restoreGame :: String }
  deriving (Show, Data, Typeable)

stratArgs :: StratArgs
stratArgs = StratArgs
  { exampleName = "chess" &= name "n" &= help "The example to run"
  , depth = 4 &= help "Tree search depth"
  , restoreGame = "newgame" &= help "Game name to restore"}
