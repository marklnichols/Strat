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
          let start = Chess.getStartNode restoreGame nextMoveColor
          GameRunner.startGame ChessText start depth aiPlaysWhite aiPlaysBlack (not noRandom)
      "checkers" -> do
          let start = Checkers.getStartNode restoreGame
          GameRunner.startGame CheckersText start depth aiPlaysWhite aiPlaysBlack (not noRandom)

      "checkersWeb" -> webInit
      _ -> print theArgs

-- START HERE -- add arg for useRandom, pass to getStartNode
data StratArgs = StratArgs
  { exampleName :: String
  , depth :: Int
  , noRandom :: Bool
  , restoreGame :: String
  , nextMoveColor :: Color }
  deriving (Show, Data, Typeable)

stratArgs :: StratArgs
stratArgs = StratArgs
  { exampleName = "chess" &= name "n" &= help "The example to run"
  , depth = 4 &= help "Tree search depth"
  , noRandom = True &= name "nr" &= help "Turn off randomness used in the computer's move selection"
  , restoreGame = "newgame" &= help "Game name to restore"
  , nextMoveColor = White &= name "c" &= help "Color to move next (White | Black)"}

--TODO: make this configurable
aiPlaysWhite, aiPlaysBlack :: Bool
aiPlaysWhite = False
aiPlaysBlack = True
