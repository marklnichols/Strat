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
          GameRunner.startGame ChessText start depth aiPlaysWhite aiPlaysBlack (not noRandom) (not noPruning)
      "checkers" -> do
          let start = Checkers.getStartNode restoreGame
          GameRunner.startGame CheckersText start depth aiPlaysWhite aiPlaysBlack (not noRandom) (not noPruning)
      "checkersWeb" -> webInit
      _ -> print theArgs

data StratArgs = StratArgs
  { exampleName :: String
  , depth :: Int
  , noRandom :: Bool
  , noPruning :: Bool
  , restoreGame :: String
  , nextMoveColor :: Color
  , aiPlaysWhite :: Bool
  , aiPlaysBlack :: Bool }
  deriving (Show, Data, Typeable)

stratArgs :: StratArgs
stratArgs = StratArgs
  { exampleName = "chess" &= name "n" &= help "The example to run"
  , depth = 4 &= help "Tree search depth"
  , noRandom = False &= name "nr" &= help "Turn off randomness used in the computer's move selection"
  , noPruning = False &= name "p" &= help "Turn off alpha-beta pruning (just for debugging)"
  , restoreGame = "newgame" &= help "Game name to restore"
  , nextMoveColor = White &= name "c" &= help "Color to move next (White | Black)"
  , aiPlaysWhite = False &= name "white-ai" &= help "The computer plays the white pieces"
  , aiPlaysBlack = True &= name "black-ai" &= help "The computer plays the black pieces"
  }
