{-# LANGUAGE OverloadedStrings #-}
module Controllers.MainController
    (   home
      , newGame
      , newGameRespond
      , playerMove
      , playerMoveRespond
      , computerMove
    ) where

import Views.Home (homeView)
import Views.NewGame (newGameView, newGameRespondView)
import Views.PlayerMove (playerMoveView , playerMoveRespondView)
import Views.ComputerMove (computerMoveView)
import Web.Scotty (ScottyM, get)
import qualified Web.Scotty.Trans as T
import Data.Text.Lazy (Text)
import GlobalState

newGameRespond :: T.ScottyT Text WebM ()
newGameRespond = T.get "/new/respond" newGameRespondView

newGame :: T.ScottyT Text WebM ()
newGame = T.get "/new" newGameView

playerMoveRespond :: T.ScottyT Text WebM ()
playerMoveRespond = T.get "/playerMove/respond" playerMoveRespondView

playerMove :: T.ScottyT Text WebM ()
playerMove = T.get "/playerMove" playerMoveView

computerMove :: T.ScottyT Text WebM ()
computerMove = T.get "/computerMove" computerMoveView

home :: T.ScottyT Text WebM ()
home = T.get "/" homeView