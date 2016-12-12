{-# LANGUAGE OverloadedStrings #-}

module Controllers.MainController
    ( home
      ,move
      ,newGame
    ) where

import Views.Home (homeView)
import Views.Move (moveView)
import Views.NewGame (newGameView)
import Web.Scotty (ScottyM, get)

move :: ScottyM ()
move = get "/move" moveView

home :: ScottyM ()
home = get "/" homeView

newGame :: ScottyM ()
newGame = get "/new" newGameView
