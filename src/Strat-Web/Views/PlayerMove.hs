{-# LANGUAGE OverloadedStrings #-}
module Views.PlayerMove (playerMoveView, playerMoveRespondView) where
            
import Prelude hiding (div, head, id)
import qualified Web.Scotty.Trans as T                 
import GlobalState
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class

playerMoveView :: T.ActionT Text WebM ()
playerMoveView = liftIO $ putStrLn "Incoming player move request"
    
playerMoveRespondView :: T.ActionT Text WebM () 
playerMoveRespondView = liftIO $ putStrLn "Incoming player move request with respond."
