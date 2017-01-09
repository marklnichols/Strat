{-# LANGUAGE OverloadedStrings #-}

module Views.ComputerMove (computerMoveView) where
            
import           Prelude        hiding (div, head, id)        
import qualified Web.Scotty.Trans as T 
import Data.Text.Lazy (Text)
import GlobalState
import Control.Monad.IO.Class

computerMoveView :: T.ActionT Text WebM ()
computerMoveView = liftIO $ putStrLn "Incoming computer move request"



