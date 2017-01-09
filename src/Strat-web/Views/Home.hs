{-# LANGUAGE OverloadedStrings #-}
module Views.Home (homeView) where
            
import Prelude hiding (div, head, id)
import qualified Web.Scotty.Trans as T
import Data.Text.Lazy (Text)                 
import Control.Monad.IO.Class
import GlobalState 

homeView :: T.ActionT Text WebM () 
homeView = do 
    liftIO $ putStrLn "Incoming 'home' request"
    T.file "src/Strat-web/Static/gameboard.html" 
