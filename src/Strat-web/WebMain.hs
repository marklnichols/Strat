{-# LANGUAGE OverloadedStrings #-}
module WebMain (init) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger
import Prelude hiding (init)
import Web.Scotty.Trans
import System.Environment
import Network.Wai.Middleware.Static  
import Controllers.MainController
import qualified Session as S
import GlobalState

init :: IO ()
init = do
    sync <- newTVarIO def
        -- 'runActionToIO' is called once per action.
    let runActionToIO m = runReaderT (runWebM m) sync

    port <- read <$> getEnv "PORT"
    scottyT port runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware $ staticPolicy (noDots >-> addBase "src/Strat-web/Static")
    middleware logStdoutDev
    home
    newGame
    newGameRespond
    playerMoveRespond
    playerMove
    computerMove
       
    get "showstate" $ do
        c <- webM $ gets tickCount
        text $ fromString $ show c

    get "/plusone" $ do
        webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
        redirect "/"

    get "/plustwo" $ do
        webM $ modify $ \ st -> st { tickCount = tickCount st + 2 }
        redirect "/"
       
----------------------------------------------------------------------------------------------------       
--TODO: get logging working
----------------------------------------------------------------------------------------------------    
--import           Data.Default
--import           System.Log.FastLogger                (newFileLoggerSet, defaultBufSize)     

        -- middleware :: Middleware -> ScottyM () (Web.Scotty)
        --logStdout :: Middleware (Network.Wai.Middleware.RequestLogger)
        --middleware (mkRequestLogger mySettings) 
        -- mkRequestLogger :: RequestLoggerSettings -> IO Middleware (ysodweb.WAI)
        --so need to go from IO Middleware -> Middleware   
     
--mySettings :: RequestLoggerSettings
--mySettings = def {destination = newFileLoggerSet defaultBufSize "logging"}
  
{-
--from SO -- REMOVE THIS:
import Data.Default
import Network.Wai.Middleware.RequestLogger

mySettings :: RequestLoggerSettings
mySettings = def {
    outputFormat = Detailed False
  , autoFlush = False
  }
-- Or any other settings you may wish to set. See doc for more info.
-}        

