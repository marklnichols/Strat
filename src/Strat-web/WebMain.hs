module WebMain where

import           Control.Applicative                  ((<$>))
import           Controllers.MainController                     
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots, staticPolicy, (>->))
import           System.Environment                   (getEnv)
import           Web.Scotty                           (middleware, scotty)

init :: IO ()
init = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
         --middleware $ staticPolicy (noDots >-> addBase "static")
         middleware logStdoutDev
         home
         move
         newGame

