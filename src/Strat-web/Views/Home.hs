{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView) where
            
import           Prelude                        hiding (div, head, id)
import           Web.Scotty                  

homeView :: ActionM ()
homeView = do 
    setHeader "Content-Type" "text/html"
    file "src/Strat-web/Static/gameboard.html"




