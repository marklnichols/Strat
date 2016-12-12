{-# LANGUAGE OverloadedStrings #-}

module Views.Move (moveView) where
            
import           Prelude                        hiding (div, head, id)
import           Web.Scotty                  

moveView :: ActionM ()
moveView = do 
    setHeader "Content-Type" "text/html"
    file "src/Strat-web/Static/gameboard2.html"




