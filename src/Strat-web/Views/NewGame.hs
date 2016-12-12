{-# LANGUAGE OverloadedStrings #-}

module Views.NewGame (newGameView) where
            
import           Prelude                        hiding (div, head, id)
import           Web.Scotty                  

newGameView :: ActionM ()
newGameView = do 
    setHeader "Content-Type" "application/json"
    --TODO: get start board, create new key, put key/node into hashtable
    --from WebRunner instead of this:
    -- GameRunner.startGame CheckersWeb Checkers.getStartNode



