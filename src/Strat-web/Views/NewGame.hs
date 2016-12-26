{-# LANGUAGE OverloadedStrings #-}

module Views.NewGame (newGameView) where
            
import Prelude hiding (div, head, id)
import Web.Scotty                  
--import qualified CheckersJson as J
import Checkers as Ck
import WebRunner
import Control.Monad.IO.Class

--TODO: create new key, put key/node into hashtable
newGameView :: ActionM ()       -- ActionM :: ActionT Text IO
newGameView = do 
    setHeader "Content-Type" "application/json"
    let ioj = processStartGame Ck.getStartNode  -- :: IO Jsonable
    let lifted = liftIO ioj     -- :: ActionT Text IO Jsonable,  liftIO :: IO a -> m a
    jAble <- lifted             -- :: Jsonable 
    case jAble of 
        Jsonable x -> json x    -- x :: ToJSON, json :: ToJSON a => a -> ActionM ()
