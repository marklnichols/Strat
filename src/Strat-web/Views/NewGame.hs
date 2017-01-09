{-# LANGUAGE OverloadedStrings #-}
module Views.NewGame where
            
import Prelude hiding (div, head, id)
import Checkers as Ck
import WebRunner
import Control.Monad.IO.Class
import Web.Scotty.Login.Session
import qualified Web.Scotty.Trans as T 
import Data.Text.Lazy (Text)
import qualified Session as S
import GlobalState


newGameView :: T.ActionT Text WebM ()       -- ActionM :: ActionT Text IO
newGameView = do 
    T.setHeader "Content-Type" "application/json"
    liftIO $ putStrLn "incoming new game request" 
    authCheck newSession sessionExists
    c <- webM $ gets tickCount
    liftIO $ putStrLn ("tick count: " ++ show c)

newGameRespondView :: T.ActionT Text WebM ()
newGameRespondView = do 
    T.setHeader "Content-Type" "application/json"
    liftIO $ putStrLn "incoming new game request with respond."
    jAble <- liftIO $ processStartGame Ck.getStartNode True     
    jsonableToActionT jAble     

newSession :: T.ActionT Text WebM ()
newSession = do 
    liftIO $ putStrLn "Adding a new session..." 
    _ <- addSession S.conf :: T.ActionT Text WebM (Maybe Session)
    sessionExists
    
sessionExists ::  T.ActionT Text WebM ()
sessionExists = do
    liftIO $ putStrLn "session exists..." 
    let ioj = processStartGame Ck.getStartNode False  -- :: IO Jsonable
    let lifted = liftIO ioj
    jAble <- lifted    -- :: Jsonable
    jsonableToActionT jAble
               
jsonableToActionT :: Jsonable -> T.ActionT Text WebM ()
jsonableToActionT j = 
    case j of
        Jsonable x -> T.json x
                      
typeHole :: a -> a -> a
typeHole x _ = x       

----------------------------------------------------------------------------------------------------
{-
sessionExists :: ActionM ()
sessionExists = do
    liftIO $ putStrLn "session exists..." 
    let ioj = processStartGame Ck.getStartNode False  -- :: IO Jsonable
    let lifted = liftIO ioj     -- :: ActionT Text IO Jsonable,  liftIO :: IO a -> m a
    jAble <- lifted             -- :: Jsonable 
    case jAble of 
        Jsonable x -> json x    -- x :: ToJSON, json :: ToJSON a => a -> ActionM ()
-}   
   
    
       