{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, 
    MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-unused-binds #-}
module YesodMain (webInit) where

import Yesod hiding (insert)
import Yesod.Static
import Checkers
import CheckersJson
import WebRunner
import Data.Text (Text)
import Data.Aeson
import Data.Maybe
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Tree
    
staticFilesList "src/Strat-web/Static" ["gameboard.html"] -- , "image.png"]

data GameApp = GameApp { getStatic :: Static, 
                         getCounter :: IORef Integer, 
                         getMap :: IORef (M.Map Text (Tree CkNode)) }

emptyMap :: M.Map Text (Tree CkNode)
emptyMap = M.empty                                     
                                     
mkYesod "GameApp" [parseRoutes|
/ RootR GET
/new NewGameR GET
/computerMove ComputerMoveR GET
/playerMove PlayerMoveR POST
/count CounterR GET
/static StaticR Static getStatic
|]

instance Yesod GameApp

getCounterR :: Handler Html
getCounterR = do 
    yesod <- getYesod
    cnt <- liftIO $ incCount $ getCounter yesod
    liftIO $ putStrLn $ "Sending Response " ++ show cnt
    defaultLayout [whamlet|Hello World #{cnt}|]

incCount :: Num a => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c+1, c))

updateMap :: IORef (M.Map Text (Tree CkNode)) -> Text -> Tree CkNode -> IO ()
updateMap mapRef key value = do 
    theMap <- readIORef mapRef
    writeIORef mapRef (M.insert key value theMap)

uniqueIdKey :: Text
uniqueIdKey = "uniqueId"
 
getRootR :: Handler Html
getRootR = do
    liftIO $ putStrLn "Incoming 'home' request"
    _ <- getGameSession
    redirect $ StaticR gameboard_html                         
      
getGameSession :: HandlerT GameApp IO Text
getGameSession = do 
    idMay <- readSession uniqueIdKey  -- :: Maybe Text
    if isNothing idMay  -- u1 :: HandlerT GameApp IO Text
        then do
            liftIO $ putStrLn "Adding new unique id to session: "
            newId <- addUniqueId 
            liftIO $ putStrLn $ "uniqueId: " ++ show newId
            return newId
        else do
            liftIO $ putStrLn "Retrieved id from session... "
            return (fromJust idMay) 
    
addUniqueId :: Handler Text
addUniqueId = do
    newUnique <- newIdent
    setSession uniqueIdKey newUnique
    return newUnique

readSession :: Text -> Handler (Maybe Text)
readSession = lookupSession

getNewGameR :: Handler Value
getNewGameR = do
    liftIO $ putStrLn "incoming new game request"
    uniqueId <- getGameSession
    wrapper <- liftIO $ processStartGame getStartNode True
    let node = getNode wrapper
    let jAble = getJsonable wrapper
    yesod <- getYesod
    liftIO $ updateMap (getMap yesod) uniqueId node
    case jAble of 
        Jsonable j -> returnJson j      

getComputerMoveR :: Handler Value                
getComputerMoveR = do
    liftIO $ putStrLn "incoming computer move request"
    uniqueId <- getGameSession  
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    wrapper <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do 
            liftIO $ putStrLn "getComputerMoveR - no tree found in the map"
            processError "Something is wrong with this game" getStartNode
        Just t -> processComputerMove t 
    let node = getNode wrapper
    liftIO $ updateMap (getMap yesod) uniqueId node
    let jAble = getJsonable wrapper
    case jAble of
        Jsonable j -> returnJson j

--TODO: factor out some common code        
postPlayerMoveR :: Handler Value
postPlayerMoveR = do
    liftIO $ putStrLn "incoming computer player move"
    (resultM :: Result JsonMove) <- parseJsonBody 
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    uniqueId <- getGameSession
    wrapper <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do 
            liftIO $ putStrLn "getPlayerMoveR - no tree found in the map"
            processError "Something is wrong with this game" getStartNode
        Just tree -> 
            --TODO: clean up these nested cases after this is working...      
            case resultM of
                Error e -> do
                    liftIO $ putStrLn "getPlayerMoveR - could not parse the json from the client"
                    liftIO $ putStrLn ("Error retuned: " ++ e)
                    processError "Something is wrong with this game" getStartNode
                Success jMove ->   
                    case jsonMoveToCkMove jMove of
                        Nothing -> do
                            liftIO $ putStrLn "getPlayerMoveR - could not covert to CkMove"
                            processError "Something is wrong with this game" getStartNode
                        Just move -> 
                            processPlayerMove tree move True  --true, computer always responds -- for now     
    let node = getNode wrapper 
    liftIO $ updateMap (getMap yesod) uniqueId node
    let jAble = getJsonable wrapper
    case jAble of
        Jsonable j -> returnJson j
        
webInit :: IO ()
webInit = do
    counter <- newIORef 0
    newMap <- newIORef emptyMap
    s <- static "src/Strat-web/Static"
    warp 3000 GameApp {getStatic = s, getCounter = counter, getMap = newMap}
    
typeHole :: a -> a -> a
typeHole _ y = y 
  
  
  