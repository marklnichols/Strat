{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-unused-binds #-}

module YesodMain (webInit) where

import Yesod
import Yesod.Static
import Checkers as Ck
import WebRunner
import Data.Text (Text)
import Data.Maybe

staticFilesList "src/Strat-web/Static" ["gameboard.html"] -- , "image.png"]

data App = App
    { getStatic :: Static
    }

mkYesod "App" [parseRoutes|
/ RootR GET
/new NewGameR GET
/j JsonTestR GET
/static StaticR Static getStatic
|]

instance Yesod App

uniqueIdKey :: Text
uniqueIdKey = "uniqueId"
 
getRootR :: Handler Html
getRootR = do
    liftIO $ putStrLn "Incoming 'home' request"
    idMay <- readSession uniqueIdKey -- :: Maybe Text
    uniqueId <- if isJust idMay
            then do
                liftIO $ putStrLn "Retrieved id from session... "
                return $ fromJust idMay
            else do
                liftIO $ putStrLn "Adding new unique id..."
                addUniqueId   
    liftIO $ putStrLn $ "uniqueId: " ++ show uniqueId
    redirect $ StaticR gameboard_html                         
    
addUniqueId :: Handler Text
addUniqueId = do
    newUnique <- newIdent
    setSession uniqueIdKey newUnique
    return newUnique

readSession :: Text -> Handler (Maybe Text)
readSession = lookupSession
--readSession name = do
--    textValue <- lookupSession name
--    return textValue
    
getNewGameR = do
    liftIO $ putStrLn "incoming new game request"
    jAble <- liftIO $ processStartGame Ck.getStartNode True     
    case jAble of 
        Jsonable j -> returnJsonEncoding j
 
getJsonTestR =  do
    --defaultLayout [whamlet|$newline never
    --    <img src=@{StaticR image_png}/>|]
    jAble <- liftIO $ processStartGame Ck.getStartNode True     
    case jAble of 
        Jsonable j -> returnJsonEncoding j
        --returnJsonEncoding :: (Monad m, J.ToJSON a) => a -> m J.Encoding
  
webInit :: IO ()
webInit = do
    -- Get the static subsite, as well as the settings it is based on
    --s@(Static settings) <- static "src/Strat-web/Static"
    s <- static "src/Strat-web/Static"
    warp 3000 $ App s
{-    
typeHole :: a -> a -> a
typeHole _ y = y 
-}   