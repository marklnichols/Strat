{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module YesodMain (webInit) where

import Yesod
import Yesod.Static
import Yesod.Core.Handler
import Checkers as Ck
import WebRunner

staticFilesList "src/Strat-web/Static" ["gameboard.html", "image.png"]

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
  
getRootR :: Handler Html
getRootR = do
    liftIO $ putStrLn "Incoming 'home' request"
    redirect $ StaticR gameboard_html

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
    s@(Static settings) <- static "src/Strat-web/Static"
    warp 3000 $ App s
