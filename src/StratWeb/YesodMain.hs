{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}

module StratWeb.YesodMain (webInit) where

import Control.Lens
import Checkers
import CheckersJson
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import Data.Tree
import qualified Strat.StratTree.TreeNode as TN
import qualified StratWeb.WebRunner as WR
import System.Random
import Yesod hiding (insert)
import Yesod.Static
import qualified Data.Map.Strict as M

staticFilesList "src/StratWeb/Static" ["gameboard.html", "bundle.js", "checker_1_king_48.png",
    "checker_1_plain_48.png", "checker_2_king_48.png", "checker_2_plain_48.png",
    "black_image_48.png", "no_image_48.png", "favicon.ico"]

data GameApp = GameApp { getStatic :: Static,
                         getCounter :: IORef Integer,
                         getMap :: IORef (M.Map Text (Tree CkNode, StdGen)) }

emptyMap :: M.Map Text (Tree CkNode, StdGen)
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

updateMap :: IORef (M.Map Text (Tree CkNode, StdGen)) -> Text -> (Tree CkNode, StdGen) -> IO ()
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

getGameSession :: HandlerFor GameApp Text
getGameSession = do
    idMay <- lookupSession uniqueIdKey
    case idMay of
        Nothing -> do
            liftIO $ putStrLn "Adding new unique id to session: "
            newId <- addUniqueId
            liftIO $ putStrLn $ "uniqueId: " ++ show newId
            return newId
        Just theId -> do
            liftIO $ putStrLn $ "Retrieved id from session: " ++ show theId
            return theId

addUniqueId :: Handler Text
addUniqueId = do
    newUnique <- newIdent
    setSession uniqueIdKey newUnique
    return newUnique

getNewGameR :: Handler Value
getNewGameR = do
    liftIO $ putStrLn "incoming new game request"
    uniqueId <- getGameSession
    (wrapper, gen) <- liftIO $ WR.processStartGame getStartNode False
    let node = WR.getNode wrapper
    let jAble = WR.getJsonable wrapper
    yesod <- getYesod
    liftIO $ updateMap (getMap yesod) uniqueId (node, gen)
    case jAble of
        WR.Jsonable j -> returnJson j

getComputerMoveR :: Handler Value
getComputerMoveR = do
    liftIO $ putStrLn "incoming computer move request"
    uniqueId <- getGameSession
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    (wrapper, gen) <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do
            liftIO $ putStrLn "getComputerMoveR - no tree found in the map"
            -- TODO: display some error about the restart
            liftIO $ WR.processStartGame getStartNode False
        Just (t, g) -> do
            wrp <- WR.processComputerMove t g
            return (wrp, g)
    liftIO $ updateMap (getMap yesod) uniqueId (WR.getNode wrapper, gen)
    case WR.getJsonable wrapper of
        WR.Jsonable j -> returnJson j

--TODO: factor out some common code
postPlayerMoveR :: Handler Value
postPlayerMoveR = do
    liftIO $ putStrLn "incoming player move"
    (resultM :: Result JsonMove) <- parseCheckJsonBody
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    uniqueId <- getGameSession
    (wrapper, gen) <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do
            liftIO $ putStrLn "getPlayerMoveR - no tree found in the map"
            -- TODO: display some error about the restart
            liftIO $ WR.processStartGame getStartNode False
        Just (tree, g) ->
            --TODO: clean up these nested cases, and properly deal with the errors / restarts
            case resultM of
                Error e -> do
                    liftIO $ putStrLn "getPlayerMoveR - could not parse the json from the client"
                    liftIO $ putStrLn ("Error retuned: " ++ e)
                    liftIO $ WR.processStartGame getStartNode False
                Success jMove -> do
                    liftIO $ putStrLn $ "Player move: " ++ show jMove
                    case jsonMoveToCkMove jMove of
                        Nothing -> do
                            liftIO $ putStrLn "getPlayerMoveR - could not covert to CkMove"
                            liftIO $ WR.processStartGame getStartNode False
                        Just move -> do
                            wrp <- WR.processPlayerMove tree move True g  --true, computer always responds -- for now
                            return (wrp, g)
    liftIO $ updateMap (getMap yesod) uniqueId $ (WR.getNode wrapper, gen)
    case WR.getJsonable wrapper of
        WR.Jsonable j -> do
            case WR.getLastMove wrapper of
                Just ms -> liftIO $ putStrLn $ "Computer's move: " ++ jsonFromCkMove (ms^.TN.move)
                Nothing -> liftIO $ putStrLn "(No computer move)"
            returnJson j

webInit :: IO ()
webInit = do
    counter <- newIORef 0
    newMap <- newIORef emptyMap
    s <- staticDevel "src/StratWeb/Static"
    putStrLn "\n--------------------------------------------------------"
    putStrLn "To play, point your web browser to http://localhost:3000"
    putStrLn "--------------------------------------------------------\n"
    warp 3000 GameApp {getStatic = s, getCounter = counter, getMap = newMap}
