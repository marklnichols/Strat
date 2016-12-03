{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersWeb where

import Checkers
import CheckersJson
import StratTree.TreeNode
import StratTree.StratTree
import Data.Tree
import Data.Maybe

data CheckersWeb = CheckersWeb

instance Output CheckersWeb CkNode CkMove CkEval where
    out _ =  sendUIMessage
    updateBoard _ = sendAll
    showCompMove _ = sendMoveChoiceInfo  
    getPlayerMove _ = playerMove 
    gameError _ = sendUIError
    
sendUIMessage :: String -> IO ()
sendUIMessage s = putStrLn $ messageToJson s

sendAll :: CkNode -> IO ()    
sendAll node =  
    let s = "Some message"
        xs = catMaybes $ intToLoc <$> getPieceLocs node
    in putStrLn $ updateToJson s xs (getAllowedMoves node)

sendMoveChoiceInfo :: Tree CkNode -> [MoveScore CkMove CkEval] -> Result CkMove CkEval -> CkMove -> IO ()
sendMoveChoiceInfo _ _ _ mv = case jsonFromCkMove mv of
    Nothing -> putStrLn "Error parsing move"
    Just s  -> putStrLn s

playerMove :: Tree CkNode -> Int -> IO CkMove
playerMove tree turn  = do
    -- TODO get the move from the web - just for now get it from the keyboard
    --remove this stuff:
    putStrLn ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    putStrLn ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            putStrLn err
            playerMove tree turn
        Right mv -> if not (isLegal tree mv)
                        then do
                            putStrLn "Not a legal move."
                            playerMove tree turn
                        else return mv   
    

sendUIError :: String -> IO ()
sendUIError s = putStrLn $ errorToJson s

    
    