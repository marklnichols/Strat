{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CheckersJson where

import Data.Aeson
import Data.Char
import Data.Maybe
import GHC.Generics
import qualified CkParser as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Ch8
import Checkers

----------------------------------------------------------------------------------------------------
-- Data types and instances
----------------------------------------------------------------------------------------------------
data JsonLoc = JsonLoc {row:: Char, col:: Int } deriving Generic

instance ToJSON JsonLoc where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonLoc

instance Show JsonLoc where
    show loc = show (row loc) ++ show (col loc)

----------------------------------------------------------------------------
data JsonMove = JsonMove {locs:: [JsonLoc] } deriving Generic

instance ToJSON JsonMove where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonMove

instance Show JsonMove where
    show jm = unwords $ show <$> locs jm  

----------------------------------------------------------------------------
data LegalMoves = LegalMoves {moves :: [JsonMove]} deriving (Generic, Show)

instance ToJSON LegalMoves where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LegalMoves    

----------------------------------------------------------------------------
data Board = Board {pieces :: [JsonLoc]} deriving (Generic, Show)

instance ToJSON Board where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Board

----------------------------------------------------------------------------
data FullUpdate = FullUpdate {msg :: String,
                              board :: Board,
                              legalMoves :: LegalMoves} deriving (Generic, Show)

instance ToJSON FullUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FullUpdate

----------------------------------------------------------------------------
data Message = Message {message :: String} deriving (Generic, Show)

instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON Message

----------------------------------------------------------------------------
data JsonError = JsonError {jsonErr :: String} deriving (Generic, Show)

instance ToJSON JsonError where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON JsonError

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------
jsonToCkMove :: String -> Maybe CkMove
jsonToCkMove s = (jsonToParserMove <$> decode (strToBs s)) >>= parserToCkMove
  
jsonFromCkMove :: CkMove -> Maybe String
jsonFromCkMove mv = let bs = fmap (encode . jsonFromParserMove) (toParserMove mv)
                    in fmap bsToStr bs  
  
updateToJson :: String -> [P.Loc]  -> [CkMove] -> String
updateToJson str allPieces ckMoves = 
    let b = Board {pieces = fmap fromParserLoc allPieces}
        parserMoves = catMaybes $ fmap toParserMove ckMoves
        legals = LegalMoves {moves = fmap jsonFromParserMove parserMoves}
    in  bsToStr $ encode FullUpdate {msg = str, board = b, legalMoves = legals}

messageToJson :: String -> String
messageToJson s = bsToStr $ encode Message {message = s}            

errorToJson :: String -> String
errorToJson s = bsToStr $ encode JsonError {jsonErr = s}         

----------------------------------------------------------------------------------------------------
 -- Internal functions
----------------------------------------------------------------------------------------------------   
fromParserLoc :: P.Loc -> JsonLoc
fromParserLoc (P.Loc c i) = JsonLoc {row = c, col = i} 

toParserLoc :: JsonLoc -> P.Loc
toParserLoc jLoc = P.Loc (row jLoc) (col jLoc)

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

strToBs :: String -> B.ByteString
strToBs = Ch8.pack
                 
jsonFromParserMove :: P.Move -> JsonMove
jsonFromParserMove (P.Move xs) = JsonMove {locs = fmap fromParserLoc xs} 

jsonToParserMove :: JsonMove -> P.Move
jsonToParserMove jMv = P.Move (fmap toParserLoc (locs jMv))
    
