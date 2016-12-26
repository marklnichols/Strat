{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CheckersJson where

import Data.Aeson
import Data.Char
import Data.Maybe
import GHC.Generics
import qualified CkParser as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Checkers as Ck

----------------------------------------------------------------------------------------------------
-- Data types and instances
----------------------------------------------------------------------------------------------------
data JsonLoc = JsonLoc {row :: Char, col :: Int } deriving Generic

instance ToJSON JsonLoc where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonLoc

instance Show JsonLoc where
    show jl = "row: " ++ show (row jl) ++ "col: " ++ show (col jl) 

----------------------------------------------------------------------------               
--JsonSquare is a JsonLoc plus the contents (piece type, color) at that loc
data JsonSquare = JsonSquare {loc :: JsonLoc, pieceType :: Int, color :: Int } deriving Generic

instance ToJSON JsonSquare where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonSquare

instance Show JsonSquare where
    show sq = "loc: " ++ show (loc sq) ++ "piece: " ++ 
        show (pieceType sq) ++ "color: " ++ show (color sq)

----------------------------------------------------------------------------
data JsonMove = JsonMove {locs :: [JsonLoc] } deriving Generic

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
data Board = Board {squares :: [JsonSquare]} deriving (Generic, Show)

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
jsonToCkMove :: String -> Maybe Ck.CkMove
jsonToCkMove s = (jsonToParserMove <$> decode (strToBs s)) >>= Ck.parserToCkMove
  
jsonFromCkMove :: Ck.CkMove -> Maybe String
jsonFromCkMove mv = let bs = fmap (encode . jsonFromParserMove) (Ck.toParserMove mv)
                    in fmap bsToStr bs  
 
{- 
updateToJson :: String -> Ck.CkNode  -> [Ck.CkMove] -> String
updateToJson str node ckMoves = 
        --todo: change this:
    let parserMoves = catMaybes $ fmap Ck.toParserMove ckMoves
        legals = LegalMoves {moves = fmap jsonFromParserMove parserMoves}
    in  bsToStr $ encode FullUpdate {msg = str, board = boardToJson node, legalMoves = legals}
-}
jsonUpdate :: String -> Ck.CkNode -> [Ck.CkMove] -> FullUpdate    
jsonUpdate str node ckMoves = 
    let parserMoves = catMaybes $ fmap Ck.toParserMove ckMoves
        legals = LegalMoves {moves = fmap jsonFromParserMove parserMoves}
    in  FullUpdate {msg = str, board = boardToJson node, legalMoves = legals}

{-    
messageToJson :: String -> String
messageToJson s = bsToStr $ encode Message {message = s}            
-}
jsonMessage :: String -> Message
jsonMessage s = Message {message = s}

{-
errorToJson :: String -> String
errorToJson s = bsToStr $ encode JsonError {jsonErr = s}         
-}
jsonError :: String -> JsonError
jsonError s = JsonError {jsonErr = s}

----------------------------------------------------------------------------------------------------
 -- Internal functions
---------------------------------------------------------------------------------------------------- 
boardToJson :: Ck.CkNode -> Board
boardToJson node = Board {squares = fmap toJsonSquare $ Ck.pieceLocs $ Ck.boardAsPieceList node}
   
toJsonSquare :: Ck.PieceLoc -> JsonSquare
toJsonSquare pieceloc = 
    let val = Ck.pieceLocValue pieceloc
        absVal = abs val
        clr = signum val
    in  JsonSquare {loc = fromParserLoc (Ck.pieceLoc pieceloc), pieceType = absVal, color = clr}
  
fromParserLoc :: P.Loc -> JsonLoc
fromParserLoc (P.Loc c i) = JsonLoc {row = c, col = i} 

toParserLoc :: JsonLoc -> P.Loc
toParserLoc jLoc = P.Loc (row jLoc) (col jLoc)

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

strToBs :: String -> B.ByteString
strToBs = B8.pack
                 
jsonFromParserMove :: P.Move -> JsonMove
jsonFromParserMove (P.Move xs) = JsonMove {locs = fmap fromParserLoc xs} 

jsonToParserMove :: JsonMove -> P.Move
jsonToParserMove jMv = P.Move (fmap toParserLoc (locs jMv))
    
