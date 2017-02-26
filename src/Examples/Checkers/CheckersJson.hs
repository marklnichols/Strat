{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CheckersJson where

import Data.Aeson
import Data.Char
import GHC.Generics
import qualified CkParser as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Checkers as Ck

----------------------------------------------------------------------------------------------------
-- Data types and instances
----------------------------------------------------------------------------------------------------
data JsonLoc = JsonLoc {col :: Char, row :: Int } deriving Generic

instance ToJSON JsonLoc where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonLoc

instance Show JsonLoc where
    show jl = "col: " ++ show (col jl) ++ "row: " ++ show (row jl) 

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
data FullUpdate = FullUpdate {msg :: String,
                              board :: [JsonSquare],
                              legalMoves :: LegalMoves,
                              latestMove :: JsonMove} deriving (Generic, Show)

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
--todo: do I now not need this since Yesod does the decode?
jsonToCkMove :: String -> Maybe Ck.CkMove
jsonToCkMove s = (jsonToParserMove <$> decode (strToBs s)) >>= Ck.parserToCkMove
--instead:
jsonMoveToCkMove :: JsonMove -> Maybe Ck.CkMove
jsonMoveToCkMove jMove = Ck.parserToCkMove $ jsonToParserMove jMove
  
jsonFromCkMove :: Ck.CkMove -> String
jsonFromCkMove mv = (bsToStr . encode . jsonFromParserMove) (Ck.toParserMove mv)
                
jsonUpdate :: String -> Ck.CkNode -> [Ck.CkMove] -> Maybe Ck.CkMove -> FullUpdate    
jsonUpdate str node ckMoves moveMay = 
    let parserMoves = fmap Ck.toParserMove ckMoves
        legals = LegalMoves {moves = fmap jsonFromParserMove parserMoves}
        latest = jsonFromParserMove . Ck.maybeToParserMove $ moveMay
    in  FullUpdate {msg = str, board = boardToJson node, legalMoves = legals, latestMove = latest}

jsonMessage :: String -> Message
jsonMessage s = Message {message = s}

jsonError :: String -> JsonError
jsonError s = JsonError {jsonErr = s}

----------------------------------------------------------------------------------------------------
 -- Internal functions
---------------------------------------------------------------------------------------------------- 
boardToJson :: Ck.CkNode -> [JsonSquare]
boardToJson node = fmap toJsonSquare $ Ck.pieceLocs $ Ck.boardAsPieceList node
   
toJsonSquare :: Ck.PieceLoc -> JsonSquare
toJsonSquare pieceloc = 
    let val = Ck.pieceLocValue pieceloc
        absVal = abs val
        clr = signum val
    in  JsonSquare {loc = fromParserLoc (Ck.pieceLoc pieceloc), pieceType = absVal, color = clr}
  
fromParserLoc :: P.Loc -> JsonLoc
fromParserLoc (P.Loc c i) = JsonLoc {col = c, row = i} 

toParserLoc :: JsonLoc -> P.Loc
toParserLoc jLoc = P.Loc (col jLoc) (row jLoc)

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

strToBs :: String -> B.ByteString
strToBs = B8.pack
                 
jsonFromParserMove :: P.Move -> JsonMove
jsonFromParserMove (P.Move xs) = JsonMove {locs = fmap fromParserLoc xs} 

jsonToParserMove :: JsonMove -> P.Move
jsonToParserMove jMv = P.Move (fmap toParserLoc (locs jMv))
    
----------------------------------------------------------------------------------------------------
-- Example messages
----------------------------------------------------------------------------------------------------
{-
FullUpdate - 
{"msg": "New Game, player moves first",
 "board":[{"loc":{"row":"A","col":1},"pieceType":1,"color":1},
                            {"loc":{"col":"H","row":8},"pieceType":1,"color":-1}],
 "legalMoves": {"moves":   [{"locs":[{"col":"C","row":7},{"col":"D","row":6}]},
                            {"locs":[{"col":"C","row":1},{"col":"D","row":2}]}]}
}
-}