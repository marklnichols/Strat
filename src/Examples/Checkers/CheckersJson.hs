{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CheckersJson
    ( jsonError
    , jsonFromCkMove
    , jsonMessage
    , JsonMove(..)
    , jsonMoveToCkMove
    , jsonToCkMove
    , jsonUpdate
    ) where

import Data.Aeson
import Data.Char
import GHC.Generics
import Strat.StratTree.TreeNode (MoveScore(..))
import qualified Checkers as Ck
import qualified MegaParser8By8 as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Maybe (fromMaybe, mapMaybe)

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
newtype JsonMove = JsonMove {locs :: [JsonLoc] } deriving Generic

instance ToJSON JsonMove where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonMove

instance Show JsonMove where
    show jm = unwords $ show <$> locs jm

emptyJM :: JsonMove
emptyJM = JsonMove {locs = []}

----------------------------------------------------------------------------
data JsonEval = JsonEval {total :: Float, details :: String } deriving Generic

instance ToJSON JsonEval where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonEval

instance Show JsonEval where
    show je = "total: " ++ show (total je) ++ "details: " ++ details je

emptyJE :: JsonEval
emptyJE = JsonEval {total = 0, details = ""}

----------------------------------------------------------------------------
data JsonMoveScore = JsonMoveScore {move :: JsonMove, score :: JsonEval } deriving Generic

instance ToJSON JsonMoveScore where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonMoveScore

instance Show JsonMoveScore where
    show jms = "move: " ++ show (move jms) ++ "score: " ++ show (score jms)

emptyJMS :: JsonMoveScore
emptyJMS = JsonMoveScore {move = emptyJM, score = emptyJE}

----------------------------------------------------------------------------
newtype LegalMoves = LegalMoves {moves :: [JsonMove]} deriving (Generic, Show)

instance ToJSON LegalMoves where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LegalMoves

----------------------------------------------------------------------------
data FullUpdate = FullUpdate {msg :: String,
                              prevBoard :: [JsonSquare],
                              board :: [JsonSquare],
                              legalMoves :: LegalMoves,
                              latestMove :: JsonMoveScore} deriving (Generic, Show)

instance ToJSON FullUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FullUpdate

----------------------------------------------------------------------------
newtype Message = Message {message :: String} deriving (Generic, Show)

instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Message

----------------------------------------------------------------------------
newtype JsonError = JsonError {jsonErr :: String} deriving (Generic, Show)

instance ToJSON JsonError where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JsonError

----------------------------------------------------------------------------------------------------
-- Exported functionsn
----------------------------------------------------------------------------------------------------
jsonToCkMove :: String -> Maybe Ck.CkMove
jsonToCkMove s = (jsonToParserMove <$> decode (strToBs s)) >>= Ck.parserToCkMove

jsonMoveToCkMove :: JsonMove -> Maybe Ck.CkMove
jsonMoveToCkMove jMove = Ck.parserToCkMove $ jsonToParserMove jMove

jsonFromCkMove :: Ck.CkMove -> String
jsonFromCkMove mv = (bsToStr . encode . jsonFromParserMove) (Ck.toParserMove mv)

jsonFromMoveScore :: MoveScore Ck.CkMove Ck.CkNode -> Maybe JsonMoveScore
jsonFromMoveScore ms =
    case jsonFromParserMove $ Ck.toParserMove $ _move ms of
        Nothing -> Nothing
        Just jMove ->
            let eval = _score ms
                jEval = JsonEval {total = Ck._total (Ck._ckValue eval), details = Ck._details (Ck._ckValue eval) }
            in Just $ JsonMoveScore {move = jMove, score = jEval}


jsonFromMoveScore' :: Maybe (MoveScore Ck.CkMove Ck.CkNode) -> JsonMoveScore
jsonFromMoveScore' ms = fromMaybe emptyJMS (ms >>= jsonFromMoveScore)

jsonUpdate :: String -> Ck.CkNode -> Ck.CkNode -> [Ck.CkMove] -> Maybe (MoveScore Ck.CkMove Ck.CkNode) -> FullUpdate
jsonUpdate str prevN newN ckMoves moveScoreM =
    let parserMoves = fmap Ck.toParserMove ckMoves
        legals = LegalMoves {moves = mapMaybe jsonFromParserMove parserMoves}
        latest = jsonFromMoveScore' moveScoreM
    in  FullUpdate {msg = str, prevBoard = boardToJson prevN, board = boardToJson newN,
                    legalMoves = legals, latestMove = latest}

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

jsonFromParserMove :: P.Entry -> Maybe JsonMove
jsonFromParserMove (P.Move xs) = Just $ JsonMove {locs = fmap fromParserLoc xs}
jsonFromParserMove (P.Cmd _) = Nothing

jsonToParserMove :: JsonMove -> P.Entry
jsonToParserMove jMv = P.Move (fmap toParserLoc (locs jMv))
