{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-} -- until the 'safe' usages of 'head' and 'tail' are removed
{-# HLINT ignore "Eta reduce" #-}

module Chess
    ( Castle(..)
    , Castling(..)
    , ChessEval(..), total, details
    , ChessGrid(..)
    , ChessMove(..), exchange, startIdx, endIdx
    , ChessMoves(..), cmEmpty, cmEnemy
    , ChessNode(..), chessMv, chessVal, chessErrorVal, chessPos
    , ChessPos(..), cpGrid, cpState, cpFin
    , ChessPosState(..)
    , Color(..
           )
    , colorFromInt
    , colorToInt
    , countMaterial
    , getNodeFromFen
    , getStartNode
    , toParserMove
    -- exported for testing only
    , StdMoveTestData(..)
    , allowableBishopMoves
    , allowableKnightMoves
    , allowableKingMoves
    , allowableEnPassant
    , allowablePawnNonCaptures
    , allowablePawnCaptures
    , allowableQueenMoves
    , allowableRookMoves
    , calcBishopPawnScore
    , calcCenterPawnScore
    , calcKnightPawnScore
    , calcRookPawnScore
    , calcDevelopment
    , calcLocsForColor
    , calcMobility
    , calcMoveLists
    , calcPawnPositionScore
    , castleMoves
    , castlingAvailable
    , castlingNode
    , castlingStatus
    , checkPromote
    , checkFinal'
    , cnShowMoveOnly
    , connectedRooks
    , Dir(..)
    , Direction(..)
    , dirAttackMeLoc
    , dirLocsCount
    , dirLocsSingleCount
    , discoveredCheckDirs
    , discoveredCheckNode
    , discoveredCheckTree
    , endgameNode01
    , findDirFromKing
    , right, left, up, down, diagUL, diagDR, diagUR, diagDL
    , knightLU, knightRD, knightRU, knightLD, knightUL, knightDR, knightUR, knightDL
    , isKingInCheckFull
    , invertGrid
    , locsForColor
    , critBug01TestData
    , critBug01TestDataB
    , mateInTwo01TestData
    , mateInTwo02TestData
    , mateInTwo03TestData
    , mateInTwo03bTestData
    , mateInThree01TestData
    , mateInThree02TestData
    , moveExposesKing
    , moveExposesKingDirect
    , moveExposesKingDiscovered
    , moveIsCheck
    , moveIsDirectCheck
    , moveIsDiscoveredCheck
    , invalidTree
    , pairToIndexes
    , parseChessEntry
    , promotion01TestData
    , rookFileStatus
    , RookFileStatus(..)
    , rookMobility
    , queenMobility
    , showScoreDetails
    , startingBoard
    , fromFen
    , toFen
    , wK, wKB, wKN, wKR, wQB, wQN, wQR
    , bK, bKB, bKN, bKR, bQB, bQN, bQR
    ) where

import Control.Lens hiding (Empty)

import Data.Char
import qualified Data.Foldable as F
import Data.List.Extra (replace, notNull, upper, lower)
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Singletons
import Data.Singletons.Base.TH
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(Node), rootLabel)
import Data.Tuple.Extra
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Hashable
import GHC.Generics
import System.Console.CmdArgs.Implicit (Data)
-- import Text.Printf
-- import Debug.Trace

import qualified MegaParser8By8 as Parser
import qualified Strat.Helpers as Helpers
import Strat.StratTree.TreeNode
import qualified FenParser as FP
import qualified Strat.ZipTree as Z

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
empty :: Char
empty = ' '

data Castling = Castled
              | KingSideOnlyAvailable
              | QueenSideOnlyAvailable
              | BothAvailable
              | Unavailable
  deriving (Eq, Generic, Hashable, Show, Ord)

data Color where
  Black :: Color
  White :: Color
    deriving (Generic, Hashable, Show, Eq, Ord, Data )

data Castle = QueenSide | KingSide
  deriving (Generic, Hashable, Eq, Ord, Show)

data ChessMove
  = StdMove { _exchange :: Maybe Char, _startIdx :: Int, _endIdx :: Int, _stdNote :: String }
  | CastlingMove { _castle :: Castle, _kingStartIdx :: Int, _kingEndIdx :: Int
                 , _rookStartIdx :: Int, _rookEndIdx :: Int, _castleNote :: String }
  | EnPassantMove { _epStartIdx :: Int, _epEndIdx :: Int, _epRemoveIdx :: Int, _epNote :: String }
  deriving (Eq, Generic, Hashable, Ord)
makeLenses ''ChessMove

rowIndexes :: M.Map Int Int
rowIndexes = M.fromList [(1, 11), (2, 10), (3, 14), (4, 19), (5, 23), (6, 28), (7, 32), (8, 37)]

intToParserLoc :: Int -> Parser.Loc
intToParserLoc n =
    let r = n `div` 10
        c = chr $ 64 + (n - r * 10)
    in Parser.Loc c r

locToInt :: Parser.Loc -> Int
locToInt (Parser.Loc c d) =
    -- A->1, B->2, etc...
    let colBase = ord (toUpper c) - 65 + 1
    in d * 10 + colBase

toParserMove :: ChessMove -> Parser.Entry
toParserMove StdMove {..} = Parser.Move $ intToParserLoc _startIdx : [intToParserLoc _endIdx]
toParserMove CastlingMove{..} = Parser.Move $ intToParserLoc _kingStartIdx : [intToParserLoc _kingEndIdx]

instance Show ChessMove where
  show stm@StdMove {..} =
      let nonCaptureStr = show $ toParserMove stm
      in case _exchange of
           Just _ -> replace "-" "x" nonCaptureStr
           Nothing -> nonCaptureStr
  show cm = show $ toParserMove cm

data UnChessMove
  = StdUnMove { unReplace :: Maybe Char, unStartIdx :: Int, unEndIdx :: Int, unStdNote :: String }
  | CastlingUnMove { unCastle :: Castle, kingUnStartIdx :: Int, kingUnEndIdx :: Int
                   , rookUnStartIdx :: Int, rookUnEndIdx :: Int, unCastleNote :: String }
  deriving (Eq, Ord)

data StdMoveTestData where
  StdMoveTestData ::
    { smtdBoardName :: String
    , smtdDepth :: Int
    , smtdCritDepth :: Int
    , smtdStartIdx :: Int
    , smtdEndIdx :: Int
    }
    -> StdMoveTestData
  deriving (Eq, Ord)

data ChessPosState = ChessPosState
  { _cpsColorToMove :: Color
  , _cpsLastMove :: Maybe ChessMove
  , _cpsHalfMovesForDraw :: Int
  , _cpsMoveNumber :: Int
  , _cpsCastling :: (Castling, Castling)
  , _cpsEnPassant :: Maybe Int
  }
  deriving (Show, Eq, Generic, Hashable, Ord)
makeLenses ''ChessPosState

instance Z.PositionState ChessPosState where
  toString = show
  combineTwo = combineChessStates

combineChessStates :: ChessPosState -> ChessPosState -> ChessPosState
combineChessStates s1 s2 =
  let updatedCastling = case _cpsColorToMove s1 of
        White ->
            ( combineCastling (fst (_cpsCastling s1)) (fst (_cpsCastling s2))
            , snd (_cpsCastling s1))
        Black ->
            ( fst (_cpsCastling s1)
            , combineCastling (snd (_cpsCastling s1)) (snd (_cpsCastling s2)))
  in ChessPosState
      { _cpsColorToMove = _cpsColorToMove s2
      , _cpsLastMove = _cpsLastMove s2
      , _cpsHalfMovesForDraw = _cpsHalfMovesForDraw s2
      , _cpsMoveNumber = _cpsMoveNumber s2
      , _cpsCastling = updatedCastling
      , _cpsEnPassant = _cpsEnPassant s2
      }

combineCastling :: Castling ->  Castling -> Castling
combineCastling Unavailable _ = Unavailable
combineCastling _ Unavailable = Unavailable
combineCastling _ Castled = Castled
combineCastling Castled _ = Castled
combineCastling BothAvailable c2 = c2
combineCastling c1 BothAvailable = c1
combineCastling KingSideOnlyAvailable _ = KingSideOnlyAvailable
combineCastling _ KingSideOnlyAvailable = KingSideOnlyAvailable
combineCastling QueenSideOnlyAvailable _ = QueenSideOnlyAvailable

newtype ChessGrid = ChessGrid { unGrid :: Vector Char }
  deriving (Generic, Eq, Show, Ord)

instance Hashable ChessGrid where
  hashWithSalt n (ChessGrid v) = hashWithSalt n (V.toList v)

data ChessPos = ChessPos
  { _cpGrid :: ChessGrid
  , _cpWhitePieceLocs :: [(Int, Char, Color)]
  , _cpBlackPieceLocs :: [(Int, Char, Color)]
  , _cpKingLoc :: (Int, Int)
  , _cpInCheck :: Bool
  , _cpFin :: FinalState
  , _cpState :: ChessPosState }
  deriving (Generic, Hashable, Show, Eq, Ord)
makeLenses ''ChessPos

data ChessEval = ChessEval {_total :: Float, _details :: String}
    deriving (Eq, Generic, Hashable, Ord)
makeLenses ''ChessEval

instance Show ChessEval where
    show e = show (e ^. total)

data RookFileStatus = Open | HalfOpen | NotOpen
  deriving (Show, Eq)

showScoreDetails :: ChessEval -> String
showScoreDetails e =  "Total " ++ show e ++ " made up of " ++ (e ^. details)

data ChessNode = ChessNode { _chessTreeLoc :: TreeLocation, _chessMv :: ChessMove
                           , _chessVal :: ChessEval , _chessErrorVal :: ChessEval , _chessPos :: ChessPos
                           , _chessMvSeq :: [ChessMove] , _chessIsEvaluated :: Bool }
    deriving (Eq, GHC.Generics.Generic, Hashable)
makeLenses ''ChessNode

instance Show ChessNode where
    -- show n = "move: " ++ show (n ^. chessMv) ++ ", value: " ++ show (n ^. chessVal)
    --          ++ ", history: " ++ show (n ^. chessMvSeq)
  show = cnShowMoveOnly

cnShowMoveOnly :: ChessNode -> String
cnShowMoveOnly cn = show (cn ^. chessMv)

evalChessNode :: ChessNode -> Float
evalChessNode cn = cn ^. (chessVal . total)

instance (TreeNode ChessNode m) =>  Z.ZipTreeNode ChessNode where
  ztnEvaluate = evalChessNode
  ztnMakeChildren = makeChessNodeChildren
  ztnSign cn = colorToSign (cn ^. (chessPos . cpState . cpsColorToMove))
  ztnFinal cn = cn ^. (chessPos . cpFin) /= NotFinal
  ztnDeepDescend = critsOnly

makeChessNodeChildren :: TreeNode ChessNode m => ChessNode -> [Tree ChessNode]
makeChessNodeChildren = Helpers.makeChildren

colorToSign :: Color -> Z.Sign
colorToSign White = Z.Pos
colorToSign _ = Z.Neg

critsOnly :: TreeNode n m => n -> Bool
critsOnly = critical

---------------------------------------------------------------------------------------------------
data ChessMoves = ChessMoves
  { _cmEmpty :: [ChessMove],
    _cmEnemy :: [ChessMove],
    _cmForColor :: Color -- for debugging output
  }
  deriving (Eq)

makeLenses ''ChessMoves

instance Show ChessMoves where
  show ChessMoves{..} = "Moves for color: " ++ show _cmForColor
    ++  "\nEmpty: " ++ show _cmEmpty ++ "\nEnemy: " ++ show _cmEnemy

---------------------------------------------------------------------------------------------------
-- Weird Haskell bug? (GHC 9.4.7) -- these two versions do not behave the same:
-- In the commented-out code, 'colorToChar White' fails with a non-exaustive pattern match
-- In the second version, it works correctly
---------------------------------------------------------------------------------------------------
-- colorToChar :: Color -> Char
-- colortoChar White = 'w'
-- colorToChar Black = 'b'
colorToChar :: Color -> Char
colorToChar clr =
  case clr of
    White -> 'w'
    Black -> 'b'

colorToInt :: Color -> Int
colorToInt Black = -1
colorToInt White = 1

colorFromInt :: Int -> Color
colorFromInt 1 = White
colorFromInt _ = Black

enemyColor :: Color -> Color
enemyColor White = Black
enemyColor Black = White

enemyColorMay :: Maybe Color -> Maybe Color
enemyColorMay Nothing = Nothing
enemyColorMay (Just c) = Just (enemyColor c)

colorToTupleElem :: Color -> (a, a) -> a
colorToTupleElem White (x, _y) = x
colorToTupleElem _ (_x, y) = y

colorToTuple :: Color -> (a, a) -> a -> (a, a)
colorToTuple White (_x, y) x' = (x', y)
colorToTuple _ (x, _y) y' = (x, y')

---------------------------------------------------------------------------------------------------
-- get piece locations for a given color from a board
-- (pre-calculated via calcLocsForColor)
--------------------------------------------------------------------------------------------------
locsForColor :: ChessPos -> ([(Int, Char, Color)], [(Int, Char, Color)])
locsForColor cp = (_cpWhitePieceLocs cp, _cpBlackPieceLocs cp)

calcLocsForColor :: ChessGrid -> ([(Int, Char, Color)], [(Int, Char, Color)])
calcLocsForColor locs' =
    let locs = unGrid locs'
    in V.ifoldr f ([], []) locs where
        f :: Int -> Char -> ([(Int, Char, Color)], [(Int, Char, Color)])
                         -> ([(Int, Char, Color)], [(Int, Char, Color)])
        f n c (wLocs, bLocs) =
            case asciiToColor c of
                (Just White) -> ((n, c, White) : wLocs, bLocs)
                (Just Black) -> (wLocs, (n, c, Black) : bLocs)
                Nothing    -> (wLocs, bLocs)

--------------------------------------------------------------------------------------------------
-- convert ascii piece representations to piece color
-- i.e. upper case -> White, lower case -> Black
--------------------------------------------------------------------------------------------------
asciiToColor :: Char -> Maybe Color
asciiToColor c
  | ord c > 64 && ord c < 91 = Just White  -- A - Z : 65 - 90
  | ord c > 96 && ord c < 123 = Just Black -- | a - z : 97 - 122
  | otherwise = Nothing
{-# INLINE asciiToColor #-}

--------------------------------------------------------------------------------------------------
-- convert char representations of color (i.e., 'w', 'W', 'b', 'B') to the Color type
--------------------------------------------------------------------------------------------------
charToColor :: Char -> Maybe Color
charToColor c | c == 'w' = Just White
              | c == 'W' = Just White
              | c == 'b' = Just Black
              | c == 'B' = Just Black
              | otherwise = Nothing

---------------------------------------------------------------------------------------------------
findKingLocs :: Vector Char -> (Int, Int)
findKingLocs g =
    V.ifoldr f (0, 0) g
      where
        f :: Int -> Char -> (Int, Int) -> (Int, Int)
        f idx 'K' (_, accB) = (idx, accB)
        f idx 'k' (accW, _) = (accW, idx)
        f idx _ (accW, accB) = (accW, accB)

----------------------------------------------------------------------------------------------------
-- New, new attempt with singletons
----------------------------------------------------------------------------------------------------
$(singletons [d|
  data Piece = King | Queen | Rook | Knight | Bishop | Pawn | None | OffBoardNone
    deriving (Show, Eq)
  |])

data ChessPiece :: SomeSing Piece -> Type where
  MkChessPiece :: Color -> SomeSing Piece -> ChessPiece k

intToPiece :: Int -> ChessPiece (k :: SomeSing Piece)
intToPiece i = charToPiece (chr i)

pieceToInt :: Piece -> Color -> Int
pieceToInt p c = ord $ pieceToChar p c

pieceToChar :: Piece -> Color -> Char
pieceToChar King White = 'K'
pieceToChar King Black = 'k'
pieceToChar Queen White = 'Q'
pieceToChar Queen Black = 'q'
pieceToChar Rook White = 'R'
pieceToChar Rook Black = 'r'
pieceToChar Knight White = 'N'
pieceToChar Knight Black = 'n'
pieceToChar Bishop White = 'B'
pieceToChar Bishop Black = 'b'
pieceToChar Pawn White = 'P'
pieceToChar Pawn Black = 'p'
pieceToChar None _ = ' '
pieceToChar OffBoardNone _ = '+'

charToPiece :: Char -> ChessPiece (k :: SomeSing Piece)
charToPiece 'K' = MkChessPiece White (SomeSing SKing)
charToPiece 'Q' = MkChessPiece White (SomeSing SQueen)
charToPiece 'R' = MkChessPiece White (SomeSing SRook)
charToPiece 'N' = MkChessPiece White (SomeSing SKnight)
charToPiece 'B' = MkChessPiece White (SomeSing SBishop)
charToPiece 'P' = MkChessPiece White (SomeSing SPawn)
charToPiece 'k' = MkChessPiece Black (SomeSing SKing)
charToPiece 'q' = MkChessPiece Black (SomeSing SQueen)
charToPiece 'r' = MkChessPiece Black (SomeSing SRook)
charToPiece 'n' = MkChessPiece Black (SomeSing SKnight)
charToPiece 'b' = MkChessPiece Black (SomeSing SBishop)
charToPiece 'p' = MkChessPiece Black (SomeSing SPawn)
charToPiece ' ' = MkChessPiece Black (SomeSing SNone)
charToPiece '+' = MkChessPiece Black (SomeSing SOffBoardNone)
charToPiece ch = error $ "charToPiece not implemented for the value " ++ show ch

indexToColor2 :: ChessGrid -> Int -> Maybe Color
indexToColor2 g idx =
  let g' = unGrid g
  in asciiToColor (g' ! idx)
{-# INLINE indexToColor2 #-}

indexToChar :: ChessGrid -> Int -> Char
indexToChar g idx =
  let g' = unGrid g
  in fromMaybe ' ' (g' ^? ix idx)

pieceVal :: ChessPiece (k :: SomeSing Piece) -> Float
pieceVal piece@(MkChessPiece c _) =
  let absVal = pieceAbsVal piece
  in case c of
    White -> absVal
    Black -> negate absVal

pieceAbsVal :: ChessPiece (k :: SomeSing Piece) -> Float
pieceAbsVal (MkChessPiece _c (SomeSing SKing)) = Z.maxScore
pieceAbsVal (MkChessPiece _c (SomeSing SQueen)) = 9.0
pieceAbsVal (MkChessPiece _c (SomeSing SRook)) = 5.0
pieceAbsVal (MkChessPiece _c (SomeSing SKnight)) = 3.0
pieceAbsVal (MkChessPiece _c (SomeSing SBishop)) = 3.0
pieceAbsVal (MkChessPiece _c (SomeSing SPawn)) = 1.0
pieceAbsVal (MkChessPiece _c (SomeSing SNone)) = 0.0
pieceAbsVal (MkChessPiece _c (SomeSing SOffBoardNone)) = 0.0

mobilityCountFromLoc :: ChessPos -> (Int, Char, Color) -> Int
mobilityCountFromLoc cp loc@(idx, _, _) =
  let g = cp ^. cpGrid
      ch = toUpper $ indexToChar g idx
  in case ch of
      'K' -> kingMobility cp loc
      'Q' -> queenMobility cp loc
      'R' -> rookMobility g loc
      'N' -> knightMobility g loc
      'B' -> bishopMobility g loc
      _   -> 0

mobilityCountFromLocs :: ChessPos -> [(Int, Char, Color)] -> Int
mobilityCountFromLocs cp =
  foldr f 0 where
    f :: (Int, Char, Color) -> Int -> Int
    f loc r =
      let cnt = mobilityCountFromLoc cp loc
      in cnt + r

---------------------------------------------------------------------------------------------------
-- Calculate the 'mobility' score for the pieces on the board
--------------------------------------------------------------------------------------------------
calcMobility :: ChessPos -> Float
calcMobility cp =
  let g = cp ^. cpGrid
      (wLocs, bLocs) = locsForColor cp
      wCnt = mobilityCountFromLocs cp wLocs
      bCnt = mobilityCountFromLocs cp bLocs
  in fromIntegral (wCnt - bCnt)

calcPiecePositionScore :: Map Int Float -> [Int] -> Float
calcPiecePositionScore theMap locs =
    let f :: Int -> Float -> Float
        f loc acc =  M.findWithDefault 0 loc theMap + acc
    in foldr f 0.0 locs

---------------------------------------------------------------------------------------------------
-- Calculate a score for Knight positioning
--------------------------------------------------------------------------------------------------
calcKnightPositionScore :: ChessPos -> Float
calcKnightPositionScore cp =
  let (wFiltered, bFiltered) = filterLocsByChar (locsForColor cp) 'N'
      wLocs = map (\(loc, _, _) -> loc) wFiltered
      bLocs = map (\(loc, _, _) -> loc) bFiltered
      wTotal = calcPiecePositionScore knightAdjustments wLocs
      bTotal = calcPiecePositionScore knightAdjustments bLocs
  in wTotal - bTotal

knightAdjustments :: Map Int Float
knightAdjustments = M.fromList
    [ (23, 4.0), (32, 4.0), (52, 4.0), (63, 4.0), (65, 4.0), (56, 4.0), (36, 4.0), (25, 4.0)
    , (24, 4.0), (33, 4.0), (53, 4.0), (64, 4.0), (66, 4.0), (57, 4.0), (37, 4.0), (26, 4.0)
    , (33, 4.0), (42, 4.0), (62, 4.0), (73, 4.0), (75, 4.0), (66, 4.0), (46, 4.0), (35, 4.0)
    , (34, 4.0), (43, 4.0), (63, 4.0), (74, 4.0), (76, 4.0), (67, 4.0), (47, 4.0), (36, 4.0) ]

filterLocsByChar :: ([(Int, Char, Color)], [(Int, Char, Color)]) -> Char
                 -> ([(Int, Char, Color)], [(Int, Char, Color)])
filterLocsByChar (wLocs, bLocs) charToMatch =
  let wFiltered = filter (f (toUpper charToMatch)) wLocs
      bFiltered = filter (f (toLower charToMatch)) bLocs
      -- f c = \(_, ch, _) -> ch == c
      f c (_, ch, _) = ch == c
  in (wFiltered, bFiltered)

wRookCastlingAdjustments :: Map Int Float
wRookCastlingAdjustments = M.fromList [(11, 0.1), (18, 1.0)]

bRookCastlingAdjustments :: Map Int Float
bRookCastlingAdjustments = M.fromList [(81, 0.1), (88, 1.0)]

castlingToAbsVal :: Castling -> Float
castlingToAbsVal Castled = 3.0
castlingToAbsVal Unavailable = -3.0
castlingToAbsVal BothAvailable = 0.0
castlingToAbsVal KingSideOnlyAvailable = -1.0
castlingToAbsVal QueenSideOnlyAvailable = -1.0

---------------------------------------------------------------------------------------------------
-- Count the 'material' score for the pieces on the board
--------------------------------------------------------------------------------------------------
countMaterial :: ChessGrid -> Float
countMaterial g =
  let g' = unGrid g
  in V.foldr f 0 g'
  where
    f ch theTotal =
        if ch == empty
          then theTotal
          else theTotal + pieceVal (charToPiece ch)

{- TODOs:
-- Convert more stored positions to FEN
-- Add :? cmd that shows the available commands
-- Add verbose, brief, etc. -- print out other moves not picked (move sequence), on vv print evals also
-- Add warning message if non-quiet move chosen, consider not picking those
-- Auto depth increase in endgame
-- Add way to detect castling score bonus when loading from FEN
-- Another round of perfomance opt
-- Add pawn promotion other than queen
-- Keep track of 'half move clock' for 50 move draws -- implement in to/from FEN
-- Determine mate in n and display
-- Remove TemplateHaskell (remove TH generated Lenses, manually make only needed lenses (deep updates)
-- Various command line debug tools
--    load
--    save
--    show eval detail
--    change level
-- Limit positive score eval for moves like A7-A6 only when there is a knight at A3 or C3
-- And add evaluation scores for:
      score penalty if pawns at both A3 and B3, etc.
      outposts
      rooks (or queen) on 7th (or 8th) rank
      doubled pawns
      having both bishops
      slight pref. to kingside castle over queenside
      pawn advancement
      trading (esp. queens) only when ahead
      good bishop vs bad bishop (blocked by its own pawns)
      'weak color complex' around the king(s)
-}
---------------------------------------------------------------------------------------------------
-- Evaluate and produce a score for the position
--------------------------------------------------------------------------------------------------
evalPos :: ChessPos -> (ChessEval, FinalState)
evalPos pos =
     let material = countMaterial (pos ^. cpGrid)
         mobility = calcMobility pos
         castling = calcCastling pos
         development = calcDevelopment pos
         earlyQueen = calcEarlyQueen pos
         pawnPositionScore = calcPawnPositionScore pos
         knightPositionScore = calcKnightPositionScore pos
         connectedRookScore = calcConnectedRookScore pos
         rookOpenFileScore = calcRookOpenFileScore pos
         finalState = checkFinal' pos
         (t, detailsStr) =
            if finalState /= NotFinal then
                let finalScore = finalStateToScore finalState
                in (finalScore, "\n\tThe position is final: " ++ show finalScore )
            else
              ( (material * 30.0 ) + (mobility * 1) + (castling * 9.0)
              + (development * 5.0) + (earlyQueen * 15.0) + (pawnPositionScore * 2)
              + knightPositionScore + (connectedRookScore * 25.0) + (rookOpenFileScore * 25.0)
              ,  "\n\tMaterial (x 30.0): " ++ show (material * 30.0)
                  ++ "\n\tMobility (x 1): " ++ show (mobility * 1)
                  ++ "\n\tCastling (x 9): " ++ show (castling * 9.0)
                  ++ "\n\tDevelopment (x 5): " ++ show (development * 5.0)
                  ++ "\n\tQueen dev. early (x 15): " ++ show (earlyQueen * 15.0)
                  ++ "\n\tPawn position score (x 2): " ++ show (pawnPositionScore * 2.0)
                  ++ "\n\tKnight position score (x 1): " ++ show knightPositionScore
                  ++ "\n\tConnected rook score (x 25): " ++ show (connectedRookScore * 25.0)
                  ++ "\n\tRook open file score (x 25): " ++ show (rookOpenFileScore * 25.0))
         eval = ChessEval { _total = t
                          , _details = detailsStr
                          }
     in (eval, finalState)

finalStateToScore :: FinalState -> Float
finalStateToScore WWins = Z.maxScore
finalStateToScore BWins = Z.minScore
finalStateToScore _ = 0.0

---------------------------------------------------------------------------------------------------
-- Create a string for the current position in FEN (Forsyth-Edwards Notation)
---------------------------------------------------------------------------------------------------
toFen :: ChessPos -> String
toFen cp =
    let state = _cpState cp
        v = unGrid $ _cpGrid cp
        rows y acc = scanRow v y ++ "/" ++ acc
        strPlus = foldr rows "" [88, 78, 68, 58, 48, 38, 28, 18]
        boardFen = take (length strPlus - 1) strPlus -- drop the final '/'
        nextColor = [colorToChar $ _cpsColorToMove state]
        castlingPair = _cpsCastling state
        castlingStr = fenCastlingToChars castlingPair
        enPassantStr = maybe "-" locToAlgebraic (_cpsEnPassant state)
        halfMoves = "0" -- not yet implemented
        fullMoves = show (_cpsMoveNumber state `div` 2)
    in boardFen ++ " " ++ nextColor ++ " " ++ castlingStr ++ " " ++ enPassantStr ++ " "
       ++ halfMoves ++ " " ++ fullMoves

fromFen :: String -> Either String (Tree ChessNode, ChessPosState)
fromFen fenStr =
    case FP.parseFen fenStr of
        Left err -> Left err
        Right fenData@FP.FenData{..} ->
          let initialV = emptyGrid
              rowStrs = T.unpack <$> rowData
              rowIdxs :: [Int] = [81, 71, 61, 51, 41, 31, 21, 11]
              pairs = zip rowStrs rowIdxs
              newV :: Vector Char = F.foldl' foldF initialV pairs
              grid = ChessGrid newV
              (wKing, bKing) = findKingLocs newV
              locPair@(wLocs, bLocs) = calcLocsForColor grid
              cpState = fenDataToCpState fenData
              colorToMove = _cpsColorToMove cpState
              kingLocs = findKingLocs newV
              cp = ChessPos
                   { _cpGrid = grid
                   , _cpKingLoc = kingLocs
                   , _cpInCheck = False
                   , _cpWhitePieceLocs = wLocs
                   , _cpBlackPieceLocs = bLocs
                   , _cpFin = NotFinal -- can be anything, will be replaced
                   , _cpState = cpState }
              inCheck = isKingInCheckFull grid colorToMove (colorToTupleElem colorToMove kingLocs)
          in Right
              ( Node ChessNode
                { _chessTreeLoc = TreeLocation {tlDepth = 0}
                , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
                , _chessVal = ChessEval { _total = 0.0, _details = "" }
                , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
                , _chessPos = cp {_cpFin = snd $ evalPos cp, _cpInCheck = inCheck }
                , _chessMvSeq = []
                , _chessIsEvaluated = False } []
              , cpState)
            where
              foldF :: Vector Char -> ([Char], Int) -> Vector Char
              foldF accV (str, n) = fst (F.foldl' (foldF2 n) (accV, 0) str)

foldF2 :: Int -> (Vector Char, Int) -> Char -> (Vector Char, Int)
foldF2 n (accV, idx) x =
    case x of
        ch | ch > '0' && ch < '9' -> (accV, idx + digitToInt ch)
           | otherwise            -> (set (ix (idx+n)) ch accV, idx + 1)

emptyGrid :: Vector Char
emptyGrid =
   V.concat
     [ V.fromList topOrBottom
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList middle
     , V.fromList topOrBottom ]
  where
    topOrBottom = ['+', '+', '+', '+', '+', '+', '+', '+', '+', '+']
    middle      = ['+', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '+']

fenDataToCpState :: FP.FenData -> ChessPosState
fenDataToCpState FP.FenData {..} =
    ChessPosState
    { _cpsColorToMove = fromJust $ charToColor nextColor
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = halfMovesForDraw
    , _cpsMoveNumber = moveNumber
    , _cpsCastling = fenDataToCastling castling
    , _cpsEnPassant = algebraicToLoc $ T.unpack <$> enPassant }

scanRow :: Vector Char -> Int -> String
scanRow v idx =
    let loop :: Int -> Int -> String -> String
        loop i curSpaces str =
          case i `mod` 10 of
            0 -> if curSpaces == 0
                     then str
                     else intToDigit curSpaces : str
            _ -> case v!i of
                    ' ' -> loop (i-1) (curSpaces+1) str
                    ch  -> if curSpaces == 0
                              then loop (i-1) 0 (ch : str)
                              else loop (i-1) 0 (ch : (intToDigit curSpaces : str))
    in loop idx 0 ""

locToAlgebraic :: Int -> String
locToAlgebraic n =
  let Parser.Loc c r = intToParserLoc n
  in c : show r

algebraicToLoc :: Maybe String -> Maybe Int
algebraicToLoc Nothing = Nothing
algebraicToLoc (Just "-") = Nothing
alegebraicToLoc (Just s) =
    -- parser quarentees a string of length 2 of the form, e.g., "e4"
    let s0 = head s
        digit = digitToInt s0
        loc = Parser.Loc (head (tail s)) digit
    in Just $ locToInt loc

fenCastlingToChars :: (Castling, Castling) -> String
fenCastlingToChars (wCastling, bCastling) =
    let wCastlingChars = upper $ castlingChars wCastling
        bCastlingChars = castlingChars bCastling
    in if wCastlingChars == "" && bCastlingChars == ""
         then "-"
         else wCastlingChars ++ bCastlingChars
    where
      castlingChars :: Castling -> String
      castlingChars Castled = ""
      castlingChars KingSideOnlyAvailable = "k"
      castlingChars QueenSideOnlyAvailable = "q"
      castlingChars BothAvailable = "kq"
      castlingChars Unavailable = ""

fenDataToCastling :: (Maybe Text, Maybe Text) -> (Castling, Castling)
fenDataToCastling (w, b) = (textToCastling w, textToCastling b)

textToCastling :: Maybe Text -> Castling
textToCastling Nothing = Unavailable
textToCastling (Just t) =
    let s = lower $ T.unpack t
    in case s of
        "kq" -> BothAvailable
        "k" -> KingSideOnlyAvailable
        "q" -> QueenSideOnlyAvailable
        _   -> Unavailable

castlingToInt :: Castling -> Int
castlingToInt Castled = 0
castlingToInt Unavailable = 0
castlingToInt KingSideOnlyAvailable = 1
castlingToInt QueenSideOnlyAvailable = 2
castlingToInt BothAvailable =  3

intToCastling :: Int -> Maybe Castling
intToCastling 0 = Just Unavailable
intToCastling 1 = Just KingSideOnlyAvailable
intToCastling 2 = Just QueenSideOnlyAvailable
intToCastling 3 = Just BothAvailable -- Note: this does not say whether castling has been done or not
intToCastling _ = Nothing

fenCharsToCastling :: String -> Maybe (Castling, Castling)
fenCharsToCastling str =
  let neither = 0
  in if null str || length str > 4
     then Nothing
     else if str == "-" then Just (Unavailable, Unavailable)
     else let (w, b, _) = foldr foldf (neither, neither, White) str -- Note: White chars before black here
          in do
            w' <- intToCastling w
            b' <- intToCastling b
            return (w', b')
        where
            foldf :: Char -> (Int, Int, Color) -> (Int, Int, Color)
            foldf ch (accW, accB, curColor) =
                let kside = 1
                    qside = 2
                in case ch of
                    'K' -> (accW + kside, accB, White)
                    'Q' -> (accW + qside, accB, White)
                    'k' -> (accW, accB + kside, Black)
                    'q' -> (accW, accB + qside, Black)

showChessNodeAs :: ChessNode -> String -> String
showChessNodeAs cn formatType =
  if upper formatType == "FEN"
    then toFen $ _chessPos cn
    else show cn

instance TreeNode ChessNode ChessMove where
    newNode = calcNewNode
    possibleMoves = legalMoves
    color = colorToInt . view (chessPos . cpState . cpsColorToMove)
    final = checkFinal
    critical = isCritical
    parseEntry = parseChessEntry
    getMove = _chessMv
    treeLoc = _chessTreeLoc
    undoMove = undoChessMove
    moveNum n = n ^. (chessPos . cpState . cpsMoveNumber)

instance Move ChessMove

instance Eval ChessNode where
    evaluate = evalChessNode
    isEvaluated cn = cn ^. chessIsEvaluated
    setFloat cn x = cn & (chessVal . total) .~ x

instance Ord ChessNode where
    (<=) cn1 cn2 = evaluate cn1 <= evaluate cn2

---------------------------------------------------------------------------------------------------
-- Grid layout - indexes 0-99
---------------------------------------------------------------------------------------------------
{- how indexes relate to board position (indexes in parens are off the edge of the board):

   (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

8| (80)  81   82   83   84   85   86   87   88  (89)
7| (50)  71   72   73   74   75   76   77   78  (79)
6| (50)  61   62   63   64   65   66   67   68  (69)
5| (50)  51   52   53   54   55   56   57   58  (59)
4| (40)  41   4   43   44   45   46   47   48  (49)
3| (30)  31   32   33   34   35   36   37   38  (39)
2| (20)  21   22   23   24   25   26   27   28  (29)
1| (10)  11   12   13   14   15   16   17   18  (19)

   (00) (01) (02) (03) (04) (05) (06) (07) (08) (09)
   -------------------------------------------------
         A    B    C    D    E    F    G    H

    An index x is off the board if:
        x < 10 or
        x > 88 or
        x mod 10 = 0 or
        x mod 10 = 9

    Direction   add/subtract        Direction   add/subtract
    Right       1                   Knight LU   +8    -- first listed direction is the longer side of
                                                      -- the 'L'-move, so UL is 2 up and one left,
                                                      -- while LU is 2 left and 1 up.
    Left        -1                  Knght  RD   -8
    Up          10                  Knight LD   -12
    Down        -10                 Knight RU   +12
    Diag U/L    +9                  Knight UL   +19
    Diag D/R    -9                  Knight DR   -19
    Diag U/R    +11                 Knight UR   +21
    Diag D/L    -11                 Knight DL   -21

Piece representation as integers:

75 = 0x4b = 'K' = white King     107 = 0x6b = 'k' = black King
81 = 0x51 = 'Q' = white Queen    113 = 0x71 = 'q' = black Queen
82 = 0x52 = 'R' = white Rook     114 = 0x72 = 'r' = black Rook
78 = 0x4e = 'N' = white Knight   110 = 0x6e = 'n' = black Knight
66 = 0x42 = 'B' = white Bishop   098 = 0x62 = 'b' = black Bishop
80 = 0x50 = 'P' = white Pawn     112 = 0x70 = 'p' = black Pawn

32 = 0x20 = ' ' = Empty Squre
43 = 0x2b = '+' = off the board
-}

----------------------------------------------------------------------------------------------------
-- starting position, white at the 'bottom'
----------------------------------------------------------------------------------------------------
startingBoard :: ChessGrid
startingBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  'P',  'P',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'p',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   P   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

newGameTree :: (Tree ChessNode, ChessPosState)
newGameTree =
    case getNodeFromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0" of
      Left err -> error "newGameTree returned left??" -- this shouldn't happen
      Right r -> r

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   P   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

---------------------------------------------------------------------------------------------------
-- start new game, or load via game name
---------------------------------------------------------------------------------------------------
getStartNode :: String -> (Tree ChessNode, ChessPosState)
getStartNode restoreGame =
    let bottomColor = White -- TBD allow either color
    in case restoreGame of
      "newgame" -> newGameTree
      "alphabeta" ->
        let (wLocs, bLocs) = calcLocsForColor alphaBetaBoard
            cPos = ChessPos
              { _cpGrid = alphaBetaBoard
              , _cpKingLoc = (11, 31)
              , _cpInCheck = False
              , _cpWhitePieceLocs = wLocs
              , _cpBlackPieceLocs = bLocs
              , _cpFin = NotFinal
              , _cpState = castledTestState White}
        in ( Node ChessNode
              { _chessTreeLoc = TreeLocation {tlDepth = 0}
              , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
              , _chessVal = ChessEval { _total = 0.0, _details = "" }
              , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
              , _chessPos = cPos
              , _chessMvSeq = []
              , _chessIsEvaluated = False } []
           , castledTestState White)
      "discovered" -> discoveredCheckTree
      "checkmate"  -> (Node checkMateExampleNode [], preCastledTestState White)
      "checkmate2" -> (Node checkMateExampleNode2 [], preCastledTestState White)
      "checkmate3" -> (Node checkMateExampleNode3 [], preCastledTestState White)
      "checkmate4" -> (Node checkMateExampleNode4 [], preCastledTestState White)
      "mateInTwo01" -> mateInTwoTree01
      "mateInTwo02" -> (Node mateInTwoExampleNode02 [], preCastledTestState Black)
      "mateInTwo02b" -> (Node mateInTwoExampleNode02b [], preCastledTestState Black)
      "mateInTwo03" -> (Node mateInTwoExampleNode03 [], castledTestState Black)
      "mateInTwo03b" -> (Node mateInTwoExampleNode03b [], castledTestState Black)
      "mateInThree01" -> mateInThreeTree01
      "mateInThree02" -> mateInThreeTree02
      "invalidTree"   -> invalidTree
      "promotion01"  -> (Node promotionNode01 [], castledTestState White)
      "critBug01"    -> (Node critBugNode01 [], preCastledTestState Black)
      "debug"        -> (Node debugExampleNode [], preCastledTestState White)
      "debug02"      -> (Node debugExampleNode02 [], preCastledTestState White)
      "debug03"      -> (Node debugExampleNode03 [], preCastledTestState Black)
      "debug04"      -> (Node debugExampleNode04 [], preCastledTestState Black)
      "debug05"      -> (Node debugExampleNode05 [], preCastledTestState Black)
      "debug06"      -> (Node debugExampleNode06 [], preCastledTestState Black)
      "draw"         -> (Node drawnExampleNode [], preCastledTestState White)
      "castling"     -> (Node castlingNode [], preCastledTestState White)
      "debug06b"     -> (Node debugExampleNode06b [], preCastledTestState White)
      "debug06c"     -> (Node debugExampleNode06c [], preCastledTestState Black)
      "debug07"      -> (Node debugExampleNode07 [], preCastledTestState Black)
      "enpassant"    -> (Node enPassantNode01 [], preCastledTestState Black)
      "enpassant2"    -> (Node enPassantNode02 [], preCastledTestState Black)
      "castling"     -> (Node castlingNode [], preCastledTestState Black)
      "connectrooks" -> (Node connectRookNode [], castledTestState Black)
      "endgame01"    -> (Node endgameNode01 [], castledTestState White)
      _ -> error "unknown restore game string - choices are:\n \
                 \ alphabeta, \n \
                 \ castling, checkmate, checkmate2, checkmate3, checkmate4, critBug01, \n \
                 \ discovered, \n \
                 \ mateInTwo01, mateInTwo02, mateInTwo02b, mateInTwo03, mateInTwo03b, \n \
                 \ mateInThree01, mateInThree02, \n \
                 \ invalidTree, \n \
                 \ promotion01, \n \
                 \ debug, debug02, debug03, debug04, debug05, \n \
                 \ debug06, debug06b, debug06c, debug07, debug08, \n \
                 \ enpassant, enpassant2, connectrook, endgame01, \n \
                 \ draw, newgame"


---------------------------------------------------------------------------------------------------
-- Create game from FEN
---------------------------------------------------------------------------------------------------
getNodeFromFen :: String -> Either String (Tree ChessNode, ChessPosState)
getNodeFromFen = fromFen

-- Color represents the color at the 'bottom' of the board
mkStartGrid :: Color -> ChessGrid
mkStartGrid White = startingBoard
mkStartGrid Black =
  let g' = unGrid startingBoard
  in ChessGrid $ V.reverse g'

newGameState :: ChessPosState
newGameState = ChessPosState
    { _cpsColorToMove = White
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = 0
    , _cpsMoveNumber = 0
    , _cpsCastling = (BothAvailable, BothAvailable)
    , _cpsEnPassant = Nothing
    }

castledTestState :: Color -> ChessPosState
castledTestState clr = castledTestState' clr 0

castledTestState' :: Color -> Int -> ChessPosState
castledTestState' clr moveNum = ChessPosState
    { _cpsColorToMove = clr
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = 0
    , _cpsMoveNumber = moveNum
    , _cpsCastling = (Castled, Castled)
    , _cpsEnPassant = Nothing
    }

preCastledTestState :: Color -> ChessPosState
preCastledTestState clr = ChessPosState
    { _cpsColorToMove = clr
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = 0
    , _cpsMoveNumber = 0
    , _cpsCastling = (BothAvailable, BothAvailable)
    , _cpsEnPassant = Nothing
    }

data Direction = RightDir | LeftDir | UpDir | DownDir | DiagULDir | DiagDRDir | DiagURDir | DiagDLDir

toDir :: Direction -> Dir
toDir RightDir = right
toDir LeftDir = left
toDir UpDir = up
toDir DownDir = down
toDir DiagULDir = diagUL
toDir DiagDRDir = diagDR
toDir DiagURDir = diagUR
toDir DiagDLDir = diagDL

toDirection :: Dir -> Direction
toDirection d = case d 0 of
    1   -> RightDir
    -1  -> LeftDir
    10  -> UpDir
    -10 -> DownDir
    9   -> DiagULDir
    -9  -> DiagDRDir
    11  -> DiagURDir
    -11 -> DiagDLDir

flipDirection :: Direction -> Direction
flipDirection RightDir = LeftDir
flipDirection LeftDir = RightDir
flipDirection UpDir = DownDir
flipDirection DownDir = UpDir
flipDirection DiagULDir = DiagDRDir
flipDirection DiagDRDir = DiagULDir
flipDirection DiagURDir = DiagDLDir
flipDirection DiagDLDir = DiagURDir

charHasDirection :: Char -> Direction -> Bool
charHasDirection ch d =
  let sg = directionToSlideGroup d
  in charInSlideGroup sg ch

data SlideGroup = Diagonal | Rectangular

directionToSlideGroup :: Direction -> SlideGroup
directionToSlideGroup RightDir = Rectangular
directionToSlideGroup LeftDir = Rectangular
directionToSlideGroup UpDir = Rectangular
directionToSlideGroup DownDir = Rectangular
directionToSlideGroup DiagURDir = Diagonal
directionToSlideGroup DiagDRDir = Diagonal
directionToSlideGroup DiagULDir = Diagonal
directionToSlideGroup DiagDLDir = Diagonal

charInSlideGroup :: SlideGroup -> Char -> Bool
charInSlideGroup sg ch = charInSlideGroup' sg $ toUpper ch

charInSlideGroup' :: SlideGroup -> Char -> Bool
charInSlideGroup' _ 'K' = False
charInSlideGroup' _ 'P' = False
charInSlideGroup' _ 'N' = False
charInSlideGroup' Diagonal 'Q' = True
charInSlideGroup' Diagonal 'R' = False
charInSlideGroup' Diagonal 'B' = True
charInSlideGroup' Rectangular 'Q' = True
charInSlideGroup' Rectangular 'R' = True
charInSlideGroup' Rectangular 'B' = False

type Dir = Int -> Int
right, left, up, down, diagUL, diagDR, diagUR, diagDL :: Dir
right x = x + 1
{-# INLINE right #-}
left x = x - 1
{-# INLINE left #-}
up x = x + 10
{-# INLINE up #-}
down x = x - 10
{-# INLINE down #-}
diagUL x  = x + 9
{-# INLINE diagUL #-}
diagDR x = x - 9
{-# INLINE diagDR #-}
diagUR x = x + 11
{-# INLINE diagUR #-}
diagDL x = x - 11
{-# INLINE diagDL #-}

knightLU, knightRD, knightRU, knightLD, knightUL, knightDR, knightUR, knightDL :: Dir
knightLU x = x + 8
{-# INLINE knightLU #-}
knightRD x = x - 8
{-# INLINE knightRD #-}
knightRU x = x + 12
{-# INLINE knightRU #-}
knightLD x = x - 12
{-# INLINE knightLD #-}
knightUL x = x + 19
{-# INLINE knightUL #-}
knightDR x = x - 19
{-# INLINE knightDR #-}
knightUR x = x + 21
{-# INLINE knightUR #-}
knightDL x = x - 21
{-# INLINE knightDL #-}

queenDirs :: [Dir]
queenDirs = [right, left, up, down, diagUL, diagDR, diagUR, diagDL]

kingDirs :: [Dir]
kingDirs = queenDirs

rookDirs :: [Dir]
rookDirs = [up, down, left, right]

bishopDirs :: [Dir]
bishopDirs = [diagUL, diagDR, diagUR, diagDL]

knightDirs :: [Dir]
knightDirs = [knightLU, knightRD, knightLD, knightRU, knightUL, knightDR, knightUR, knightDL]

whitePawnDir :: Dir -- non capturing moves...
whitePawnDir = up

whitePawnCaptureDirs :: [Dir]
whitePawnCaptureDirs = [diagUL, diagUR]

whitePawnEnPassantDirs :: [Dir]
whitePawnEnPassantDirs = whitePawnCaptureDirs

blackPawnDir :: Dir -- non capturing moves...
blackPawnDir = down

blackPawnCaptureDirs :: [Dir]
blackPawnCaptureDirs = [diagDL, diagDR]

blackPawnEnPassantDirs :: [Dir]
blackPawnEnPassantDirs = blackPawnCaptureDirs

noDirs :: [Dir]
noDirs = []

---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: ChessNode -> ChessMove -> TreeLocation -> ChessNode
calcNewNode node mv tLoc =
    let curPos = node ^. chessPos
        curMoveSeq = node ^. chessMvSeq
        curGrid = curPos ^. cpGrid
        curColor = curPos ^. (cpState . cpsColorToMove)
        curMoveNum = curPos ^. (cpState . cpsMoveNumber)
        mvIsCheck = moveIsCheck curPos mv
        epLoc = enPassantCaptureLoc curGrid mv

        (newGrid, _mvStartIdx, mvEndIdx) = case mv of
            m@StdMove{..} -> (movePiece' curGrid m _startIdx _endIdx, _startIdx, _endIdx)
            cm@CastlingMove{..} -> (castle' curGrid cm, _kingStartIdx, _kingEndIdx)
            ep@EnPassantMove{..} -> (movePiece' curGrid ep _epStartIdx _epEndIdx, _epStartIdx ,_epEndIdx )
        clrFlipped = flipPieceColor curColor
        kingLocs = curPos ^. cpKingLoc
        kingLocs' = updateKingLocs newGrid kingLocs mvEndIdx
        (whiteLocs, blackLocs) = calcLocsForColor newGrid
        newPos = curPos { _cpGrid = newGrid
                         , _cpWhitePieceLocs = whiteLocs
                         , _cpBlackPieceLocs = blackLocs
                         , _cpKingLoc = kingLocs'
                         , _cpInCheck = mvIsCheck
                         }
        newCastlingPair = castlingStatus newPos
        newState = (_cpState newPos)
          { _cpsColorToMove = clrFlipped, _cpsEnPassant = epLoc, _cpsCastling = newCastlingPair
          , _cpsMoveNumber = curMoveNum + 1 }
        posWithState = newPos { _cpState = newState }
        (eval, finalSt) = evalPos posWithState
        updatedPos = posWithState { _cpFin = finalSt }
    in ChessNode
        { _chessTreeLoc = tLoc
        , _chessMv = mv , _chessVal = eval
        , _chessErrorVal = ChessEval {_total = 0, _details = "not implemented"}
        , _chessPos = updatedPos
        , _chessMvSeq = mv : curMoveSeq
        , _chessIsEvaluated = False }

---------------------------------------------------------------------------------------------------
-- Update the kingLocs pair given the destination square of the move being applied
---------------------------------------------------------------------------------------------------
updateKingLocs :: ChessGrid -> (Int, Int) -> Int -> (Int, Int)
updateKingLocs grid kingLocs mvEndIdx =
    let ch = indexToChar grid mvEndIdx
    in if ch == 'K'
           then colorToTuple White kingLocs mvEndIdx
       else if ch == 'k'
           then colorToTuple Black kingLocs mvEndIdx
       else kingLocs

moveIsCheck :: ChessPos -> ChessMove -> Bool
moveIsCheck pos mv =
   let movingSideColor = _cpsColorToMove $ _cpState pos
       grid = _cpGrid pos
       kingColor = enemyColor movingSideColor
       enemyKingLoc = colorToTupleElem kingColor (_cpKingLoc pos)
   in moveIsDirectCheck grid mv movingSideColor
      || moveIsDiscoveredCheck grid mv movingSideColor enemyKingLoc

moveExposesKing :: ChessPos -> ChessMove -> Bool
moveExposesKing pos mv =
   let grid = _cpGrid pos
       movingSideColor = _cpsColorToMove $ _cpState pos
       kingLocs = _cpKingLoc pos
       movingSideKingLoc = colorToTupleElem movingSideColor (_cpKingLoc pos)
   in if _cpInCheck pos
        then moveClearsCheck grid mv movingSideColor kingLocs
        else moveExposesKingDirect grid mv movingSideColor
             || moveExposesKingDiscovered grid mv movingSideColor movingSideKingLoc

moveClearsCheck :: ChessGrid -> ChessMove -> Color -> (Int, Int) -> Bool
moveClearsCheck grid mv movingSideColor movingSideKingLoc =
    let (newGrid, _mvStartIdx, mvEndIdx) = case mv of
            m@StdMove{..} -> (movePiece' grid m _startIdx _endIdx, _startIdx, _endIdx)
            cm@CastlingMove{..} -> (castle' grid cm, _kingStartIdx, _kingEndIdx)
            ep@EnPassantMove{..} -> (movePiece' grid ep _epStartIdx _epEndIdx, _epStartIdx, _epEndIdx)
        newKingLoc = colorToTupleElem movingSideColor (updateKingLocs newGrid movingSideKingLoc mvEndIdx)
    in isKingInCheckFull newGrid movingSideColor newKingLoc

----------------------------------------------------------------------------------------------------
-- Is this move a direct (i.e., not 'discovered') check against the opposinig player? By 'direct'
-- we mean that the piece being moved provides the check. We do this by seeing if a subsequent move
-- from the piece's new location could 'capture' the enemy king. Thsee subsequent moves are standard moves,
-- not castling moves or enpassant moves
--   * If the initial move to be checked is a castling move, we need to search for subsequent rook moves
--     from the rook's castling destination (and ignore subsequent moves of the king from it's new location)
--   * enPassant moves can provide a direct check by the capturing pawn at its new location
----------------------------------------------------------------------------------------------------
moveIsDirectCheck :: ChessGrid -> ChessMove -> Color -> Bool
moveIsDirectCheck curGrid mv movingSideColor =
    let (startIdx, endIdx) = case mv of
            m@StdMove{..} -> (_startIdx, _endIdx)
            cm@CastlingMove{..} -> (_rookStartIdx, _rookEndIdx)
            ep@EnPassantMove{..} -> (_epStartIdx, _epEndIdx)
        movingChar = indexToChar curGrid startIdx
        newGrid = applyMoveToGrid curGrid mv

        -- movesFromLoc2 ignores castling and enpassant moves as required
        (_empties, enemies) = movesFromLoc2 newGrid (endIdx, movingChar, movingSideColor)
    in containsKingCapture newGrid enemies

movesFromLoc2 ::ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
movesFromLoc2 g loc@(idx, _, _) =
  let ch = toUpper $ indexToChar g idx
  in case ch of
      'K' -> allowableKingMovesNC g loc
      'Q' -> allowableQueenMoves g loc
      'R' -> allowableRookMoves g loc
      'N' -> allowableKnightMoves g loc
      'B' -> allowableBishopMoves g loc
      'P' -> allowablePawnMovesNEP g loc
      _   -> ([], [])

----------------------------------------------------------------------------------------------------
-- Is this move a discovered check against the opposing player? By 'discovered' we mean the check
-- is caused by a piece other than the one being moved
--   * castling moves are ignored here, all possible checks initiated by castling moves are handled
--     by looking at the movements of the rook in 'moveIsDirectCheck'
--   * EnPassant moves can contribute two possible discovered check paths.
----------------------------------------------------------------------------------------------------
moveIsDiscoveredCheck :: ChessGrid -> ChessMove -> Color -> Int -> Bool
moveIsDiscoveredCheck curGrid mv movingSideColor enemyKingLoc =
    let newGrid = applyMoveToGrid curGrid mv
        kingColor = enemyColor movingSideColor
        dirsFromKing = discoveredCheckDirs (unGrid newGrid) enemyKingLoc mv
        anyChecks = F.foldl' (\acc d -> acc || kingHasEnemy newGrid enemyKingLoc kingColor d )
                             False dirsFromKing
    in anyChecks

----------------------------------------------------------------------------------------------------
-- Is this an (illegal) move that exposes the moving side's king 'directly'? By direclty, we mean
-- the king has moved into a square attacked by the opposing side
--   * only king moves considered here
--   * valid castling moves cannot cause this situation
--   * This is one of the few places we need to do the full scan to see what pieces can attack the king
----------------------------------------------------------------------------------------------------
moveExposesKingDirect :: ChessGrid -> ChessMove -> Color -> Bool
moveExposesKingDirect grid mv movingSideColor =
    case mv of
      cm@CastlingMove{..} -> False
      ep@EnPassantMove{..} -> False
      m@StdMove{..} ->
         let movingChar = indexToChar grid _startIdx
         in if movingChar /= 'k' && movingChar /= 'K'
           then False
           else
             let newGrid = applyMoveToGrid grid mv
                 newKingLoc = _endIdx
             in isKingInCheckFull newGrid movingSideColor newKingLoc

----------------------------------------------------------------------------------------------------
-- Is this an (illegal) move that exposes the moving side's king in a 'discovered' way? By discovered,
-- we mean that a non-king piece was moved which exposes the king to check
--   * regular king moves not considered here
--   * casling moves not considered here
--   * EnPassant moves contribute two possible discovered check paths.
----------------------------------------------------------------------------------------------------
moveExposesKingDiscovered :: ChessGrid -> ChessMove -> Color -> Int -> Bool
moveExposesKingDiscovered grid mv movingSideColor movingSideKingLoc =
    -- no castling moves, no king moves...
    if not
      (case mv of
        cm@CastlingMove{..}                                        -> False
        m@StdMove{..}  | movingPiece <- indexToChar grid _startIdx
                       , movingPiece == 'k' || movingPiece == 'K'  -> False
                       | otherwise                                 -> True
        ep@EnPassantMove{..}                                       -> True )
            then False
            else
                let newGrid = applyMoveToGrid grid mv
                    dirsFromKing = discoveredCheckDirs (unGrid newGrid) movingSideKingLoc mv
                    anyChecks = F.foldl' (\acc d -> acc ||
                                           kingHasEnemy newGrid movingSideKingLoc movingSideColor d )
                                       False dirsFromKing
                in anyChecks

applyMoveToGrid :: ChessGrid -> ChessMove -> ChessGrid
applyMoveToGrid curGrid mv =
    let (newGrid, _mvStartIdx, _mvEndIdx) = case mv of
            m@StdMove{..} -> (movePiece' curGrid m _startIdx _endIdx, _startIdx, _endIdx)
            cm@CastlingMove{..} -> (castle' curGrid cm, _kingStartIdx, _kingEndIdx)
            ep@EnPassantMove{..} -> (movePiece' curGrid ep _epStartIdx _epEndIdx, _epStartIdx, _epEndIdx)
    in newGrid

----------------------------------------------------------------------------------------------------
-- This is only used when a position is created without applying a move (e.g., from a FEN)
-- Only the moving side can be in check if the position is valid
-- This should only be used when necessary as it involves a more expensive search
----------------------------------------------------------------------------------------------------
loadedPositionIsCheck :: ChessPos -> Bool
loadedPositionIsCheck pos =
  let kingColor = flipPieceColor $ _cpsColorToMove $ _cpState pos
      kingLoc = colorToTupleElem kingColor (_cpKingLoc pos)
      g = _cpGrid pos
  in isKingInCheckFull g kingColor kingLoc

containsKingCapture :: ChessGrid -> [ChessMove] -> Bool
containsKingCapture grid mvs =
  foldr (foldf grid) False mvs
  where
    foldf g mv acc = acc ||
        case mv of
            m@StdMove{..} ->
                case _exchange of
                    Nothing -> False
                    Just c ->  c == 'k' || c == 'K'
            cm@CastlingMove{..} -> False
            ep@EnPassantMove{..} ->
                let c = indexToChar g _epRemoveIdx
                in c == 'k' || c == 'K'

----------------------------------------------------------------------------------------------------
-- finds the rectangular or diagonal direction for a discovered check, if it exists
-- returns the direction -- this is used to detect the usual 'discovered check' plus the similar moves
-- where a player leaves his own king in check by the same 'discovered' mechanism
----------------------------------------------------------------------------------------------------
discoveredCheckDirs :: Vector Char -> Int -> ChessMove -> [Dir]
discoveredCheckDirs _g _k CastlingMove{..} = []
discoveredCheckDirs g k m@StdMove{..} =
  case findDirFromKing k _startIdx of
    Nothing -> []
    Just d -> [d]
discoveredCheckDirs g k ep@EnPassantMove{..} = --enPassant creates two possible discovererd checks
    let results = [findDirFromKing k _epStartIdx, findDirFromKing k _epRemoveIdx]
    in foldr foldf [] results
  where
    foldf Nothing acc = acc
    foldf (Just d) acc = d : acc

kingHasEnemy :: ChessGrid -> Int -> Color -> Dir -> Bool
kingHasEnemy g kingIdx kingColor dir =
    case dirAttackMeLoc g (kingIdx, ' ', kingColor) (toDirection dir) of
      Nothing -> False
      Just _  -> True

----------------------------------------------------------------------------------------------------
-- find the direction from the 'from' the king to the 'to' piece
-- returns 'Just d' if 'd' is one of the rectangular or diagonal directions (aka, one of the 'queenDirs')
-- otherwise returns 'Nothing'
----------------------------------------------------------------------------------------------------
findDirFromKing :: Int -> Int -> Maybe Dir
findDirFromKing k to =
  case (to - k, abs (to - k)) of
      (x, xAbs) | xAbs <= 8
                , x > 0         -> Just right
                | xAbs <= 8
                , x < 0         -> Just left
                | xAbs `rem` 10 == 0
                , x > 0         -> Just up
                | xAbs `rem` 10 == 0
                , x < 0         -> Just down
                | xAbs `rem` 9 == 0
                , x > 0         -> Just diagUL
                | xAbs `rem` 9 == 0
                , x < 0         -> Just diagDR
                | xAbs `rem` 11 == 0
                , x > 0         -> Just diagUR
                | xAbs `rem` 11 == 0
                , x < 0         -> Just diagDL
                | otherwise     -> Nothing

---------------------------------------------------------------------------------------------------
-- Is the king in the given position in check?
---------------------------------------------------------------------------------------------------
isKingInCheckFull :: ChessGrid -> Color -> Int -> Bool
isKingInCheckFull g kingColor kingLoc =
   let ch = if kingColor == White then 'K' else 'k'
       bLocs = multiMoveCaptureLocs bishopDirs g (kingLoc, ch, kingColor)
       rLocs = multiMoveCaptureLocs rookDirs g (kingLoc, ch, kingColor)
       nLocs = singleMoveCaptureLocs knightDirs g (kingLoc, ch, kingColor)
       kLocs = singleMoveCaptureLocs kingDirs g (kingLoc, ch, kingColor)
       pLocs = if kingColor == White
         then singleMoveCaptureLocs whitePawnCaptureDirs g (kingLoc, ch, kingColor)
         else singleMoveCaptureLocs blackPawnCaptureDirs g (kingLoc, ch, kingColor)
       ec = flipPieceColor kingColor
   in matchesChar g (pieceToChar Bishop ec) bLocs
       || matchesChar g (pieceToChar Rook ec) rLocs
       || matchesChar g (pieceToChar Knight ec) nLocs
       || matchesChar g (pieceToChar King ec) kLocs
       || matchesChar g (pieceToChar Pawn ec) pLocs
       || matchesChar g (pieceToChar Queen ec) (bLocs ++ rLocs)

matchesChar :: ChessGrid -> Char -> [Int] -> Bool
matchesChar g ch xs = foldr f False xs
  where
    f x r = indexToChar g x == ch || r

---------------------------------------------------------------------------------------------------
checkQueen
  :: ChessGrid
  -> Bool
  -> ChessMove
  -> Bool
checkQueen _ True _ = True
checkQueen g False mv =
    case mv of
        StdMove _isExch start _end _s ->
            let ch = toUpper $ indexToChar g start
            in case ch of
                'Q' -> True
                _   -> False
        CastlingMove{}  -> False

isCritical :: ChessNode -> Bool
isCritical cn =
    let isAnExchange =
          case _chessMv cn of
              StdMove{..} -> case _exchange of
                Just _ -> True
                Nothing -> False
              _ -> False
        pos = cn ^. chessPos
        clr = pos ^. (cpState . cpsColorToMove)
    {- this turns out to be a bad idea...
         isInCheck = colorToTupleElem clr (pos ^. cpInCheck)
    in isAnExchange || isInCheck -}

    in isAnExchange

---------------------------------------------------------------------------------------------------
flipPieceColor :: Color -> Color
flipPieceColor White = Black
flipPieceColor Black = White

---------------------------------------------------------------------------------------------------
-- Calculate the 'development' score for the position (White - Black)
--------------------------------------------------------------------------------------------------
calcDevelopment :: ChessPos -> Float
calcDevelopment cp = whiteDev cp - blackDev cp

whiteDev :: ChessPos -> Float
whiteDev cp =
  let (wKns, bKns) = filterLocsByChar (locsForColor cp) 'N'
      wKnLocs = map (\(loc, _, _) -> loc) wKns
      wKnTotal = calcPiecePositionScore wKnightInitialLocAdjustments wKnLocs


      (wBs, bBs) = filterLocsByChar (locsForColor cp) 'B'
      wBLocs = map (\(loc, _, _) -> loc) wBs
      wBsTotal = calcPiecePositionScore wBishopInitialLocAdjustments wBLocs
   in (4 - (wKnTotal + wBsTotal))

blackDev :: ChessPos -> Float
blackDev cp =
  let (wKns, bKns) = filterLocsByChar (locsForColor cp) 'N'
      bKnLocs = map (\(loc, _, _) -> loc) bKns
      bKnTotal = calcPiecePositionScore bKnightInitialLocAdjustments bKnLocs

      (wBs, bBs) = filterLocsByChar (locsForColor cp) 'B'
      bBLocs = map (\(loc, _, _) -> loc) bBs
      bBsTotal = calcPiecePositionScore bBishopInitialLocAdjustments bBLocs
  in  (4 - (bKnTotal + bBsTotal))

wKnightInitialLocAdjustments :: Map Int Float
wKnightInitialLocAdjustments = M.fromList [(12, 1.0), (17, 1.0)]

bKnightInitialLocAdjustments :: Map Int Float
bKnightInitialLocAdjustments = M.fromList [(82, 1.0), (87, 1.0)]

wBishopInitialLocAdjustments :: Map Int Float
wBishopInitialLocAdjustments = M.fromList [(13, 1.0), (16, 1.0)]

bBishopInitialLocAdjustments :: Map Int Float
bBishopInitialLocAdjustments = M.fromList [(83, 1.0), (86, 1.0)]

---------------------------------------------------------------------------------------------------
-- Calculate a penalty for developing the queen prematurely (White - Black)
--------------------------------------------------------------------------------------------------
calcEarlyQueen :: ChessPos -> Float
calcEarlyQueen cp =
    let locs = locsForColor cp
        (wqs, bqs) = filterLocsByChar locs 'Q'
        wqEarly
            -- no queen, or multiple queen endgame, dont bother with this
           | length wqs /= 1       =  0.0
           | fst3 (head wqs) /= wQ = whiteDev cp - 4.0
           | otherwise             = 0.0
        bqEarly
           | length bqs /= 1       = 0.0
           | fst3 (head bqs) /= bQ = blackDev cp - 4.0
           | otherwise             = 0.0
    in wqEarly - bqEarly

---------------------------------------------------------------------------------------------------
-- Calculate a score for pawn positioning
--------------------------------------------------------------------------------------------------
calcPawnPositionScore :: ChessPos -> Float
calcPawnPositionScore cp =
    let g = cp ^. cpGrid
        (wLocs, bLocs) = locsForColor cp
        filterF loc =
            case toUpper $ indexToChar g loc of
                'P' -> True
                _   -> False
        wPawns = filter filterF (fst3 <$> wLocs)
        bPawns = filter filterF (fst3 <$> bLocs)
        centerScore = calcCenterPawnScore wPawns bPawns
        npScore = calcKnightPawnScore g wPawns bPawns
        rpScore = calcRookPawnScore g wPawns bPawns
        bpScore = calcBishopPawnScore wPawns bPawns
    in centerScore + npScore + rpScore + bpScore

invertGrid :: ChessGrid -> ChessGrid
invertGrid g =
  let rows = asRows g
  -- asRows has already reversed the rows...
  in ChessGrid $ V.concat rows

-- black pawns are reflected to the white side of the board to simplify lookups
-- e.g. a black pawn at 71 is represented as 21, 61 as 31, etc.
invertY :: Int -> Int
invertY n = 10 * (9 - (n `div` 10)) + n `mod` 10

asRows :: ChessGrid -> [Vector Char]
asRows g =
  let v = unGrid g
  in V.ifoldl' foldf [] v
  where
    foldf [] _ x = [V.singleton x]
    foldf acc@(a:acc') n x =
      case n of
      n' | n' `div` 10 /= ((n' - 1) `div` 10) -> V.singleton x : acc
         | otherwise -> (a `V.snoc` x) : acc'

calcCenterPawnScore :: [Int] -> [Int] -> Float
calcCenterPawnScore wPawns bPawns =
    let w = calcPawnAdjustments kpQpAdjustments wPawns
        b = calcPawnAdjustments kpQpAdjustments (invertY <$> bPawns)
    in w - b

calcKnightPawnScore :: ChessGrid -> [Int] -> [Int] -> Float
calcKnightPawnScore grid wPawns bPawns =
    let prelimW = calcPawnAdjustments knpQnpAdjustments wPawns
        prelimB = calcPawnAdjustments knpQnpAdjustments (invertY <$> bPawns)
        w = calcNP White grid wPawns
        b = calcNP Black grid bPawns
    in prelimW + w - (prelimB + b)
  where
    calcNP clr g pawns =
          -- remove penalty for pawn moves that setup fiancetto
        let kSideAdj = if knpSpaceForFianchetto clr g && kBishopAtHome clr g
              then pn3FiancettoValue
              else 0
            qSideAdj = if qnpSpaceForFianchetto clr g && qBishopAtHome clr g
              then pn3FiancettoValue
              else 0
        in kSideAdj + qSideAdj

calcRookPawnScore :: ChessGrid -> [Int] -> [Int] -> Float
calcRookPawnScore grid wPawns bPawns =
    let prelimW = calcPawnAdjustments krpQrpAdjustments wPawns
        prelimB = calcPawnAdjustments krpQrpAdjustments (invertY <$> bPawns)
        w = calcRP White grid wPawns
        b = calcRP Black grid bPawns
    in prelimW + w - (prelimB + b)
  where
    calcRP clr g pawns =
        -- add penalty for p-r4 pawn moves that do not open a space for rook
        let kSidePenR4 = if pawnAt clr g wKR 4 && not (krpSpaceForRook clr g && kRookAtHome clr g)
              then pR4Penalty
              else 0
            qSidePenR4 = if pawnAt clr g wQR 4 && not (qrpSpaceForRook clr g && qRookAtHome clr g)
              then pR4Penalty
              else 0
            -- add penalty for p-r3 moves that expose the king
            kSidePenR3 = if pawnAt clr g wKR 3 && kNearKrp clr g
              then pR3Penalty
              else 0
            qSidePenR3 = if pawnAt clr g wQR 3 && kNearQrp clr g
              then pR3Penalty
              else 0
        in kSidePenR4 + qSidePenR4 + kSidePenR3 + qSidePenR3

-- row numbers are 1-based here so they are consistent with chess notation
-- e.g. 'pawnAt White grid wKR 4' corresponds to the square KR4 in chess notation
pawnAt :: Color -> ChessGrid -> Int -> Int -> Bool
pawnAt White grid columnBase row =
    let idx = columnBase + (row - 1) * 10
    in case indexToChar grid idx of
         'P' -> True
         _   -> False
pawnAt Black grid columnBase row =
    let idx = columnBase + (7 - (row - 1)) * 10
    in case indexToChar grid idx of
         'p' -> True
         _   -> False

calcBishopPawnScore :: [Int] -> [Int] -> Float
calcBishopPawnScore wPawns bPawns =
    let wp = calcPawnAdjustments kbpQbpAdjustments wPawns
        bp = calcPawnAdjustments kbpQbpAdjustments (invertY <$> bPawns)
    in wp - bp

calcPawnAdjustments :: Map Int Float -> [Int] -> Float
calcPawnAdjustments m pawns =
    foldr foldF 0 pawns
    where
      foldF xLoc r = M.findWithDefault 0 xLoc m + r

kRookAtHome :: Color -> ChessGrid -> Bool
kRookAtHome White grid = case indexToChar grid wKR of
                          'R' -> True
                          _   -> False
kRookAtHome Black grid = case indexToChar grid bKR of
                          'r' -> True
                          _   -> False

kNearKrp :: Color -> ChessGrid -> Bool
kNearKrp White grid =
  case indexToChar grid wKN of
    'K' -> True
    _   ->
        case indexToChar grid wKR of
            'K' -> True
            _   -> False
kNearKrp Black grid =
  case indexToChar grid bKN of
    'k' -> True
    _   ->
        case indexToChar grid bKR of
            'k' -> True
            _   -> False

kNearQrp :: Color -> ChessGrid -> Bool
kNearQrp White grid =
  case indexToChar grid wQN of
    'K' -> True
    _   ->
        case indexToChar grid wQR of
            'K' -> True
            _   -> False
kNearQrp Black grid =
  case indexToChar grid bQN of
    'k' -> True
    _   ->
        case indexToChar grid bQR of
            'k' -> True
            _   -> False

qRookAtHome :: Color -> ChessGrid -> Bool
qRookAtHome White grid = case indexToChar grid wQR of
                          'R' -> True
                          _   -> False
qRookAtHome Black grid = case indexToChar grid bQR of
                          'r' -> True
                          _   -> False

krpSpaceForRook :: Color -> ChessGrid -> Bool
krpSpaceForRook White grid = case indexToChar grid 48 of
                          'P' -> True
                          _   -> False
krpSpaceForRook Black grid = case indexToChar grid 58 of
                          'p' -> True
                          _   -> False

qrpSpaceForRook :: Color -> ChessGrid -> Bool
qrpSpaceForRook White grid = case indexToChar grid 41 of
                          'P' -> True
                          _   -> False
qrpSpaceForRook Black grid = case indexToChar grid 51 of
                          'p' -> True
                          _   -> False

kBishopAtHome :: Color -> ChessGrid -> Bool
kBishopAtHome White grid = case indexToChar grid wKB of
                        'B' -> True
                        _   -> False
kBishopAtHome Black grid = case indexToChar grid bQB of
                        'b' -> True
                        _   -> False
qBishopAtHome :: Color -> ChessGrid -> Bool
qBishopAtHome White grid = case indexToChar grid wQB of
                        'B' -> True
                        _   -> False
qBishopAtHome Black grid = case indexToChar grid bQB of
                        'b' -> True
                        _   -> False

knpSpaceForFianchetto :: Color -> ChessGrid -> Bool
knpSpaceForFianchetto White grid = case indexToChar grid 37 of
                          'P' -> True
                          _ -> False
knpSpaceForFianchetto Black grid = case indexToChar grid 67 of
                          'p' -> True
                          _ -> False

qnpSpaceForFianchetto :: Color -> ChessGrid -> Bool
qnpSpaceForFianchetto White grid = case indexToChar grid 32 of
                          'P' -> True
                          _ -> False
qnpSpaceForFianchetto Black grid = case indexToChar grid 62 of
                          'p' -> True
                          _ -> False

kpQpAdjustments :: Map Int Float
kpQpAdjustments = M.fromList
    [ (24, 0.0), (34, 3.0), (44, 9.0), (25, 0.0), (35, 3.0), (45, 9.0) ]

pn3FiancettoValue :: Float
pn3FiancettoValue = 3 -- when used, added to pawn at N3's value

knpQnpAdjustments :: Map Int Float
knpQnpAdjustments = M.fromList
    [ (22, 0.0), (32, -1), (42, -2.0), (27, 0.0), (37, -1), (47, -2.0) ]

pR3Penalty :: Float
pR3Penalty = -3 -- when used, subtracted from pawn at R3's value

pR4Penalty :: Float
pR4Penalty = -3 -- when used, subtracted to pawn at R4's value

krpQrpAdjustments :: Map Int Float
krpQrpAdjustments = M.fromList
    [ (21, 0.0), (31, 2.0), (41, 2), (28, 0.0), (38, 2.0), (48, 2) ]

kbpQbpAdjustments :: Map Int Float
kbpQbpAdjustments = M.fromList
    [ (23, 0.0), (33, -1.0), (43, -2.0), (26, 0.0), (36, -1.0), (46, -2.0) ]

---------------------------------------------------------------------------------------------------
-- Calculate a score for Connected rooks
--------------------------------------------------------------------------------------------------
calcConnectedRookScore :: ChessPos -> Float
calcConnectedRookScore cp =
  let wScore = if connectedRooks cp White
        then 1.0
        else 0.0
      bScore = if connectedRooks cp Black
        then 1.0
        else 0.0
  in wScore - bScore


---------------------------------------------------------------------------------------------------
-- Calculate a score for rooks on open or half-open files
--------------------------------------------------------------------------------------------------
calcRookOpenFileScore :: ChessPos -> Float
calcRookOpenFileScore cp =
    let (wRs, bRs) = filterLocsByChar (locsForColor cp) 'R'
        wScore = rooksOpenFileScore cp wRs
        bScore = rooksOpenFileScore cp bRs
    in wScore - bScore

rooksOpenFileScore :: ChessPos -> [(Int, Char, Color)] -> Float
rooksOpenFileScore cp rLocs =
    let scores = fmap (rookOpenFileScore cp) rLocs
    in sum scores

rookOpenFileScore :: ChessPos -> (Int, Char, Color) -> Float
rookOpenFileScore cp (loc, _ch, clr) =
  case rookFileStatus cp loc clr  of
    Open -> 1.0
    HalfOpen -> 0.5
    NotOpen -> 0.0

-- TODO: implement black/white switching sides (re use of 'up'/'down')
rookFileStatus :: ChessPos -> Int -> Color -> RookFileStatus
rookFileStatus cp idx0 clr0 =
  let dir = case clr0 of
              White -> up
              Black -> down
      g = _cpGrid cp
      loop idx =
        let ch = toUpper $ indexToChar g idx
            clr = indexToColor2 g idx
        in case ch of
            'P' ->
                if clr == Just clr0
                    then NotOpen
                    else HalfOpen
            _ ->
              if not (onBoard idx)
                  then Open
              else
                  loop (dir idx)
  in loop (dir idx0)

---------------------------------------------------------------------------------------------------
-- Checks each sides castling status, then adds checking for the necessary empty squares
-- between king and rook to determine which castling moves are available
--------------------------------------------------------------------------------------------------
castlingAvailable :: ChessPos -> Color -> Castling
castlingAvailable pos c =
    let z1 = pos ^. cpState
        (wStatus, bStatus) = z1 ^. cpsCastling
        (castling, kPos) = case c of -- color to move next
            White -> (wStatus, wK)
            _ -> (bStatus, bK)
        kSideOpen = allEmpty pos [kPos + 1, kPos + 2]
        qSideOpen = allEmpty pos [kPos - 1, kPos - 2, kPos - 3]
    in case castling of
            KingSideOnlyAvailable ->
                if kSideOpen then KingSideOnlyAvailable else Unavailable
            QueenSideOnlyAvailable ->
                if qSideOpen then QueenSideOnlyAvailable else Unavailable
            BothAvailable | kSideOpen
                          , qSideOpen
                            -> BothAvailable
                          | kSideOpen
                            -> KingSideOnlyAvailable
                          | qSideOpen
                            -> QueenSideOnlyAvailable
                          | otherwise
                            -> Unavailable
            Castled -> Castled
            Unavailable -> Unavailable

allEmpty :: ChessPos -> [Int] -> Bool
allEmpty pos xs =
  foldr f True xs where
      f x acc = acc && isEmpty pos x

---------------------------------------------------------------------------------------------------
-- Determines each side's castling status:
-- Castled, KingSideOnlyAvailable, QueenSideOnlyAvailable, BothAvailable, or Unavailable
--------------------------------------------------------------------------------------------------
castlingStatus :: ChessPos -> (Castling, Castling)
castlingStatus pos =
  let (w, b) = _cpKingLoc pos
      (wRks, bRks) = filterLocsByChar (locsForColor pos) 'R'
      wRkLocs = map (\(loc, _, _) -> loc) wRks
      bRkLocs = map (\(loc, _, _) -> loc) bRks
      wStatus =
        if wCastledPosition w wRkLocs then Castled
        else
            let wRkTotal = calcPiecePositionScore wRookCastlingAdjustments wRkLocs
            in totalToCastling White wRkTotal w
      bStatus =
        if bCastledPosition b bRkLocs then Castled
        else
            let bRkTotal = calcPiecePositionScore bRookCastlingAdjustments bRkLocs
            in totalToCastling Black bRkTotal b
  in (wStatus, bStatus)
    where
      wCastledPosition :: Int -> [Int] -> Bool
      wCastledPosition kLoc rLocs =
        (kLoc == 17 && 16 `elem` rLocs)
          || (kLoc == 13 && 14 `elem` rLocs)

      bCastledPosition :: Int -> [Int] -> Bool
      bCastledPosition kLoc rLocs =
        (kLoc == 87 && 86 `elem` rLocs)
           || (kLoc == 83 && 84 `elem` rLocs)

      totalToCastling :: Color -> Float -> Int -> Castling
      totalToCastling c val kLoc =
          case val of
              1.1 | White <- c
                  , 15 <- kLoc
                    -> BothAvailable
                  | Black <- c
                  , 85 <- kLoc
                    -> BothAvailable
              0.1 | White <- c
                  , 15 <- kLoc
                    -> QueenSideOnlyAvailable
                  | Black <- c
                  , 85 <- kLoc
                    -> QueenSideOnlyAvailable
              1.0 | White <- c
                  , 15 <- kLoc
                    -> KingSideOnlyAvailable
                  | Black <- c
                  , 85 <- kLoc
                    -> KingSideOnlyAvailable
              _  -> Unavailable

calcCastling :: ChessPos -> Float
calcCastling pos =
   let (wStatus, bStatus) = pos ^. (cpState . cpsCastling)
   in castlingToAbsVal wStatus - castlingToAbsVal bStatus

---------------------------------------------------------------------------------------------------
-- Move a piece on the board, removing any captured piece.  Returns the updated node
--------------------------------------------------------------------------------------------------
movePiece :: ChessNode -> ChessMove -> Int -> Int -> ChessNode
movePiece node move pFrom pTo =
    let newGrid = movePiece' (node ^. (chessPos . cpGrid)) move pFrom pTo
    in set (chessPos . cpGrid) newGrid node

---------------------------------------------------------------------------------------------------
-- Move a piece on the grid vector, removing any captured piece.  Returns the updated grid
--------------------------------------------------------------------------------------------------
movePiece' :: ChessGrid -> ChessMove -> Int -> Int -> ChessGrid
movePiece' g move pFrom pTo =
    let g' = unGrid g
        pieceChar = g' ^? ix pFrom
    in case pieceChar of
        Nothing -> ChessGrid g'
        Just ch ->
            let z = checkPromote ch pTo
                p = ChessGrid $ set (ix pTo) z g'
            in case move of
              EnPassantMove{..} -> removeEnPassant p move
              _                 -> removePiece' p pFrom

mkCastleMove :: Color -> Castle -> ChessMove
mkCastleMove White KingSide = CastlingMove
  { _castle = KingSide
  , _kingStartIdx = 15
  , _kingEndIdx = 17
  , _rookStartIdx = 18
  , _rookEndIdx = 16
  , _castleNote = "O-O" }
mkCastleMove White QueenSide = CastlingMove
  { _castle = QueenSide
  , _kingStartIdx = 15
  , _kingEndIdx = 13
  , _rookStartIdx = 11
  , _rookEndIdx = 14
  , _castleNote = "O-O-O" }
mkCastleMove _ KingSide = CastlingMove
  { _castle = KingSide
  , _kingStartIdx = 85
  , _kingEndIdx = 87
  , _rookStartIdx = 88
  , _rookEndIdx = 86
  , _castleNote = "O-O" }
mkCastleMove _ QueenSide = CastlingMove
  { _castle = QueenSide
  , _kingStartIdx = 85
  , _kingEndIdx = 83
  , _rookStartIdx = 88
  , _rookEndIdx = 84
  , _castleNote = "O-O-O" }

castle' :: ChessGrid -> ChessMove -> ChessGrid
castle' _g StdMove{} = error "This shouldn't happen..."
castle' g cm@CastlingMove{..} =
  let gTemp = movePiece' g cm _kingStartIdx _kingEndIdx
  in movePiece' gTemp cm _rookStartIdx _rookEndIdx

removePiece' :: ChessGrid -> Int -> ChessGrid
removePiece' g idx =
  let g' = unGrid g
  in ChessGrid $ set (ix idx) empty g'

-- Remove the capturing piece from its start location and also
-- remove the captured pawn from its p4/p6 location
removeEnPassant :: ChessGrid -> ChessMove -> ChessGrid
removeEnPassant g EnPassantMove{..} =
  let g' = unGrid g
      tmp = set (ix _epStartIdx) empty g'
  in ChessGrid $ set (ix _epRemoveIdx) empty tmp
removeEnPassant g _ = error "removeEnPassant called with the wrong move type .... "

-- TODO: return different moves/positions for each possible promotion choice
-- for the moment, just choosing a queen
checkPromote :: Char -> Int -> Char
checkPromote chPiece toLoc =
    let c = asciiToColor chPiece
        b = lastRank toLoc c
    in if b then
         case charToPiece chPiece of
            MkChessPiece White (SomeSing SPawn) -> pieceToChar Queen White
            MkChessPiece Black (SomeSing SPawn) -> pieceToChar Queen Black
            _ -> chPiece
       else chPiece

lastRank :: Int -> Maybe Color -> Bool
lastRank loc c
    | c == Just White
    , loc `div` 10 == 8
      = True
    | c == Just Black
    , loc `div` 10 == 1
      = True
    | otherwise
      = False

--TODO: implement this typeclass function with what is currently in GameRunner.hs
undoChessMove :: ChessNode -> ChessMove -> ChessNode
undoChessMove = undefined

---------------------------------------------------------------------------------------------------
-- check for checkmate / stalemate
--------------------------------------------------------------------------------------------------
checkFinal :: ChessNode -> FinalState
checkFinal cn =
    let cp = _chessPos cn
    in checkFinal' cp

checkFinal' :: ChessPos -> FinalState
checkFinal' cp =
    let c = _cpsColorToMove $ _cpState cp
        inCheckStatus = _cpInCheck cp
        iLose = colorToTupleElem c (BWins, WWins)
        -- ^ e.g. colorToTupleElem White (BWins, WWins) = BWins -- White loses
        mvs = legalMoves' cp
    in if notNull mvs
           then NotFinal
           else if inCheckStatus then iLose else Draw

---------------------------------------------------------------------------------------------------
-- get possible moves from a node
--------------------------------------------------------------------------------------------------
legalMoves :: ChessNode -> [ChessMove]
legalMoves node =
    let pos = node ^. chessPos
    in  legalMoves' pos

legalMoves' :: ChessPos -> [ChessMove]
legalMoves' pos =
    let moves = calcMoveLists pos
        initialList = _cmEmpty moves ++ _cmEnemy moves
    in  filterKingInCheck' pos initialList

filterKingInCheck :: ChessNode -> [ChessMove] -> [ChessMove]
filterKingInCheck node xs =
    let pos = node ^. chessPos
    in filterKingInCheck' pos xs

filterKingInCheck' :: ChessPos -> [ChessMove] -> [ChessMove]
filterKingInCheck' pos xs =
    let g = pos ^. cpGrid
        c = pos ^. (cpState . cpsColorToMove)
        kingLoc = colorToTupleElem c (pos ^. cpKingLoc)
        foldf mv r =
            if moveExposesKing pos mv
              then r
              else mv : r
    in foldr foldf [] xs

---------------------------------------------------------------------------------------------------
-- Determine if there are two connected rooks of a given color
---------------------------------------------------------------------------------------------------
connectedRooks :: ChessPos -> Color -> Bool
connectedRooks cp clr =
  let (wRs, bRs) = filterLocsByChar (locsForColor cp) 'R'
      rookLocs = case clr of
        White -> wRs
        _     -> bRs
  in if length rookLocs < 2
    then False
    else hasConnectedPartner cp rookDirs (head rookLocs)

---------------------------------------------------------------------------------------------------
-- Given one rook, determine if there is a connected partner
---------------------------------------------------------------------------------------------------
hasConnectedPartner :: ChessPos -> [Dir] -> (Int, Char, Color) -> Bool
hasConnectedPartner cp dirs loc@(idx, ch, clr) =
  let g' = _cpGrid cp
  in F.foldl' (f g') False dirs
    where
      f :: ChessGrid -> Bool -> Dir -> Bool
      f g acc x =
        case dirFriendlyPieceLoc g loc x of
            Nothing -> acc
            Just z  ->
              acc ||
              case (indexToChar g z, clr) of
                  ('R', White) -> True
                  ('r', Black) -> True
                  _ -> False

-- r to R, B to b, etc.
flipCharColor :: Char -> Char
flipCharColor ch =
  case asciiToColor ch of
    Just Black -> chr $ ord ch - 32
    Just White -> chr $ ord ch + 32
    Nothing -> ch

allowableKingMoves :: ChessPos -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableKingMoves pos loc =
    let g = pos ^. cpGrid
        castleMvs = castleMoves pos
        (empties, enemies) = allowableSingleMoves kingDirs g loc
    in (castleMvs ++ empties, enemies)

-- similar to allowableKingMoves, but not considering castling moves
allowableKingMovesNC :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableKingMovesNC grid loc = allowableSingleMoves kingDirs grid loc

castleMoves :: ChessPos -> [ChessMove]
castleMoves pos =
  let c = pos ^. (cpState . cpsColorToMove)
      avail = castlingAvailable pos c
  in case avail of
      Unavailable -> []
      Castled -> []
      KingSideOnlyAvailable -> kingSideCastlingMove pos c
      QueenSideOnlyAvailable -> queenSideCastlingMove pos c
      BothAvailable -> kingSideCastlingMove pos c
                    ++ queenSideCastlingMove pos c

kingSideCastlingMove :: ChessPos -> Color -> [ChessMove]
kingSideCastlingMove pos White =
    let g = _cpGrid pos
    in [mkCastleMove White KingSide |
        not (_cpInCheck pos) -- verify moving side not already in check

        -- and move in between orig and dest not a 'check'
        -- Note: Check at dest square is already handled by 'moveExposesKing' filtering
        && not (moveExposesKing pos StdMove {_exchange = Nothing, _startIdx = 15, _endIdx = 16, _stdNote = "" } )
     ]
kingSideCastlingMove pos _ =
    let g = _cpGrid pos
    in [mkCastleMove Black KingSide |
        not (_cpInCheck pos) -- verify moving side not already in check

        -- and move in between orig and dest not a 'check'
        -- Note: Check at dest square is already handled by 'moveExposesKing' filtering
        && not (moveExposesKing pos (StdMove {_exchange = Nothing, _startIdx = 85, _endIdx = 86, _stdNote = "" } ))
     ]

queenSideCastlingMove :: ChessPos -> Color -> [ChessMove]
queenSideCastlingMove pos White =
  let g = _cpGrid pos
  in [mkCastleMove White QueenSide |
        not (_cpInCheck pos) -- verify moving side not already in check

        -- and move in between orig and dest not a 'check'
        -- Note: Check at dest square is already handled by 'moveExposesKing' filtering
        && not (moveExposesKing pos (StdMove {_exchange = Nothing, _startIdx = 15, _endIdx = 14, _stdNote = "" } ))
     ]

queenSideCastlingMove pos _ =
  let g = _cpGrid pos
  in [mkCastleMove Black QueenSide |
        not (_cpInCheck pos) -- verify moving side not already in check

        -- and move in between orig and dest not a 'check'
        -- Note: Check at dest square is already handled by 'moveExposesKing' filtering
        && not (moveExposesKing pos (StdMove {_exchange = Nothing, _startIdx = 15, _endIdx = 16, _stdNote = "" } ))
     ]

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos = isEmptyGrid (_cpGrid pos)

isEmpty' :: ChessNode -> Int -> Bool
isEmpty' node = isEmptyGrid (_cpGrid (_chessPos node))

isEmptyGrid :: ChessGrid -> Int -> Bool
isEmptyGrid g idx =
  let g' = unGrid g
  in (g' V.! idx) == empty
{-# INLINE isEmptyGrid #-}

pairToIndexes :: ([ChessMove], [ChessMove]) -> ([Int], [Int])
pairToIndexes (xs, ys) = ( fmap moveToIndex  xs
                         , fmap moveToIndex ys)

moveToIndex :: ChessMove -> Int
moveToIndex CastlingMove{..} = _kingEndIdx
moveToIndex StdMove{..} = _endIdx

----------------------------------------------------------------------------------------------------
-- Calculate the set of all possible moves for the given position & the position's color
----------------------------------------------------------------------------------------------------
calcMoveLists :: ChessPos -> ChessMoves
calcMoveLists pos =
    let c = _cpsColorToMove (_cpState pos)
        (_empty, enemy) = pieceDestinations pos c
    in ChessMoves
      { _cmEmpty = _empty
      , _cmEnemy = enemy
      , _cmForColor = c }

pieceDestinations :: ChessPos -> Color -> ([ChessMove], [ChessMove])
pieceDestinations pos c =
  let locs = case c of
        White -> fst $ locsForColor pos
        _     -> snd $ locsForColor pos
  in movesFromLocs pos locs

movesFromLocs :: ChessPos -> [(Int, Char, Color)] -> ([ChessMove], [ChessMove])
movesFromLocs pos =
  let f :: (Int, Char, Color) -> ([ChessMove], [ChessMove]) -> ([ChessMove], [ChessMove])
      f loc (r1, r2) =
        let (xs, ys) = movesFromLoc pos loc
        in (xs ++ r1, ys ++ r2)
  in foldr f ([], [])

movesFromLoc :: ChessPos -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
movesFromLoc pos loc@(idx, _, _) =
  let g = _cpGrid pos
      ch = toUpper $ indexToChar g idx
  in case ch of
      'K' -> allowableKingMoves pos loc
      'Q' -> allowableQueenMoves g loc
      'R' -> allowableRookMoves g loc
      'N' -> allowableKnightMoves g loc
      'B' -> allowableBishopMoves g loc
      'P' -> allowablePawnMoves pos loc
      _   -> ([], [])

allowableMultiMoves :: [Dir] -> ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableMultiMoves pieceDirs g loc =
  F.foldl' f ([], []) pieceDirs
    where
      f :: ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f (r, r') x =
        let (freeLocs, captureLocs) = dirLocs g loc x
        in (freeLocs ++ r, captureLocs ++ r')

multiMoveMobility :: [Dir] -> ChessGrid -> (Int, Char, Color) -> Int
multiMoveMobility pieceDirs g loc =
  F.foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsCount g loc x
        in count + r

multiMoveCaptureLocs :: [Dir] -> ChessGrid -> (Int, Char, Color) -> [Int]
multiMoveCaptureLocs pieceDirs g loc =
  F.foldl' f [] pieceDirs
    where
      f :: [Int] -> Dir -> [Int]
      f r x =
        case dirCaptureLoc g loc x of
            Just z  -> z : r
            Nothing -> r

-- find the destination locs for pieces that move one square in a given
-- direction (i.e., King and Knight). See @allowableMultiMoves
allowableSingleMoves :: [Dir] -> ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableSingleMoves pieceDirs g loc =
  F.foldl' f ([], []) pieceDirs
    where
      f :: ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f (r, r') x =
        let (freeLocs, captureLocs) = dirLocsSingle g loc x
        in (freeLocs ++ r, captureLocs ++ r')

singleMoveMobility :: [Dir] -> ChessGrid -> (Int, Char, Color) -> Int
singleMoveMobility pieceDirs g (idx, ch, clr) =
  F.foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsSingleCount g (idx, ch, clr) x
        in count + r

singleMoveCaptureLocs :: [Dir] -> ChessGrid -> (Int, Char, Color) -> [Int]
singleMoveCaptureLocs pieceDirs g loc =
  F.foldl' f [] pieceDirs
    where
      f :: [Int] -> Dir -> [Int]
      f r x =
        case dirCaptureLocSingle g loc x of
            Just z -> z : r
            Nothing -> r

kingMobility :: ChessPos -> (Int, Char, Color) -> Int
kingMobility cp loc =
    let g = cp ^. cpGrid
    in singleMoveMobility queenDirs g loc

-- find the allowable destination locs for a queen.
allowableQueenMoves :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableQueenMoves = allowableMultiMoves queenDirs

queenMobility :: ChessPos -> (Int, Char, Color) -> Int
queenMobility cp loc =
    let g = cp ^. cpGrid
    in multiMoveMobility queenDirs g loc

-- find the allowable destination locs for a rook
allowableRookMoves :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableRookMoves = allowableMultiMoves rookDirs

rookMobility :: ChessGrid -> (Int, Char, Color) -> Int
rookMobility = multiMoveMobility rookDirs

-- find the allowable destination locs for a bishop
allowableBishopMoves :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableBishopMoves = allowableMultiMoves bishopDirs

bishopMobility :: ChessGrid -> (Int, Char, Color) -> Int
bishopMobility = multiMoveMobility bishopDirs

-- find the possible destination locs for a knight
allowableKnightMoves :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableKnightMoves = allowableSingleMoves knightDirs

knightMobility :: ChessGrid -> (Int, Char, Color) -> Int
knightMobility = singleMoveMobility knightDirs

allowablePawnMoves :: ChessPos -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowablePawnMoves pos loc =
   let g = _cpGrid pos
       (_, enemies) = allowablePawnCaptures g loc
       ep_enemies = allowableEnPassant pos loc
   in (allowablePawnNonCaptures g loc, enemies ++ ep_enemies)

allowablePawnMovesNEP :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowablePawnMovesNEP grid loc =
   let (_, enemies) = allowablePawnCaptures grid loc
   in (allowablePawnNonCaptures grid loc, enemies)

-- EnPassant captures are handled in allowableEnPassant, and not included here
allowablePawnCaptures :: ChessGrid -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowablePawnCaptures g loc@(_, _, clr) =
      let dirs = case clr of
              White -> whitePawnCaptureDirs
              Black -> blackPawnCaptureDirs
          (empties, enemies) = allowableSingleMoves dirs g loc
      in (empties, enemies)

-- find the allowable enPassant destination capture locs for a pawn
allowableEnPassant :: ChessPos -> (Int, Char, Color) -> [ChessMove]
allowableEnPassant = enPassantCaptures

enPassantCaptures :: ChessPos -> (Int, Char, Color) -> [ChessMove]
enPassantCaptures pos loc@(idx, _, clr) =
    let g = _cpGrid pos
        state = _cpState pos
        dirs = case clr of
           White -> whitePawnEnPassantDirs
           Black -> blackPawnEnPassantDirs
        empties = fst $ allowableSingleMoves dirs g loc
    in case _cpsEnPassant state of
        Just ep ->
            let filtered = filter (\m -> _endIdx m == ep) empties
            in stdMovesToEnPassant g filtered
        Nothing -> []

-- asssumed: the the input StdMove(s) represents enPassant captures
stdMovesToEnPassant :: ChessGrid -> [ChessMove] -> [ChessMove]
stdMovesToEnPassant g mvs =
    map f mvs
  where
    f m@StdMove{..} =
      EnPassantMove
      { _epStartIdx = _startIdx
      , _epEndIdx = _endIdx
      , _epRemoveIdx = epRemoveLoc _startIdx _endIdx
      , _epNote = _stdNote }

-- find the allowable destination locs for a pawn (non-capturing moves)
allowablePawnNonCaptures :: ChessGrid -> (Int, Char, Color) -> [ChessMove]
allowablePawnNonCaptures g loc@(_, _, clr) =
    let hasMoved = hasPawnMoved loc
    in case clr of
        White -> pawnMoves g whitePawnDir hasMoved loc
        Black -> pawnMoves g blackPawnDir hasMoved loc

pawnMoves :: ChessGrid -> Dir -> Bool -> (Int, Char, Color) -> [ChessMove]
pawnMoves g dir hasMoved loc@(_, ch, clr) =
    case dirLocsSingle g loc dir of
        ([], _) -> []
        (firstMove:_, _) ->  -- only take the 'empty' square
            let twoSpacer =
                  if hasMoved then []
                  else
                    -- get the one square moves starting from the end loc of the first
                    -- and combine with fistMove to make a 2 square pawn move
                    fmap f (fst $ dirLocsSingle g (_endIdx firstMove, ch, clr) dir)
                    where f m = m {_startIdx = _startIdx firstMove}
            in firstMove : twoSpacer

hasPawnMoved :: (Int, Char, Color) -> Bool
hasPawnMoved (idx, _, White) = idx > 28
hasPawnMoved (idx, _, Black) = idx < 71

isTwoSquarePawnMove :: ChessGrid -> ChessMove -> Bool
isTwoSquarePawnMove g StdMove{..} =
    let isPawn =
          case toUpper $ indexToChar g _startIdx of
            'P' -> True
            _   -> False
    in isPawn && abs (_endIdx - _startIdx) == 20
isTwoSquarePawnMove _g _ = False

-- the enpassant 'capture' square for a pawn that has moved two squares
enPassantCaptureLoc :: ChessGrid -> ChessMove -> Maybe Int
enPassantCaptureLoc g mv@StdMove{..} =
  if isTwoSquarePawnMove g mv
  then Just (_startIdx + (_endIdx - _startIdx) `div` 2)
  else Nothing
enPassantCaptureLoc _ _ = Nothing

data SquareState = Empty | HasFriendly | HasEnemy | OffBoard
   deriving Show

dirLocs :: ChessGrid -> (Int, Char, Color) -> Dir ->([ChessMove], [ChessMove])
dirLocs g (idx0, _, c0) dir =
    loop (dir idx0) ([], [])
      where
        loop idx (empties, enemies) =
            let cx = indexToColor2 g idx
                ob = onBoard idx
                friendly = ob && (cx == Just c0)
                enemy = ob && cx == Just (enemyColor c0)
                (sqState, (newEmpties, newEnemies))
                    | not ob = (OffBoard ,(empties, enemies))
                    | enemy = (HasEnemy, (empties, StdMove (Just (indexToChar g idx)) idx0 idx "": enemies))
                    | friendly = (HasFriendly, (empties, enemies))
                    | otherwise = (Empty, (StdMove Nothing idx0 idx "": empties, enemies))
            in (case sqState of
                Empty -> loop (dir idx) (newEmpties, newEnemies)
                _ -> (newEmpties, newEnemies))

---------------------------------------------------------------------------------------------------
-- find the first enemy piece in a given direction
-- TODO: remove the unused element of the triple
---------------------------------------------------------------------------------------------------
dirCaptureLoc :: ChessGrid -> (Int, Char, Color) -> Dir -> Maybe Int
dirCaptureLoc g (idx0, _, clr0) dir =
   let clr0Enemy = enemyColor clr0
       loop idx =
           let clr = indexToColor2 g idx
           in case clr == Just clr0Enemy of
               True -> Just idx
               False | not (onBoard idx) -> Nothing
                     | isEmptyGrid g idx -> loop (dir idx)
                     | otherwise -> Nothing
   in loop (dir idx0)

---------------------------------------------------------------------------------------------------
-- finds the first enemy piece in a given direction that can attack me
-- back from that direction
---------------------------------------------------------------------------------------------------
dirAttackMeLoc :: ChessGrid -> (Int, Char, Color) -> Direction -> Maybe Int
dirAttackMeLoc g loc@(idx0, _, clr0) direction =
  case dirCaptureLoc g loc (toDir direction) of
      Nothing -> Nothing
      Just n ->
         let ch = indexToChar g n
             dir' = toDir direction
             hitBackDir = flipDirection direction
         in if charHasDirection ch hitBackDir
             then Just n
             else Nothing

---------------------------------------------------------------------------------------------------
-- Find the first friendly piece in a given direction
-- TODO: combine with dirCaptureLoc
---------------------------------------------------------------------------------------------------
dirFriendlyPieceLoc :: ChessGrid -> (Int, Char, Color) -> Dir -> Maybe Int
dirFriendlyPieceLoc g (idx0, _, clr0) dir =
    let loop idx =
          let clr = indexToColor2 g idx
          in case clr == Just clr0 of
               True -> Just idx
               False | not (onBoard idx) -> Nothing
                     | isEmptyGrid g idx -> loop (dir idx)
                     | otherwise -> Nothing
    in loop (dir idx0)

---------------------------------------------------------------------------------------------------
-- Count the number of squares that can be moved in a given direction
---------------------------------------------------------------------------------------------------
dirLocsCount :: ChessGrid -> (Int, Char, Color) -> Dir -> Int
dirLocsCount g (idx, ch, clr0) dir =
    let (enemies, empties) = dirLocs g (idx, ch, clr0) dir
    in length enemies + length empties

---------------------------------------------------------------------------------------------------
-- Same as dirLocs, but for pieces that move only one square in a given "direction"
-- (aka King and Knight) -- some code intentionally duplicated with 'dirLocs'
---------------------------------------------------------------------------------------------------
dirLocsSingle :: ChessGrid -> (Int, Char, Color) -> Dir ->([ChessMove], [ChessMove])
dirLocsSingle g (idx0, _, c0) dir =
    let x = dir idx0
        cx = indexToColor2 g x
        cEnemy0 = enemyColor c0
    in
      if not (onBoard x) then ([], [])
      else if cx == Just cEnemy0 then ([], [StdMove (Just (indexToChar g x)) idx0 x ""])
      else if cx == Just c0 then ([], [])
      else ([StdMove Nothing idx0 x ""],[]) -- empty square

dirLocsSingleCount :: ChessGrid -> (Int, Char, Color) -> Dir -> Int
dirLocsSingleCount g idx dir =
    let (enemies, empties) = dirLocsSingle g idx dir
    in length enemies + length empties

dirCaptureLocSingle :: ChessGrid -> (Int, Char, Color) -> Dir -> Maybe Int
dirCaptureLocSingle g (idx, _, clr) dir =
    let enemyClr = enemyColor clr
    in case dir idx of
        x | not (onBoard x) -> Nothing
          | cx <- indexToColor2 g x
          , cx == Just enemyClr -> Just x
          | otherwise -> Nothing

onBoard :: Int -> Bool
onBoard x
    | x < 10          = False
    | x > 88          = False
    | x `mod` 10 == 0 = False
    | x `mod` 10 == 9 = False
    | otherwise       = True
{-# INLINE onBoard #-}

offBoard :: Int -> Bool
offBoard x = not $ onBoard x

----------------------------------------------------------------------------------------------------
{-
 Convert a list of to/from move indexes to a ChessMove
 Assumptions:
 1)  The list is always of length two
 2)  Any piece found at the destination square of the move must be an opponent's piece being captured.
-}
----------------------------------------------------------------------------------------------------
indexesToMove :: ChessNode -> [Int] -> Either String ChessMove
indexesToMove node [fromLoc, toLoc] =
    case checkIndexesToCastle [fromLoc, toLoc] of
        Just cm -> Right cm
        Nothing ->
            case checkIndexesToEnPassant (node ^. (chessPos . cpGrid )) [fromLoc, toLoc] of
                Just epm -> Right epm
                Nothing ->
                  let g = node ^. (chessPos . cpGrid)
                      ch = indexToChar g toLoc
                      exch = if ch == empty
                        then Nothing
                        else Just ch
                  in Right $ StdMove { _exchange = exch
                                    , _startIdx = fromLoc
                                    , _endIdx = toLoc
                                    , _stdNote = ""}
indexesToMove _ _ = Left "IndexesToMove - expected 2 element list as input, e.g. [E2, E4]"

-- Looking for king moves: 15->17(KS-W), 85->87(KS-B), 15->13(QS-W), or 85->83(QS-B)
-- The param should always be a list of length two
checkIndexesToCastle :: [Int] -> Maybe ChessMove
checkIndexesToCastle [fromLoc, toLoc]
    | fromLoc /= 15
    , fromLoc /= 85 = Nothing
    | toLoc == fromLoc + 2  = Just $ CastlingMove
        { _castle = KingSide
        , _kingStartIdx = fromLoc
        , _kingEndIdx = toLoc
        , _rookStartIdx = fromLoc + 3
        , _rookEndIdx = toLoc - 1
        , _castleNote = "O-O" }
    | toLoc == fromLoc - 2  = Just $ CastlingMove
        { _castle = QueenSide
        , _kingStartIdx = fromLoc
        , _kingEndIdx = toLoc
        , _rookStartIdx = fromLoc - 4
        , _rookEndIdx = toLoc + 1
        , _castleNote = "O-O-O" }
    | otherwise = Nothing
checkIndexesToCastle z = error $ "checkIndexesToCastle called with a list not of length 2: " ++ show z

-- Given the start & end locations for an EnPassant capture,
-- calculate the square containing the enemy pawn to be removed
epRemoveLoc :: Int -> Int -> Int
epRemoveLoc startIdx endIdx =
  let black = endIdx < startIdx
      captureLoc =
        if black then endIdx + 10
        else endIdx - 10
  in captureLoc

-- Looking for a diagonal pawn move to an empty square
-- The param should always be a list of length two
checkIndexesToEnPassant :: ChessGrid -> [Int] -> Maybe ChessMove
checkIndexesToEnPassant g [fromLoc, toLoc] =
  case toUpper $ indexToChar g fromLoc of
    'P' ->
        let remEleven = (toLoc - fromLoc) `rem` 11 == 0
            remNine = (toLoc - fromLoc) `rem` 9 == 0
        in if (remEleven || remNine) && (indexToChar g toLoc == empty)
          then
            let captureLoc = epRemoveLoc fromLoc toLoc
            in Just $ EnPassantMove
              { _epStartIdx = fromLoc
              , _epEndIdx = toLoc
              , _epRemoveIdx = captureLoc
              , _epNote = "" }
          else
            Nothing
    _ -> Nothing

---------------------------------------------------------------------------------------------------
-- starting locs...
---------------------------------------------------------------------------------------------------
wK, wQ, wKB, wKN, wKR, wQB, wQN, wQR ,wKP, wQP :: Int
wK = 15
wQ = 14
wKB = 16
wKN = 17
wKR = 18
wQB = 13
wQN = 12
wQR = 11
wKP = 25
wQP = 24

wDevPieces :: Set Int
wDevPieces = S.fromList [wKB, wKN, wQB, wQN]

wCenterPawns :: Set Int
wCenterPawns = S.fromList [wKP, wQP]

wCastlingPieces :: Set Int
wCastlingPieces = S.fromList [wK, wKR, wQR]

bK, bQ, bKB, bKN, bKR, bQB, bQN, bQR, bKP, bQP :: Int
bK = 85
bQ = 84
bKB = 86
bKN = 87
bKR = 88
bQB = 83
bQN = 82
bQR = 81
bKP = 75
bQP = 74

bDevPieces :: Set Int
bDevPieces = S.fromList [bKB, bKN, bQB, bQN]

bCenterPawns :: Set Int
bCenterPawns = S.fromList [bKP, bQP]

bCastlingPieces :: Set Int
bCastlingPieces = S.fromList [bK, bKR, bQR]

---------------------------------------------------------------------------------------------------
-- parse string input to move or command
---------------------------------------------------------------------------------------------------
parseChessEntry n s
    | s == ""             = Left "No input"
    | Left err <- pMove   = Left err
    | Right x  <- pMove   = parserToChessEntry n x
        where
        pMove = Parser.run s

---------------------------------------------------------------------------------------------------
-- Convert Parser's Entry type to ChessEntry
---------------------------------------------------------------------------------------------------
parserToChessEntry :: ChessNode -> Parser.Entry -> Either String (Entry ChessMove s)
parserToChessEntry node (Parser.Move xs)
    | length xs == 2 =
        case indexesToMove node $ fmap parserLocToInt xs of
            Left s -> Left s
            Right r -> Right (MoveEntry r)
    | otherwise      = Left "parserToChessEntry - expected 2 element list as input, e.g. [E2, E4]"
parserToChessEntry _ (Parser.Cmd s) = Right $ CmdEntry s

parserLocToInt :: Parser.Loc -> Int  -- parser ensures valid key
parserLocToInt (Parser.Loc c row) =
    let col = ord (toUpper c) - 64  -- 1 based index
    in row * 10 + col

indexRange :: Vector Int
indexRange = V.fromList [0..100] :: Vector Int

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   P   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
-- Some special starting positions for evaluating certain features....
-- TODO: Eventually delete these or move to the ChessTest module
----------------------------------------------------------------------------------------------------
alphaBetaBoard :: ChessGrid
alphaBetaBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  'K',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'k',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   k   -   p   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   P   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   K   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

alphaBetaBoard2 :: ChessGrid
alphaBetaBoard2 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'K',  '+',
                             '+',  'Q',  'B',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'b',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'r',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   k   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   r   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   b   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   -   -   P          3| (30)  31   32   33   34   35   36   37   38  (39)
Q   B   -   -   -   -   P   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   K          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

discoveredCheckTree :: (Tree ChessNode, ChessPosState)
discoveredCheckTree =
    case getNodeFromFen "3r4/8/3k4/2b1R3/8/6B1/8/3K4 w - - 0 0" of
      Left err -> error "discoveredCheckTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   -   r   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   k   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   b   -   R   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   B   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   K   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-- Moves to verify:
-- (W) E5xC5 (discovered) check
-- (b) D6xC5 (discovered) check
-}

--used in tests:
discoveredCheckNode :: ChessNode
discoveredCheckNode = rootLabel $ fst discoveredCheckTree

checkMateExampleBoard :: ChessGrid
checkMateExampleBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  'B',  ' ',  'K',  ' ',  ' ',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'q',  ' ',  'P',  ' ',  ' ',  'Q',  ' ',  '+',
                             '+',  ' ',  'B',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'n',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'b',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  ' ',  ' ',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
r   -   -   -   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   b   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   B   -   -   N   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   q   -   P   -   -   Q   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   -   K   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Move to verify
(W) G4-D7 mate
-}

checkMateExampleBoard2 :: ChessGrid
checkMateExampleBoard2 = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                      '+',  'R',  ' ',  'B',  'Q',  'K',  ' ',  ' ',  'R',  '+',
                                      '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                                      '+',  ' ',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  '+',
                                      '+',  ' ',  'q',  ' ',  'P',  ' ',  ' ',  'b',  ' ',  '+',
                                      '+',  ' ',  'B',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  '+',
                                      '+',  ' ',  ' ',  'n',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                      '+',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  'p',  '+',
                                      '+',  'r',  ' ',  ' ',  ' ',  'k',  'b',  'n',  'r',  '+',
                                      '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   B   -   -   N   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   q   -   P   -   -   b   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   Q   K   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d2 -c6):
(W) D1xG4
(b) Avoid G4-D7 mate ... the only non-losing replies are B4-D6, G1-F3 and E7-E6
-}




checkMateExampleBoard3 :: ChessGrid
checkMateExampleBoard3 = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                      '+',  'R',  ' ',  ' ',  'Q',  'K',  'B',  'N',  'R',  '+',
                                      '+',  'P',  ' ',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                                      '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  'B',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  'p',  ' ',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  'p',  '+',
                                      '+',  'p',  'p',  'p',  'p',  ' ',  'p',  ' ',  ' ',  '+',
                                      '+',  'r',  'n',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                                      '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   p          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   P   -   p   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   P   -   -   B          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d3):
(W) E5xF6
(b) Avoid G5xH4 leads to mate
-}

checkMateExampleBoard4 :: ChessGrid
checkMateExampleBoard4 = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                      '+',  'R',  ' ',  ' ',  ' ',  'K',  'B',  'N',  'R',  '+',
                                      '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                                      '+',  ' ',  ' ',  'Q',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                                      '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                                      '+',  'p',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                                      '+',  'r',  'n',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                                      '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   Q   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   -   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d4):
(W) E4-E5
(b) Chosen move of G8-H8 is terrible.
    The only saving move is (maybe) F6-E4
-}

mateInTwoTree01 :: (Tree ChessNode, ChessPosState)
mateInTwoTree01 =
    case getNodeFromFen "r7/8/8/8/8/1p6/PP6/KBk5 b - - 0 1" of
      Left err -> error "mateInTwoTree01 invalid FEN?" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   p   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
K   B   k   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d4),
mate in 2: (b) A8-A3
-}
mateInTwo01TestData :: StdMoveTestData
mateInTwo01TestData = StdMoveTestData
    { smtdBoardName = "mateInTwo01"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 81
    , smtdEndIdx = 31 }

mateInTwoBoard02:: ChessGrid
mateInTwoBoard02 = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  'K',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  'b',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   b   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   p   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
K   -   k   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d4),
mate in 2: (b) B4-B3
-}
mateInTwo02TestData :: StdMoveTestData
mateInTwo02TestData = StdMoveTestData
    { smtdBoardName = "mateInTwo02"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 42
    , smtdEndIdx = 32 }

mateInTwoBoard02b:: ChessGrid
mateInTwoBoard02b = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  'K',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  'b',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   b   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   p   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   k   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
K   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Previous position with first move taken by each side
Moves to verify
mate in 1: (b) C5-D4
-}

mateInTwoBoard03:: ChessGrid
mateInTwoBoard03 = ChessGrid $ V.fromList
                              [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  'P',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  '+',
                                '+',  'q',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'P',  ' ',  'K',  'b',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'r',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   r   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   k   -   -   -   -   n   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   P   -   K   b   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   p   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
q   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   -   -   -   P   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d4),
mate in 2: (b) F7-F3, (W)D5-E4 (or others), (b) A3-A8
-}
mateInTwo03TestData :: StdMoveTestData
mateInTwo03TestData = StdMoveTestData
    { smtdBoardName = "mateInTwo03"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 76
    , smtdEndIdx = 36 }

mateInThreeTree01 :: (Tree ChessNode, ChessPosState)
mateInThreeTree01 =
    case getNodeFromFen "5k1K/6pp/3p2rr/3P4/8/8/7P/8 b - - 0 1" of
      Left err -> error "mateInThreeTree01 invalid FEN?" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   -   -   -   k   -   K          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   p   -   -   r   r          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   P   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   _          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d6),
mate in 3: (b)      , H6-H4
               H2-H3, H7-H4
               H8-H7, G6-H6 mate
-}
mateInThree01TestData :: StdMoveTestData
mateInThree01TestData = StdMoveTestData
    { smtdBoardName = "mateInThree01"
    , smtdDepth = 5
    , smtdCritDepth = 6
    , smtdStartIdx = 68
    , smtdEndIdx = 48 }

mateInThreeTree02 :: (Tree ChessNode, ChessPosState)
mateInThreeTree02 =
    case getNodeFromFen "r1b2rk1/pppp1ppp/8/2b1p3/2B1P1nq/2N2N2/PPP2PPP/R1BQR1K1 b - - 0 1" of
      Left err -> error "mateInThreeTree02 invalid FEN?" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
r   -   b   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   b   -   p   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   B   -   P   -   n   q          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   Q   R   -   K   _          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with --depth=4 --critdepth=4)
mate in 3: (b)      , H4xF2
               G1-H1, F2-G1
               E1xG1, G4-F2 mate  (or F3xG1, G4-F2 mate)
Note that at this depth the mate is not 'known' at the choice of the first move...
-}
mateInThree02TestData :: StdMoveTestData
mateInThree02TestData = StdMoveTestData
    { smtdBoardName = "mateInThree02"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 48
    , smtdEndIdx = 26 }

mateInTwoBoard03b:: ChessGrid
mateInTwoBoard03b = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  'P',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  '+',
                                '+',  'q',  ' ',  ' ',  ' ',  ' ',  'r',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  'p',  'K',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'P',  ' ',  ' ',  'b',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   k   -   -   -   -   n   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   P   -   -   b   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   p   K   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
q   -   -   -   -   r   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   -   -   -   P   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Previous position with first move taken by each side
Moves to verify (run with -d4),
mate in 1: (b) A3-A8
-}
mateInTwo03bTestData :: StdMoveTestData
mateInTwo03bTestData = StdMoveTestData
    { smtdBoardName = "mateInTwo03b"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 31
    , smtdEndIdx = 81 }

----------------------------------------------------------------------------------------------------
-- Boards to check pawn promotion
----------------------------------------------------------------------------------------------------
promotionBoard01 :: ChessGrid
promotionBoard01 = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'K',  ' ',  ' ',  '+',
                                '+',  ' ',  'r',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  'R',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'k',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   k   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   R   P   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   r   -   p   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   K   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify
mate in 1: (W) D7-D8
or (b) D1-D1
-}
promotion01TestData :: StdMoveTestData
promotion01TestData = StdMoveTestData
    { smtdBoardName = "promotion01"
    , smtdDepth = 4
    , smtdCritDepth = 4
    , smtdStartIdx = 74
    , smtdEndIdx = 84 }


----------------------------------------------------------------------------------------------------
-- Board to check handling of drawn game
----------------------------------------------------------------------------------------------------
drawnBoard :: ChessGrid
drawnBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'K',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'k',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   k          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   K          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   R   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Moves to verify:
(W) E1-G1 - Draw
or
(W) E1-E8 - White wins
-}

----------------------------------------------------------------------------------------------------
-- Misc additional boards for debugging
----------------------------------------------------------------------------------------------------
debugBoard :: ChessGrid
debugBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  'N',  ' ',  ' ',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'P',  'P',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                             '+',  ' ',  ' ',  'n',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  'b',  'q',  'k',  'b',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -  --   P   P   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   N   -   -   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Moves to verify:
W: F4-F5
B: C8xF5 ?? (for some reason missing white's pawn capture)
-}

debugBoard_2 :: ChessGrid
debugBoard_2 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  ' ',  ' ',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'Q',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                             '+',  ' ',  ' ',  'n',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  'b',  'q',  'k',  'b',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   Q   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   -   -   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Moves to verify:
W: D4-D1
B: F6xE4 ??? i.e. 66 x 45
-}

debugBoard02 :: ChessGrid
debugBoard02 =  ChessGrid $ V.fromList  [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  'R',  ' ',  ' ',  'Q',  'K',  'B',  'N',  'R',  '+',
                              '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                              '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  'b',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                              '+',  'p',  ' ',  'n',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                              '+',  ' ',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                              '+',  'r',  ' ',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
p   -   n   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   b   -   -   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
White: A2-A3
Black shouldn't need to lose material
-}

debugBoard03 :: ChessGrid
debugBoard03 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  'P',  'B',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'p',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  'q',  'k',  'b',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   n   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   P   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   P   B   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Black: E6-F4 ???
  look into the sequence:
  E6-F4
  E3xF4
  or the resulting hash
  9088222571369637754

  after E6-F4, Board hash: 4913391084627495306
  and then after E3xE4: -3107579353474362187


-}

debugBoard04 :: ChessGrid
debugBoard04 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  'B',  ' ',  '+',
                             '+',  ' ',  ' ',  'n',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  'b',  'q',  'k',  'b',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   p   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Black: D7-D5 ???
-}

debugBoard05 :: ChessGrid
debugBoard05 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  'K',  ' ',  ' ',  'B',  ' ',  'R',  '+',
                             '+',  'P',  ' ',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  'R',  'B',  'N',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'b',  ' ',  '+',
                             '+',  ' ',  'Q',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  'p',  '+',
                             '+',  ' ',  ' ',  'p',  ' ',  'n',  'p',  'p',  ' ',  '+',
                             '+',  'r',  ' ',  ' ',  'q',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   p   -   n   p   p   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   p          6| (50)  61   62   63   64   65   66   67   68  (69)
-   Q   -   P   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   b   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   R   B   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   K   -   -   B   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Black: G4-H3 ???
or
Black: A8-A5 ???

> cabal exec strat-exe -- -d4 -rdebug05 -cBlack --nopresort --ptracing --ctracing --tracestr="A8-A5, B5xA5"

-}

debugBoard06 :: ChessGrid
debugBoard06 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  ' ',  'R',  'K',  ' ',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'B',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  'P',  'N',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  'B',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  'n',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'p',  'b',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  ' ',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   p   b   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   n   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   P   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   B   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   -   R   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Black: ...D8-H4 ????? followed by F3xH4
Black: ...F8-A3 ??? followed by B2xA3

> cabal exec strat-exe -- -d4 -rdebug06 -cBlack --ctracing --tracestr="[ D8-H4,"

-}

debugBoard06b :: ChessGrid
debugBoard06b = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  ' ',  'R',  'K',  ' ',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'B',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  'P',  'N',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  'B',  ' ',  'q',  '+',
                             '+',  'p',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  'n',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'p',  'b',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  ' ',  ' ',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   p   b   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   n   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   B   -   q          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   P   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   B   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   -   R   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Same as 6, but horrible more D8-H4 made already
Test with the program playing white

if not, W F3xH4, something really wrong

> cabal exec strat-exe -- -d4 -rdebug06b --black-ai=False --white-ai=True  -- WRONG
> cabal exec strat-exe -- -d4 -rdebug06b --black-ai=False --white-ai=True -p -- RIGHT
The combination of pruning and random move variation is breaking something here...
-}

debugBoard06c :: ChessGrid
debugBoard06c = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  'Q',  ' ',  'R',  'K',  ' ',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'B',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  'P',  'N',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  'B',  ' ',  'q',  '+',
                             '+',  'p',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  'n',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'p',  'b',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  ' ',  ' ',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   p   b   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   n   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   B   -   q          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   P   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   B   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   -   R   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Same as 6b, but with white making crazy D1-C1
Test from here with the program playing Black
Black must immed move the queen on H4, if not, something is really wrong

> cabal exec strat-exe -- -d4 -rdebug06c
> cabal exec strat-exe -- -d4 -rdebug06c --ctracing --tracestr="[ C6xD4"
-}

debugBoard07 :: ChessGrid
debugBoard07 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  'K',  'R',  ' ',  'B',  'N',  'R',  '+',
                             '+',  'P',  ' ',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'Q',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  ' ',  'P',  ' ',  'P',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'p',  'n',  'p',  'p',  'p',  '+',
                             '+',  'r',  ' ',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   n   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   -   P   -   P   -          5| (50)  51   52   53   54   55   56   57   58  (59)
Q   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   K   R   -   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

Black:
.... D7-D5 ??

Why not move knight on F6?

> cabal exec strat-exe -- -d4 -rdebug07
-}


critBugBoard01 :: ChessGrid
critBugBoard01 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  'R',  'N',  ' ',  ' ',  'K',  'B',  ' ',  'R',  '+',
                              '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  'Q',  'P',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                              '+',  'p',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                              '+',  'r',  ' ',  'b',  'q',  'k',  'b',  ' ',  'r',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   B   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   Q   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   -   -   K   B   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H

Moves to verify (run with -d3 --critdepth=5),
should NOT be one of these:
Black:
... F6-G4
... F6-H5
-}
critBug01TestData :: StdMoveTestData
critBug01TestData  = StdMoveTestData
    { smtdBoardName = "critBug01"
    , smtdDepth = 3
    , smtdCritDepth = 5
    , smtdStartIdx = 66
    , smtdEndIdx = 47 }

critBug01TestDataB :: StdMoveTestData
critBug01TestDataB = StdMoveTestData
    { smtdBoardName = "critBug01"
    , smtdDepth = 3
    , smtdCritDepth = 5
    , smtdStartIdx = 66
    , smtdEndIdx = 58 }

----------------------------------------------------------------------------------------------------
castlingBoard :: ChessGrid
castlingBoard = ChessGrid $ V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                         '+',  'R',  ' ',  'B',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                                         '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'K',  'P',  '+',
                                         '+',  ' ',  ' ',  'N',  ' ',  ' ',  'Q',  ' ',  ' ',  '+',
                                         '+',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                                         '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                         '+',  ' ',  ' ',  'p',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                                         '+',  'p',  ' ',  'p',  ' ',  'p',  'p',  'p',  'p',  '+',
                                         '+',  ' ',  'r',  ' ',  ' ',  'k',  'b',  ' ',  'r',  '+',
                                         '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   r   -   -   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   p   -   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   Q   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   K   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   -   R   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Moves to verify (run with -d2 -c6):
<<(W) F3-C6 most recent >>
No weird king-side castle that makes the bishop dissappear...
-}

----------------------------------------------------------------------------------------------------
enPassantBoard01 :: ChessGrid
enPassantBoard01 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  'K',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                              '+',  ' ',  ' ',  'B',  'R',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  '+',
                              '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   k   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   p   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
P   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   P   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   B   R   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   B   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   K   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
enPassantBoard02 :: ChessGrid
enPassantBoard02 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  'R',  ' ',  'B',  'Q',  'K',  ' ',  ' ',  'R',  '+',
                              '+',  'P',  'P',  'P',  ' ',  'N',  'P',  'B',  'P',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  'P',  'p',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  'n',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                              '+',  'p',  'p',  'p',  ' ',  'b',  'p',  'p',  'p',  '+',
                              '+',  'r',  ' ',  'b',  'q',  'k',  ' ',  ' ',  'r',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   -   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   b   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   P   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   p   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   P   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   N   P   B   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   Q   K   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H

Bug:
Computer's move: F7-F5

Enter player's move:
E5 F6
Not a legal move.
-}

connectRooksBoard :: ChessGrid
connectRooksBoard = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'K',  ' ',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'Q',  ' ',  ' ',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  'r',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   k   -   -   -   -   r   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   -   r   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   P   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   Q   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   -   -   R   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}


----------------------------------------------------------------------------------------------------
endgameBoard01 :: ChessGrid
endgameBoard01 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  ' ',  ' ',  'K',  'R',  ' ',  ' ',  ' ',  'R',  '+',
                              '+',  ' ',  'P',  ' ',  ' ',  ' ',  'P',  ' ',  'P',  '+',
                              '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'Q',  ' ',  ' ',  '+',
                              '+',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'q',  ' ',  '+',
                              '+',  ' ',  'p',  'p',  ' ',  ' ',  'p',  'p',  ' ',  '+',
                              '+',  'r',  ' ',  'k',  ' ',  'r',  ' ',  ' ',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   k   -   r   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   p   -   -   p   p   -          7| (70)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   q   -          6| (60)  61   62   63   64   65   66   67   68  (69)
p   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   Q   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   -   P   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   -   -   -   P   -   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   K   R   -   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H

FEN: r1k1r3/1pp2pp1/6q1/p7/5Q2/P5P1/1P3P1P/2KR3R w - - 0 20
-}


_invalidTree :: (Tree ChessNode, ChessPosState)
_invalidTree =
    case getNodeFromFen "rnbqk1nr/ppp2ppp/4p3/3p4/2PP4/5N2/PP1NPPPP/R2QKB1R b KQkq - 0 5" of
      Left err -> error "mateInThreeTree02 invalid FEN?" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
r   n   b   q   k   -   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   P   P   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   N   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   Q   K   B   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H

Computer's move at this position:
Tree size: 1
[1]
Evaluated: 0 (percent saved by pruning: NaN)
strat-exe: received 'Max' in toNegaMoves?
CallStack (from HasCallStack):
  error, called at src/Strat/ZipTree.hs:158:19 in main:Strat.ZipTree
CallStack (from -prof):
  Strat.ZipTree.toNegaMoves (src/Strat/ZipTree.hs:(157,1)-(166,27))
  Strat.ZipTree.CAF (<entire-module>)

Moves to get there, starting from a new game:
D2-D4, D7-D5
C2-C4, E7-E6
G1-F3, F1-B4+
B1xD2, errore...

run params:
cabal exec strat-exe -- +RTS -p -RTS --nr

-}






invalidTree :: (Tree ChessNode, ChessPosState)
invalidTree =
    case getNodeFromFen "rnbqkbnr/ppp2ppp/4p3/3p4/2PP4/5N2/PP2PPPP/RNBQKB1R b KQkq - 0 5" of
      Left err -> error "mateInThreeTree02 invalid FEN?" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   p   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   P   P   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   -   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H

Computer's move at this position:
Tree size: 1
[1]
Evaluated: 0 (percent saved by pruning: NaN)
strat-exe: received 'Max' in toNegaMoves?
CallStack (from HasCallStack):
  error, called at src/Strat/ZipTree.hs:158:19 in main:Strat.ZipTree
CallStack (from -prof):
  Strat.ZipTree.toNegaMoves (src/Strat/ZipTree.hs:(157,1)-(166,27))
  Strat.ZipTree.CAF (<entire-module>)

Moves to get there, starting from a new game:
D2-D4, D7-D5
C2-C4, E7-E6
G1-F3, F1-B4+
B1xD2, errore...

run params:
cabal exec strat-exe -- +RTS -p -RTS --nr

-}





----------------------------------------------------------------------------------------------------
checkMateExampleNode :: ChessNode
checkMateExampleNode = preCastlingGameNode checkMateExampleBoard White (15, 85)

checkMateExampleNode2 :: ChessNode
checkMateExampleNode2 = preCastlingGameNode checkMateExampleBoard2 White (15, 85)

checkMateExampleNode3 :: ChessNode
checkMateExampleNode3 = preCastlingGameNode checkMateExampleBoard3 White (15, 87)

checkMateExampleNode4 :: ChessNode
checkMateExampleNode4 = preCastlingGameNode checkMateExampleBoard4 White (15, 87)

mateInTwoExampleNode02 :: ChessNode
mateInTwoExampleNode02 = preCastlingGameNode mateInTwoBoard02 Black (21, 23)

mateInTwoExampleNode02b :: ChessNode
mateInTwoExampleNode02b = preCastlingGameNode mateInTwoBoard02b Black (11, 23)

mateInTwoExampleNode03 :: ChessNode
mateInTwoExampleNode03 = postCastlingGameNode mateInTwoBoard03 Black (54, 62)

mateInTwoExampleNode03b :: ChessNode
mateInTwoExampleNode03b = postCastlingGameNode mateInTwoBoard03b Black (45, 62)

promotionNode01 :: ChessNode
promotionNode01 = postCastlingGameNode promotionBoard01 White (16, 86)

critBugNode01 :: ChessNode
critBugNode01 = preCastlingGameNode critBugBoard01 Black (13, 87)

drawnExampleNode :: ChessNode
drawnExampleNode = preCastlingGameNode drawnBoard White (68, 88)

debugExampleNode :: ChessNode
debugExampleNode = preCastlingGameNode debugBoard White (15, 85)

debugExampleNode02 :: ChessNode
debugExampleNode02 = preCastlingGameNode debugBoard02 White (15, 85)

debugExampleNode03 :: ChessNode
debugExampleNode03 = preCastlingGameNode debugBoard03 Black (15, 85)

debugExampleNode04 :: ChessNode
debugExampleNode04 = preCastlingGameNode debugBoard04 Black (15, 85)

debugExampleNode05 :: ChessNode
debugExampleNode05 = preCastlingGameNode debugBoard05 Black (13, 87)

debugExampleNode06 :: ChessNode
debugExampleNode06 = preCastlingGameNode debugBoard06 Black (17, 85)

debugExampleNode06b :: ChessNode
debugExampleNode06b = preCastlingGameNode debugBoard06b White (17, 85)

debugExampleNode06c :: ChessNode
debugExampleNode06c = preCastlingGameNode debugBoard06c Black (17, 85)

debugExampleNode07 :: ChessNode
debugExampleNode07 = preCastlingGameNode debugBoard07 Black (13, 87)

castlingNode :: ChessNode
castlingNode = preCastlingGameNode castlingBoard Black (27, 85)

enPassantNode01 :: ChessNode
enPassantNode01 = postCastlingGameNode enPassantBoard01 Black (15, 83)

enPassantNode02 :: ChessNode
enPassantNode02 = preCastlingGameNode enPassantBoard02 Black (27, 85)

connectRookNode :: ChessNode
connectRookNode = postCastlingGameNode connectRooksBoard Black (22, 82)

endgameNode01 :: ChessNode
endgameNode01 = postCastlingGameNode' endgameBoard01 White 20 (13, 83)

preCastlingGameNode :: ChessGrid -> Color -> (Int, Int) -> ChessNode
preCastlingGameNode grid the_color kingLocs =
  let (wLocs, bLocs) = calcLocsForColor grid
      cPos =
        ChessPos
          { _cpGrid = grid,
            -- _cpColor = the_color,
            _cpKingLoc = kingLocs,
            _cpInCheck = False,
            _cpWhitePieceLocs = wLocs,
            _cpBlackPieceLocs = bLocs,
            _cpFin = NotFinal,
            _cpState = preCastledTestState the_color
          }
   in ChessNode
        { _chessTreeLoc = TreeLocation {tlDepth = 0},
          _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""},
          _chessVal = ChessEval {_total = 0.0, _details = ""},
          _chessErrorVal = ChessEval {_total = 0.0, _details = ""},
          _chessPos = cPos,
          _chessMvSeq = [],
          _chessIsEvaluated = False
        }

-- No castling available
postCastlingGameNode :: ChessGrid -> Color -> (Int, Int) -> ChessNode
postCastlingGameNode grid the_color kingLocs =
    postCastlingGameNode' grid the_color 0 kingLocs

postCastlingGameNode' :: ChessGrid -> Color -> Int -> (Int, Int) -> ChessNode
postCastlingGameNode' grid the_color moveNumber kingLocs =
  let (wLocs, bLocs) = calcLocsForColor grid
      cPos =
        ChessPos
          { _cpGrid = grid,
            _cpKingLoc = kingLocs,
            _cpInCheck = False,
            _cpWhitePieceLocs = wLocs,
            _cpBlackPieceLocs = bLocs,
            _cpFin = NotFinal,
            _cpState = castledTestState' the_color 40
          }
   in ChessNode
        { _chessTreeLoc = TreeLocation {tlDepth = 0},
          _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""},
          _chessVal = ChessEval {_total = 0.0, _details = ""},
          _chessErrorVal = ChessEval {_total = 0.0, _details = ""},
          _chessPos = cPos,
          _chessMvSeq = [],
          _chessIsEvaluated = False
        }
