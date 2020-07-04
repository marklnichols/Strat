{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Chess
    ( ChessEval(..)
    , ChessMv(..)
    , ChessNode(..)
    , ChessPos(..)
    , Color(..)
    , calcDefended
    , colorFromInt
    , colorToInt
    -- exported for testing only
    , getPieceLocs
    , possibleBishopMoves
    , possibleKnightMoves
    , possibleKingMoves
    , possiblePawnMoves
    , possiblePawnCaptures
    , possibleQueenMoves
    , possibleRookMoves
    ) where

import Control.Lens

import Data.Char
import Data.Foldable
import Data.HashMap (Map)
-- import qualified Data.HashMap as M
import Data.Kind
import Data.Maybe
import Data.Singletons
import Data.Singletons.TH
import Data.Tree
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Strat.StratTree.TreeNode
import qualified ChessParser as P
-- import Debug.Trace

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
empty :: Char
empty = ' '

data Color = Black | White | Unknown
    deriving (Show, Eq)

data ChessPos = ChessPos {_cpGrid :: Vector Char, _cpColor :: Color, _cpFin :: FinalState} deriving (Show)
makeLenses ''ChessPos

data ChessMv = ChessMv {isExchange :: Bool, _startIdx :: Int, _endIdx :: Int, _removedIdx :: Int}
    deriving (Eq, Ord, Show)
makeLenses ''ChessMv

data ChessEval = ChessEval {_total :: Int, _details :: String} deriving (Eq, Ord)
makeLenses ''ChessEval

data ChessNode = ChessNode {_chessMv :: ChessMv, _chessVal :: ChessEval,
                            _chessErrorVal :: ChessEval, _chessPos :: ChessPos}
makeLenses ''ChessNode

colorToInt :: Color -> Int
colorToInt Black = -1
colorToInt White = 1
colorToInt Unknown = 0

colorFromInt :: Int -> Color
colorFromInt 1 = White
colorFromInt (-1) = Black
colorFromInt _n = Unknown

----------------------------------------------------------------------------------------------------
-- New, new attempt with singletons
----------------------------------------------------------------------------------------------------
$(singletons [d|
  data Piece = King | Queen | Rook | Knight | Bishop | Pawn
    deriving (Show, Eq)
  |])

data ChessPiece :: SomeSing Piece -> Type where
  MkChessPiece :: Color -> SomeSing Piece -> ChessPiece k

intToPiece :: Int -> ChessPiece (k :: SomeSing Piece)
intToPiece i = charToPiece (chr i)

pieceToInt :: Piece -> Color -> Int
pieceToInt p c = ord $ pieceToChar p c

pieceToChar :: Piece -> Color -> Char
pieceToChar _ Unknown = ' '
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
charToPiece ch = error $ "charToPiece not implemented for the value " ++ show ch

indexToColor :: Vector Char -> Int -> Color
indexToColor g idx =
    let gridVal = fromMaybe ' ' (g ^? ix idx)
    in charToColor gridVal

indexToPiece :: Vector Char -> Int -> ChessPiece (k :: SomeSing Piece)
indexToPiece g idx =
    let gridVal =  fromMaybe ' ' (g ^? ix idx)
    in charToPiece gridVal

instance PositionNode ChessNode ChessMv ChessEval where
    newNode = calcNewNode
    possibleMoves = legalMoves
    color = colorToInt . view (chessPos . cpColor)
    final = view (chessPos . cpFin)
    parseMove = parseChessMv

instance TreeNode ChessNode ChessMv ChessEval where
    getMove = _chessMv
    getValue = _chessVal
    getErrorValue = _chessErrorVal

instance Show ChessNode where
    show n = "move: " ++ show (n ^. chessMv) ++ " value: " ++ show (n ^. chessVal) ++ " errorValue: "
             ++ show (n ^. chessErrorVal) ++ " position: " ++ show (n ^. chessPos)

instance Show ChessEval where
    show e = "Total " ++ show (e ^. total) ++ " made up of " ++ (e ^. details)

instance Move ChessMv

instance Eval ChessEval where
    getInt e = e ^. total
    setInt e n = e & total .~ n
    fromInt n = ChessEval {_total = n, _details = ""}

---------------------------------------------------------------------------------------------------
-- starting position,
---------------------------------------------------------------------------------------------------
getStartNode :: Tree ChessNode
getStartNode = undefined

---------------------------------------------------------------------------------------------------
-- Grid layout - indexes 0-99
---------------------------------------------------------------------------------------------------
{-- how indexes relate to board position (indexes in parens are off the edge of the board):

   (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

8| (80)  81   82   83   84   85   86   87   88  (89)
7| (50)  71   72   73   74   75   76   77   78  (79)
6| (50)  61   62   63   64   65   66   67   68  (69)
5| (50)  51   52   53   54   55   56   57   58  (59)
4| (40)  41   42   43   44   45   46   47   48  (49)
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
1 = White King      -1 = Black King
2 = White Queen     -2 = Black Queen
3 = White Rook      -3 = Black Rook
4 = White Bishop    -4 = Black Bishop
5 = White Knight    -5 = Black Knight
6 = White Pawn      -6 = Black Pawn

75 = 0x4b = White King     107 = 0x6b = White King
81 = 0x51 = White Queen    113 = 0x71 = White Queen
82 = 0x52 = White Rook     114 = 0x72 = White Rook
78 = 0x4e = White Knight   110 = 0x6e = White Knight
66 = 0x42 = White Bishop   098 = 0x62 = White Bishop
80 = 0x50 = White Pawn     112 = 0x70 = White Pawn

---------------------------------------------------------------------------------------------------}
type Dir = Int -> Int

right, left, up, down, diagUL, diagDR, diagUR, diagDL, knightLU, knightRD, knightLD  :: Dir
right = (1+)
left x = x - 1
up = (10+)
down x = x - 10
diagUL = (9+)
diagDR x = x - 9
diagUR = (11+)
diagDL x = x - 11
knightLU = (8+)
knightRD x = x - 8
knightLD x = x - 12

knightRU, knightUL, knightDR, knightUR, knightDL :: Dir
knightRU = (12+)
knightUL = (19+)
knightDR x = x - 19
knightUR = (21+)
knightDL x = x - 21

queenDirs :: [Dir]
queenDirs = [right, left, up, down, diagUL, diagDR, diagUR, diagDL]

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

blackPawnDir :: Dir -- non capturing moves...
blackPawnDir = down

blackPawnCaptureDirs :: [Dir]
blackPawnCaptureDirs = [diagDL, diagDR]

noDirs :: [Dir]
noDirs = []

---------------------------------------------------------------------------------------------------
-- Convert ChessMv to Parser Move (for display)
---------------------------------------------------------------------------------------------------
toParserMove :: ChessMv -> Maybe P.Move
toParserMove _ = undefined

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
format :: ChessNode -> String
format _ = undefined

---------------------------------------------------------------------------------------------------
-- parse string input to move
---------------------------------------------------------------------------------------------------
parseChessMv :: ChessNode -> String -> Either String ChessMv
parseChessMv _ _ = undefined

---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: ChessNode -> ChessMv -> ChessNode
calcNewNode _ _ = undefined

---------------------------------------------------------------------------------------------------
-- get possible moves from a given position
--------------------------------------------------------------------------------------------------
legalMoves :: ChessNode -> [ChessMv]
legalMoves = undefined

---------------------------------------------------------------------------------------------------
-- get piece locations for the current color from a ChessNode
--------------------------------------------------------------------------------------------------
getPieceLocs :: ChessNode -> [Int]
getPieceLocs node =
    let pos = node ^. chessPos
    in locsForColor (_cpGrid pos) (_cpColor pos)

---------------------------------------------------------------------------------------------------
-- get piece locations for a given color from a board
--------------------------------------------------------------------------------------------------
locsForColor :: Vector Char -> Color -> [Int]
locsForColor locs theColor =
    let range = V.fromList [0..100] :: Vector Int
        pairs = V.zip range locs
        filtrd = V.filter ((pMatch theColor) . snd) pairs
        first = V.map fst filtrd
    in V.toList first
        where
          pMatch clr ch  = charToColor ch == clr

charToColor :: Char -> Color
charToColor c
  | ord c > 64 && ord c < 91 = White  -- A - Z : 65 - 90
  | ord c > 86 && ord c < 123 = Black -- a - z : 97 - 122
  | otherwise = Unknown

pieceMoves :: ChessNode -> Int -> [ChessMv]
pieceMoves _ _ = undefined

-- movesFromDir :: Dir -> Int -> MoveType -> [ChessMv]
-- movesFromDir _ _ _ = undefined  -- dir idx moveType

--possible destination squares for a king
possibleKingMoves :: Vector Char -> Int -> [Int]
possibleKingMoves g idx =
  let theColor = indexToColor g idx
      onTheBoard = filter onBoard (fmap ($ idx) queenDirs)
  in filter (not . hasFriendly g theColor) onTheBoard

legalKingMoves :: ChessPos -> (Map Int Bool) -> Int -> [ChessMv]
legalKingMoves pos defendedMap idx =
    let locs = possibleKingMoves (pos^.cpGrid) idx
        indexes = filter (kingFilter (pos ^.cpGrid) defendedMap (pos^.cpColor)) locs
    in  destinationsToMoves idx indexes

kingFilter :: Vector Char -> (Map Int Bool) -> Color -> Int -> Bool
kingFilter g defendedMap c idx =
    if hasFriendly g c idx
        then False
        else not $ isDefended defendedMap c idx

hasFriendly :: Vector Char -> Color -> Int -> Bool
hasFriendly g c idx = indexToColor g idx == c

-- hasFriendly pos c idx = case indexToPiece (_cpGrid pos) idx of
--     MkSomeChessPiece _theSing (UnsafeMkChessPiece aColor) -> aColor == c

hasEnemy :: Vector Char -> Color -> Int -> Bool
hasEnemy g c idx = not $ hasFriendly g c idx

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos idx = fromMaybe empty ((_cpGrid pos) ^? ix idx) == empty

  -- for each opposing piece,
  -- build list of legal moves
  -- build Set consisting of each ending loc
  -- (for pawns this must be only the capturing moves)

----------------------------------------------------------------------------------------------------
-- Calculate the set of all locations that are 'defended' by the given color
-- The intendtion is to use this only once per position / color
-- This can be used to filter lists of possible moves in order to quickly resolve
-- captures to arbitrary depths
----------------------------------------------------------------------------------------------------
calcDefended :: Vector Char -> Color -> Set Int
calcDefended g c =
    let theLocs = locsForColor g c
        destLocs = movesFromLocs g theLocs
    in S.fromList destLocs

movesFromLocs :: Vector Char -> [Int] -> [Int]
movesFromLocs g xs =
  foldr f [] xs where
    f :: Int -> [Int] -> [Int]
    f loc moves = moves ++ movesFromLoc g loc

movesFromLoc :: Vector Char -> Int -> [Int]
movesFromLoc locs loc =
  let cp = indexToPiece locs loc -- ChessPiece (k :: SomeSing Piece)
  in case cp of
      MkChessPiece _c (SomeSing SKing) -> possibleKingMoves locs loc
      MkChessPiece _c (SomeSing SQueen) -> possibleQueenMoves locs loc
      MkChessPiece _c (SomeSing SRook) -> possibleRookMoves locs loc
      MkChessPiece _c (SomeSing SKnight) -> possibleKnightMoves locs loc
      MkChessPiece _c (SomeSing SBishop) -> possibleBishopMoves locs loc
      MkChessPiece c (SomeSing SPawn) -> possiblePawnCaptures locs c loc

isDefended :: Map Int Bool -> Color -> Int -> Bool
isDefended _ =  undefined   --color index
  -- this is just lookup of map built from calcDefended, with Nothing converted to False

-- find the possible destination locs for a queen
-- TODO: change all these 'possible' functions to filter out moves blocked by friendly pieces
possibleQueenMoves :: Vector Char -> Int -> [Int]
possibleQueenMoves _g idx = fold $ fmap (dirLocs idx) queenDirs

-- find the possible destination locs for a rook
possibleRookMoves :: Vector Char -> Int -> [Int]
possibleRookMoves _g idx = fold $ fmap (dirLocs idx) rookDirs

-- find the possible destination locs for a bishop
possibleBishopMoves :: Vector Char -> Int -> [Int]
possibleBishopMoves _g idx = fold $ fmap (dirLocs idx) bishopDirs

-- find the possible destination locs for a knight
possibleKnightMoves :: Vector Char -> Int -> [Int]
possibleKnightMoves _g idx = filter onBoard (fmap ($ idx) knightDirs)

-- find the possible destination locs for a pawn (non-capturing moves)
possiblePawnMoves :: Vector Char -> Int -> Color -> [Int]
possiblePawnMoves _g _idx Unknown = []
possiblePawnMoves _g idx White =
  let oneSpace = whitePawnDir idx
  in oneSpace : if hasPawnMoved White idx
                  then []
                  else [whitePawnDir oneSpace] -- hasn't moved, 2 space move avail.
possiblePawnMoves _g idx Black =
  let oneSpace = blackPawnDir idx
  in oneSpace : if hasPawnMoved Black idx
                  then []
                  else [blackPawnDir oneSpace] -- hasn't moved, 2 space move avail.

hasPawnMoved :: Color -> Int -> Bool
hasPawnMoved Unknown _ = False
hasPawnMoved White idx = idx > 28
hasPawnMoved Black idx = idx < 71

-- find the possible destination capture locs for a pawn
possiblePawnCaptures :: Vector Char -> Color -> Int -> [Int]
possiblePawnCaptures _g Unknown _idx = []
possiblePawnCaptures _g White idx =
  let twoCaps = filter onBoard (fmap ($ idx) whitePawnCaptureDirs)
  in twoCaps ++ if hasPawnMoved White idx
                  then []
                  else filter onBoard (fmap ($ whitePawnDir idx) whitePawnCaptureDirs) --en passant
possiblePawnCaptures _g Black idx =
  let twoCaps = filter onBoard (fmap ($ idx) blackPawnCaptureDirs)
  in twoCaps ++ if hasPawnMoved Black idx
                  then []
                  else filter onBoard (fmap ($ blackPawnDir idx) blackPawnCaptureDirs) --en passant

-- find the possible destination locs for a queen.  The first list contains the empty squares that
-- can be moved to. The second list contains squares with pieces that could be captured.
allowableQueenMvs :: Int -> ([Int], [Int])
allowableQueenMvs _idx = undefined
  {-
  foldr f ([], []) queenDirs
    where
      f :: Dir -> ([Int], [Int]) -> ([Int], [Int])
      f x (r, r') =
        let (freeLocs, captureLocs) = dirLocs idx x
        in (freeLocs ++ r, captureLocs ++ r')
  -}
--(stop if blocked by friendly piece)
--(include captured piece and stop)

-- for pieces that can move as many squares as desired in a given direction (i.e. queen, rook, bishop)
-- find the possible destination locs for a queen, given a specified direction to move.
dirLocs :: Int -> Dir ->[Int]
dirLocs idx dir = loop (dir idx) []
  where
    loop x r
        | onBoard x = loop (dir x) (x : r)
        | otherwise = r

onBoard :: Int -> Bool
onBoard x
    | x < 10          = False
    | x > 88          = False
    | x `mod` 10 == 0 = False
    | x `mod` 10 == 9 = False
    | otherwise       = True

offBoard :: Int -> Bool
offBoard x = not $ onBoard x

noMoves :: ChessPos -> Int -> [ChessMv]
noMoves _ _ = undefined

destinationsToMoves :: Int -> [Int] -> [ChessMv]
destinationsToMoves _ _ = undefined     -- idx dests
    --pair up idx with each index in dests to form moves

indexesToMove :: [Int] -> ChessMv
indexesToMove _ = undefined
