{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Chess
    ( ChessEval(..), total, details
    , ChessMove(..), isExchange, startIdx, endIdx
    , ChessMoves(..), cmEmpty, cmEnemy
    , ChessNode(..), chessMv, chessVal, chessErrorVal, chessPos
    , ChessPos(..), cpGrid, cpColor, cpFin
    , Color(..)
    , colorFromInt
    , colorToInt
    , countMaterial
    , getStartNode
    , toParserMove
    -- exported for testing only
    , getPieceLocs
    , allowableBishopMoves
    , allowableKnightMoves
    , possibleKingMoves
    , allowableEnPassant
    , allowablePawnNonCaptures
    , allowablePawnCaptures
    , allowableQueenMoves
    , allowableRookMoves
    , calcMoveListsGrid
    , locsForColor
    , pairToIndexes
    , startingBoard
    ) where

import Control.Lens hiding (Empty)

import Data.Char
import Data.HashMap (Map)
import Data.Kind
import Data.Maybe
import Data.Singletons
import Data.Singletons.TH
import Data.Tree
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Text.Printf

import qualified CkParser as Parser -- TODO: rename this to be more general
import Strat.StratTree.TreeNode
import Debug.Trace


---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
empty :: Char
empty = ' '

data ChessMove = ChessMove {_isExchange :: Bool, _startIdx :: Int, _endIdx :: Int}
    deriving (Eq, Ord)
makeLenses ''ChessMove

toParserMove :: ChessMove -> Parser.Move
toParserMove mv = Parser.Move $ intToParserLoc (mv^.startIdx) : [intToParserLoc (mv^.endIdx)]

intToParserLoc :: Int -> Parser.Loc
intToParserLoc n =
    let r = n `div` 10
        c = chr $ 64 + (n - r * 10)
    in Parser.Loc c r

instance Show ChessMove where
  show cm@ChessMove{..} =
    let pm = show $ toParserMove cm
        exch = if _isExchange then " [Exchange]" else ""
    in pm ++ exch

data Color = Black | White | Unknown
    deriving (Show, Eq)

data Castling = Castled
              | LeftOnlyAvailable
              | RightOnlyAvailable
              | BothAvailable
              | Unavailable
  deriving (Eq, Show)

data CastlingState = CastlingState
  { _csWhiteCastling :: Castling
  , _csBlackCastling :: Castling }
  deriving (Show, Eq)
makeLenses ''CastlingState

---------------------------------------------------------------------------------------------------

data ChessMoves = ChessMoves
  { _cmEmpty :: [ChessMove]
  , _cmEnemy :: [ChessMove]
  , _cmForColor :: Color }  -- for debugging output
  deriving (Eq)
makeLenses ''ChessMoves

instance Show ChessMoves where
  show ChessMoves{..} = "Moves for color: " ++ show _cmForColor
    ++  "\nEmpty: " ++ show _cmEmpty ++ "\nEnemy: " ++ show _cmEnemy

data ChessPos = ChessPos { _cpGrid :: Vector Char, _cpColor :: Color
                         , _cpCastlingState :: CastlingState, _cpFin :: FinalState
                         }  deriving (Show)
makeLenses ''ChessPos

data ChessEval = ChessEval {_total :: Int, _details :: String} deriving (Eq, Ord)
makeLenses ''ChessEval

data ChessNode = ChessNode {_chessMv :: ChessMove, _chessVal :: ChessEval, _chessErrorVal :: ChessEval
                           , _chessPos :: ChessPos}
makeLenses ''ChessNode

colorToInt :: Color -> Int
colorToInt Black = -1
colorToInt White = 1
colorToInt Unknown = 0

colorFromInt :: Int -> Color
colorFromInt 1 = White
colorFromInt (-1) = Black
colorFromInt _n = Unknown

enemyColor :: Color -> Color
enemyColor White = Black
enemyColor Black = White
enemyColor Unknown = Unknown

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
charToPiece ' ' = MkChessPiece Unknown (SomeSing SNone)
charToPiece '+' = MkChessPiece Unknown (SomeSing SOffBoardNone)
charToPiece ch = error $ "charToPiece not implemented for the value " ++ show ch

indexToColor :: Vector Char -> Int -> Color
indexToColor g idx =
    let gridVal = fromMaybe ' ' (g ^? ix idx)
    in charToColor gridVal

indexToPiece :: Vector Char -> Int -> ChessPiece (k :: SomeSing Piece)
indexToPiece g idx =
    let gridVal =  fromMaybe ' ' (g ^? ix idx)
    in charToPiece gridVal

indexToChar :: Vector Char -> Int -> Char
indexToChar g idx = fromMaybe ' ' (g ^? ix idx)

pieceVal :: ChessPiece (k :: SomeSing Piece) -> Int
pieceVal piece@(MkChessPiece c _) =
  let absVal = pieceAbsVal piece
  in case c of
    White -> absVal
    Black -> negate absVal
    Unknown -> 0

pieceAbsVal :: ChessPiece (k :: SomeSing Piece) -> Int
pieceAbsVal (MkChessPiece _c (SomeSing SKing)) = 100
pieceAbsVal (MkChessPiece _c (SomeSing SQueen)) = 9
pieceAbsVal (MkChessPiece _c (SomeSing SRook)) = 5
pieceAbsVal (MkChessPiece _c (SomeSing SKnight)) = 3
pieceAbsVal (MkChessPiece _c (SomeSing SBishop)) = 3
pieceAbsVal (MkChessPiece _c (SomeSing SPawn)) = 1
pieceAbsVal (MkChessPiece _c (SomeSing SNone)) = 0
pieceAbsVal (MkChessPiece _c (SomeSing SOffBoardNone)) = 0

instance PositionNode ChessNode ChessMove ChessEval where
    newNode = calcNewNode
    possibleMoves = legalMoves
    color = colorToInt . view (chessPos . cpColor)
    final = view (chessPos . cpFin)
    parseMove = parseChessMove

instance TreeNode ChessNode ChessMove ChessEval where
    getMove = _chessMv
    getValue = _chessVal
    getErrorValue = _chessErrorVal

instance Show ChessNode where
    show n = "move: " ++ show (n ^. chessMv) ++ " value: " ++ show (n ^. chessVal) ++ " errorValue: "
             ++ show (n ^. chessErrorVal) ++ " position: " ++ show (n ^. chessPos)

instance Show ChessEval where
    show e = "Total " ++ show (e ^. total) ++ " made up of " ++ (e ^. details)

instance Move ChessMove

instance Eval ChessEval where
    getInt e = e ^. total
    setInt e n = e & total .~ n
    fromInt n = ChessEval {_total = n, _details = ""}

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
startingBoard :: V.Vector Char
startingBoard = V.fromList
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
---------------------------------------------------------------------------------------------------
-- starting position
-- TODO: let this be either color
---------------------------------------------------------------------------------------------------
getStartNode :: Tree ChessNode
getStartNode =
    let firstColor = White
        cPos = ChessPos { _cpGrid = mkStartGrid White, _cpColor = firstColor
                        , _cpCastlingState = CastlingState
                          { _csWhiteCastling = BothAvailable
                          , _csBlackCastling = BothAvailable }
                        , _cpFin = NotFinal }
    in Node ChessNode
        { _chessMv = ChessMove {_isExchange = False, _startIdx = -1, _endIdx = -1}
        , _chessVal = ChessEval { _total = 0, _details = "" }
        , _chessErrorVal = ChessEval { _total = 0, _details = "" }
        , _chessPos = cPos}

      []

-- Color represents the color at the 'bottom' of the board
mkStartGrid :: Color -> V.Vector Char
mkStartGrid White = startingBoard
mkStartGrid Black = V.reverse startingBoard
mkStartGrid Unknown = V.empty

---------------------------------------------------------------------------------------------------}
-- TODO: revert this eventually...
-- type Dir = Int -> Int
data Dir = Dir { apply :: Int -> Int, _dirName :: String }

right, left, up, down, diagUL, diagDR, diagUR, diagDL :: Dir
right = Dir (1+) "right"
left = Dir (\x -> x - 1) "left"
up = Dir (10+) "up"
down = Dir (\x -> x - 10) "down"
diagUL = Dir (9+) "diagUL"
diagDR = Dir (\x -> x - 9) "diagDR"
diagUR = Dir (11+) "diagUR"
diagDL = Dir (\x -> x - 11) "diagDL"

knightLU, knightRD, knightRU, knightLD, knightUL, knightDR, knightUR, knightDL :: Dir
knightLU = Dir (8+) "knightLU"
knightRD = Dir (\x -> x - 8) "knightRD"
knightRU = Dir (12+) "knightRU"
knightLD = Dir (\x -> x - 12) "knightLD"
knightUL = Dir (19+) "knightUL"
knightDR = Dir (\x -> x - 19) "knightDR"
knightUR = Dir (21+) "knightUR"
knightDL = Dir (\x -> x - 21) "knightDL"

instance Show Dir where
  show = _dirName

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
whitePawnEnPassantDirs = [knightUL, knightUR]

blackPawnDir :: Dir -- non capturing moves...
blackPawnDir = down

blackPawnCaptureDirs :: [Dir]
blackPawnCaptureDirs = [diagDL, diagDR]

blackPawnEnPassantDirs :: [Dir]
blackPawnEnPassantDirs = [knightDL, knightDR]

noDirs :: [Dir]
noDirs = []

---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: ChessNode -> ChessMove -> ChessNode
calcNewNode node mv =
    let
        prevPos = node ^. chessPos
        prevGrid = prevPos ^. cpGrid
        prevColor = prevPos ^. cpColor
        newCastling = updateCastling prevColor prevGrid (prevPos ^. cpCastlingState) mv
        newGrid = movePieceGrid prevGrid (mv ^. startIdx) (mv ^. endIdx)
        clrFlipped = flipPieceColor prevColor
        newPos = prevPos { _cpGrid = newGrid
                         , _cpColor = clrFlipped }
        (eval, finalSt) = evalPos newPos
        updatedPos = newPos { _cpFin = finalSt }
    in ChessNode { _chessMv = mv , _chessVal = eval
                 , _chessErrorVal = ChessEval {_total = 0, _details = "not implemented"}
                 , _chessPos = updatedPos }

---------------------------------------------------------------------------------------------------
flipPieceColor :: Color -> Color
flipPieceColor White = Black
flipPieceColor Black = White
flipPieceColor Unknown = Unknown

---------------------------------------------------------------------------------------------------
-- TODO: change state on rook moves as well
updateCastling :: Color -> Vector Char -> CastlingState -> ChessMove -> CastlingState
updateCastling White g oldState mv =
  let piece = indexToPiece g (mv ^. startIdx)
  in case piece of
    (MkChessPiece _c (SomeSing SKing)) -> oldState { _csWhiteCastling = Unavailable }
    (MkChessPiece _c (SomeSing _)) -> oldState
updateCastling _ g oldState mv =
  let piece = indexToPiece g (mv ^. startIdx)
  in case piece of
    (MkChessPiece _c (SomeSing SKing)) -> oldState { _csBlackCastling = Unavailable }
    (MkChessPiece _c (SomeSing _)) -> oldState

--TODO: add more evaluations and set the FinalState appropirately
---------------------------------------------------------------------------------------------------
-- Evaluate and produce a score for the position
--------------------------------------------------------------------------------------------------
evalPos :: ChessPos -> (ChessEval, FinalState)
evalPos pos =
     let material = countMaterial (pos ^. cpGrid)
         mobility = calcMobility pos
         castling = calcCastling pos
         t = (material * 10 ) + mobility + (castling * 10)
         detailsStr = "\n\tMaterial (x 10): " ++ show (material * 10)
                ++ "\n\tMobility (x 1): " ++ show mobility
                ++ "\n\tCastling (x 10): " ++ show (castling * 10)
         eval = ChessEval { _total = t
                          , _details = detailsStr
                          }
         finalState = NotFinal
     in (eval, finalState)

---------------------------------------------------------------------------------------------------
-- Count the 'material' score for the pieces on the board
--------------------------------------------------------------------------------------------------
countMaterial :: Vector Char -> Int
countMaterial = V.foldr f 0
  where
    f ch theTotal =
        if ch == empty
          then theTotal
          else theTotal + pieceVal (charToPiece ch)

---------------------------------------------------------------------------------------------------
-- Calculate the 'mobility' score for the pieces on the board
--------------------------------------------------------------------------------------------------
calcMobility :: ChessPos -> Int
calcMobility cp =
  let g = cp ^. cpGrid
      wLocs = locsForColor g White
      wCnt = moveCountFromLocs g wLocs
      bLocs = locsForColor g Black
      bCnt = moveCountFromLocs g bLocs
   in wCnt - bCnt

---------------------------------------------------------------------------------------------------
-- Calculate the castling score for the position
--------------------------------------------------------------------------------------------------
calcCastling :: ChessPos -> Int
calcCastling cp =
  let castlingState = cp ^. cpCastlingState
  in castlingToAbsVal (castlingState ^. csWhiteCastling)
  - castlingToAbsVal (castlingState ^. csBlackCastling)


castlingToAbsVal :: Castling -> Int
castlingToAbsVal Castled = 3
castlingToAbsVal Unavailable = -3
castlingToAbsVal BothAvailable = 0
castlingToAbsVal LeftOnlyAvailable = -1
castlingToAbsVal RightOnlyAvailable = -1

moveCountFromLocs :: Vector Char -> [Int] -> Int
moveCountFromLocs g =
  foldr f 0 where
    f :: Int -> Int -> Int
    f loc r =
      let cnt = moveCountFromLoc g loc
      in cnt + r

moveCountFromLoc :: Vector Char -> Int -> Int
moveCountFromLoc g loc =
  let cp = indexToPiece g loc -- ChessPiece (k :: SomeSing Piece)
  in case cp of
      MkChessPiece _c (SomeSing SQueen) -> queenMobility g loc
      MkChessPiece _c (SomeSing SRook) -> rookMobility g loc
      MkChessPiece _c (SomeSing SKnight) -> knightMobility g loc
      MkChessPiece _c (SomeSing SBishop) -> bishopMobility g loc
      MkChessPiece _c (SomeSing _) -> 0

---------------------------------------------------------------------------------------------------
-- Move a piece on the board, removing any captured piece.  Returns the updated node
--------------------------------------------------------------------------------------------------
movePiece :: ChessNode -> Int -> Int -> ChessNode
movePiece node pFrom pTo =
    let newGrid = movePieceGrid (node ^. (chessPos . cpGrid)) pFrom pTo
    in set (chessPos . cpGrid) newGrid node

-- removePiece :: ChessNode -> Int -> ChessNode
-- removePiece node idx = set (chessPos . cpGrid . ix idx) empty node

---------------------------------------------------------------------------------------------------
-- Move a piece on the grid vector, removing any captured piece.  Returns the updated grid
--------------------------------------------------------------------------------------------------
movePieceGrid :: Vector Char -> Int -> Int -> Vector Char
movePieceGrid g pFrom pTo =
    let pieceChar = g ^? ix pFrom
    in case pieceChar of
        Nothing -> g
        Just ch -> let z = checkPromote g ch pTo
                       p = set (ix pTo) z g
                   in removePieceGrid p pFrom

removePieceGrid :: Vector Char -> Int -> Vector Char
removePieceGrid g idx = set (ix idx) empty g

-- TODO: currently not implemented
checkPromote :: Vector Char -> Char -> Int -> Char
checkPromote _g chPiece _toLoc = chPiece

---------------------------------------------------------------------------------------------------
-- get possible moves from a node
--------------------------------------------------------------------------------------------------
legalMoves :: ChessNode -> [ChessMove]
legalMoves node =
    let pos = node ^. chessPos
        g = pos ^. cpGrid
        moves = calcMoveListsGrid g (pos ^. cpColor)
    in _cmEmpty moves ++ _cmEnemy moves

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
        filtrd = V.filter (pMatch theColor . snd) pairs
        theFirst = V.map fst filtrd
    in V.toList theFirst
        where
          pMatch clr ch  = charToColor ch == clr

charToColor :: Char -> Color
charToColor c
  | ord c > 64 && ord c < 91 = White  -- A - Z : 65 - 90
  | ord c > 96 && ord c < 123 = Black -- a - z : 97 - 122
  | otherwise = Unknown

-- r to R, B to b, etc.
flipCharColor :: Char -> Char
flipCharColor ch =
  case charToColor ch of
    Black -> chr $ ord ch - 32
    White -> chr $ ord ch + 32
    Unknown -> ch

-- The function 'allowableKingMoves' later filters out the moves that would allow the enemy to
-- capture the king
possibleKingMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
possibleKingMoves = allowableSingleMoves kingDirs

-- TODO: this will probably become 'allowableKingMoves'
legalKingMoves :: ChessPos -> Map Int Bool -> Int -> [ChessMove]
legalKingMoves _pos _defendedMap _idx = error "legalKindMoves is undefined"
    -- let locs = possibleKingMoves (pos^.cpGrid) idx
    --     indexes = filter (kingFilter (pos ^.cpGrid) defendedMap (pos^.cpColor)) locs
    -- in  destinationsToMoves idx indexes

-- kingFilter :: Vector Char -> Map Int Bool -> Color -> Int -> Bool
-- kingFilter g defendedMap c idx =
--     if hasFriendly g c idx
--         then False
--         else not $ isDefended defendedMap c idx

hasFriendly :: Vector Char -> Color -> Int -> Bool
hasFriendly g c idx = indexToColor g idx == c

hasEnemy :: Vector Char -> Color -> Int -> Bool
hasEnemy g c idx = indexToColor g idx == enemyColor c

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos idx = fromMaybe empty (_cpGrid pos ^? ix idx) == empty

isEmpty' :: ChessNode -> Int -> Bool
isEmpty' node = isEmpty (_chessPos node )

isEmptyGrid :: Vector Char -> Int -> Bool
isEmptyGrid g idx =  fromMaybe empty (g ^? ix idx) == empty

pairToIndexes :: ([ChessMove], [ChessMove]) -> ([Int], [Int])
pairToIndexes (xs, ys) = (_endIdx <$> xs, _endIdx <$> ys)

----------------------------------------------------------------------------------------------------
-- Calculate the set of all possible moves for the given position & the position's color
----------------------------------------------------------------------------------------------------
calcMoveListsGrid :: Vector Char -> Color -> ChessMoves
calcMoveListsGrid g c =
    let (_empty, enemy) = pieceDestinations g c
    in ChessMoves
      { _cmEmpty = _empty
      , _cmEnemy = enemy
      , _cmForColor = c }

pieceDestinations :: Vector Char -> Color -> ([ChessMove], [ChessMove])
pieceDestinations g c =
  let locs = locsForColor g c
  in movesFromLocs g locs

movesFromLocs :: Vector Char -> [Int] -> ([ChessMove], [ChessMove])
movesFromLocs g =
  foldr f ([], []) where
    f :: Int -> ([ChessMove], [ChessMove]) -> ([ChessMove], [ChessMove])
    f loc (r1, r2) =
      let (xs, ys) = movesFromLoc g loc
      in (xs ++ r1, ys ++ r2)

movesFromLoc :: Vector Char -> Int -> ([ChessMove], [ChessMove])
movesFromLoc g loc =
  let cp = indexToPiece g loc -- ChessPiece (k :: SomeSing Piece)
  in case cp of
      MkChessPiece _c (SomeSing SKing) -> possibleKingMoves g loc
      MkChessPiece _c (SomeSing SQueen) -> allowableQueenMoves g loc
      MkChessPiece _c (SomeSing SRook) -> allowableRookMoves g loc
      MkChessPiece _c (SomeSing SKnight) -> allowableKnightMoves g loc
      MkChessPiece _c (SomeSing SBishop) -> allowableBishopMoves g loc
      MkChessPiece _c (SomeSing SPawn) ->
        -- let (empties, enemies, friendlies) = allowablePawnCaptures g loc
        -- in (allowablePawnNonCaptures g loc ++ empties, enemies, friendlies)
        allowablePawnMoves g loc
      MkChessPiece _c (SomeSing _) -> ([], [])

isDefended :: Map Int Bool -> Color -> Int -> Bool
isDefended _ =  error "isDefended is undefined"   --color index
  -- this is just lookup of map built from calcDefended, with Nothing converted to False

allowableMultiMoves :: [Dir] -> Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableMultiMoves pieceDirs g idx =
  foldr f ([], []) pieceDirs
    where
      f :: Dir -> ([ChessMove], [ChessMove]) -> ([ChessMove], [ChessMove])
      f x (r, r') =
        let (freeLocs, captureLocs) = dirLocs g idx x
        in (freeLocs ++ r, captureLocs ++ r')

multiMoveMobility :: [Dir] -> Vector Char -> Int -> Int
multiMoveMobility pieceDirs g idx =
  foldr f 0 pieceDirs
    where
      f :: Dir -> Int -> Int
      f x r =
        let count = dirLocsCount g idx x
        in count + r

-- find the destination locs for pieces that move one square in a given
-- direction (i.e., King and Knight). See @allowableMultiMoves
allowableSingleMoves :: [Dir] -> Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableSingleMoves pieceDirs g idx =
  foldr (f (indexToColor g idx)) ([], []) pieceDirs
    where
      -- fold function: curried f with Color applied
      f :: Color -> Dir -> ([ChessMove], [ChessMove]) -> ([ChessMove], [ChessMove])
      f c x (r, r') =
        let (freeLocs, captureLocs) = dirLocsSingle g idx c x
        in (freeLocs ++ r, captureLocs ++ r')

singleMoveMobility :: [Dir] -> Vector Char -> Int -> Int
singleMoveMobility pieceDirs g idx =
  foldr f 0 pieceDirs
    where
      f :: Dir -> Int -> Int
      f x r =
        let count = dirLocsSingleCount g idx x
        in count + r

-- find the allowable destination locs for a queen.
allowableQueenMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableQueenMoves = allowableMultiMoves queenDirs

queenMobility :: Vector Char -> Int -> Int
queenMobility = multiMoveMobility queenDirs

-- find the allowable destination locs for a rook
allowableRookMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableRookMoves = allowableMultiMoves rookDirs

rookMobility :: Vector Char -> Int -> Int
rookMobility = multiMoveMobility rookDirs

-- find the allowable destination locs for a bishop
allowableBishopMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableBishopMoves = allowableMultiMoves bishopDirs

bishopMobility :: Vector Char -> Int -> Int
bishopMobility = multiMoveMobility bishopDirs

-- find the possible destination locs for a knight
allowableKnightMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableKnightMoves = allowableSingleMoves knightDirs

knightMobility :: Vector Char -> Int -> Int
knightMobility = singleMoveMobility knightDirs

allowablePawnMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowablePawnMoves g idx =
   let (_, enemies) = allowablePawnCaptures g idx
   in (allowablePawnNonCaptures g idx, enemies)

-- find the allowable destination locs for a pawn (non-capturing moves)
allowablePawnNonCaptures :: Vector Char -> Int -> [ChessMove]
allowablePawnNonCaptures g idx =
    let c = indexToColor g idx
        hasMoved = hasPawnMoved c idx
    in case c of
        Unknown -> []
        White -> pawnMoves g c whitePawnDir hasMoved idx
        Black -> pawnMoves g c blackPawnDir hasMoved idx

pawnMoves :: Vector Char -> Color -> Dir -> Bool -> Int -> [ChessMove]
pawnMoves g c dir hasMoved idx =
    case dirLocsSingle g idx c dir of
        ([], _) -> []
        (firstMove:_, _) ->  -- only take the 'empty' square
            let twoSpacer =
                  if hasMoved then []
                  else
                    -- get the one square moves starting from the end loc of the first
                    -- and combine with fistMove to make a 2 square pawn move
                    fmap f (fst $ dirLocsSingle g (_endIdx firstMove) c dir)
                    where f m = m {_startIdx = _startIdx firstMove}
            in firstMove : twoSpacer

hasPawnMoved :: Color -> Int -> Bool
hasPawnMoved Unknown _ = False
hasPawnMoved White idx = idx > 28
hasPawnMoved Black idx = idx < 71

-- find the allowable destination capture locs for a pawn (enPassant are not included here and
-- are handled elsewhere)
allowablePawnCaptures :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowablePawnCaptures g idx =
    let c = indexToColor g idx
    in pawnCaptures g c idx

-- EnPassant captures are handled elsewhere and are not included here
pawnCaptures :: Vector Char -> Color -> Int -> ([ChessMove], [ChessMove])
pawnCaptures g c idx =
      let dirs = case c of
              White -> whitePawnCaptureDirs
              Black -> blackPawnCaptureDirs
              Unknown -> []
          (empties, enemies) = allowableSingleMoves dirs g idx
      in (empties, enemies)

-- find the allowable enPassant destination capture locs for a pawn
-- only enemy squares containing pawns are returned
allowableEnPassant :: Vector Char -> Int -> [ChessMove]
allowableEnPassant g idx =
    let c = indexToColor g idx
    in enPassantCaptures g c idx

enPassantCaptures :: Vector Char -> Color -> Int -> [ChessMove]
enPassantCaptures g c idx =
    if hasPawnMoved c idx then []
    else
      let dirs = case c of
              White -> whitePawnEnPassantDirs
              Black -> blackPawnEnPassantDirs
              Unknown -> []
          enemies = snd $ allowableSingleMoves dirs g idx
          thisPawn = indexToChar g idx
      -- only pawns can be captured this way
      in filter (\n -> indexToChar g (_endIdx n) == flipCharColor thisPawn) enemies

data SquareState = Empty | HasFriendly | HasEnemy | OffBoard
   deriving Show

-- find the allowable destination locs for a piece, given a specified direction to move.
-- this function is appropriate for queens, rooks, and bishops
-- the search short-circuits when hitting a friendly or enemy pieces
dirLocs :: Vector Char -> Int -> Dir ->([ChessMove], [ChessMove])
dirLocs g idx dir =
    loop (apply dir idx) ([], [])
      where
        c = indexToColor g idx
        loop x (empties, enemies) =
            let friendly = hasFriendly g c x
                enemy = hasEnemy g c x
                (sqState, (newEmpties, newEnemies))
                    | not (onBoard x) = (OffBoard ,(empties, enemies))
                    | enemy = (HasEnemy, (empties, ChessMove True idx x : enemies))
                    | friendly = (HasFriendly, (empties, enemies))
                    | otherwise = (Empty, (ChessMove False idx x : empties, enemies))
            in (case sqState of
                Empty -> loop (apply dir x) (newEmpties, newEnemies)
                _ -> (newEmpties, newEnemies))

dirLocsCount :: Vector Char -> Int -> Dir -> Int
dirLocsCount g idx dir =
    loop (apply dir idx) 0
      where
        c = indexToColor g idx
        loop x count
            | not (onBoard x) = count
            | isEmptyGrid g x = loop (apply dir x) count+1
            | otherwise = count

-- Same as dirLocs, but for pieces that move only one square in a given "direction"
-- (aka King and Knight) -- some code intentionally duplicated with 'dirLocs'
dirLocsSingle :: Vector Char -> Int -> Color -> Dir ->([ChessMove], [ChessMove])
dirLocsSingle g idx c dir =
    let x = apply dir idx
    in
      if not (onBoard x) then ([], [])
      else if hasEnemy g c x then ([], [ChessMove True idx x])
      else if hasFriendly g c x then ([], [])
      else ([ChessMove False idx x ],[]) -- empty square

dirLocsSingleCount :: Vector Char -> Int -> Dir -> Int
dirLocsSingleCount g idx dir =
    let x = apply dir idx
    in
      if not (onBoard x) then 0
      else if isEmptyGrid g x then 1
      else 0

onBoard :: Int -> Bool
onBoard x
    | x < 10          = False
    | x > 88          = False
    | x `mod` 10 == 0 = False
    | x `mod` 10 == 9 = False
    | otherwise       = True

offBoard :: Int -> Bool
offBoard x = not $ onBoard x

destinationsToMoves :: Int -> [Int] -> [ChessMove]
destinationsToMoves _ _ = error "destinationToMoves is undefined"     -- idx dests
    --pair up idx with each index in dests to form moves

-- This should always be a list of length two
indexesToMove :: ChessNode -> [Int] -> Either String ChessMove
indexesToMove node (fromLoc : toLoc : []) =
    let g = node ^. (chessPos . cpGrid)
        cFrom = indexToColor g fromLoc
        cTo = indexToColor g toLoc
        isExch = cFrom /= Unknown && enemyColor cFrom == cTo
        removed = case isExch of
            True -> toLoc
            False -> -1
    -- in Right $ ChessMove { _isExchange = isExch
    --                      , _startIdx = fromLoc
    --                      , _endIdx = toLoc
    --                      }
        ret = Right $ ChessMove { _isExchange = isExch
                                , _startIdx = fromLoc
                                , _endIdx = toLoc }
       in if isExch then
              let str = printf "indexesToMove - isExchange is True for (%d, %d)" fromLoc toLoc
              in trace str ret
          else ret

indexesToMove _ _ = Left "IndexesToMove - expected 2 element list as input, e.g. [E2, E4]"

---------------------------------------------------------------------------------------------------
-- parse string input to move
---------------------------------------------------------------------------------------------------
parseChessMove :: ChessNode -> String -> Either String ChessMove
parseChessMove n s
    | Left err <- pMove   = Left err
    | Right x  <- pMove   = parserToChessMove n x
        where
        pMove = Parser.run s

---------------------------------------------------------------------------------------------------
-- Convert Parser's Move type to ChessMove
---------------------------------------------------------------------------------------------------
parserToChessMove :: ChessNode -> Parser.Move -> Either String ChessMove
parserToChessMove node (Parser.Move xs)
    | length xs == 2 = indexesToMove node $ fmap parserLocToInt xs
    | otherwise      = Left "parserToChessMove - expected 2 element list as input, e.g. [E2, E4]"

parserLocToInt :: Parser.Loc -> Int  -- parser ensures valid key
parserLocToInt (Parser.Loc c row) =
    let col = ord (toUpper c) - 64  -- 1 based index
    in row * 10 + col



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
