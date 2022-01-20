{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Chess
    ( Castle(..)
    , Castling(..)
    , ChessEval(..), total, details
    , ChessMove(..), exchange, startIdx, endIdx
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
    , StdMoveTestData(..)
    , allowableBishopMoves
    , allowableKnightMoves
    , allowableKingMoves
    , allowableEnPassant
    , allowablePawnNonCaptures
    , allowablePawnCaptures
    , allowableQueenMoves
    , allowableRookMoves
    , calcDevelopment
    , calcLocsForColor
    , calcMoveLists
    , calcPawnPositionScore
    , castleMoves
    , castlingAvailable
    , castlingStatus
    , checkPromote
    , cnShowMoveOnly
    , discoveredCheckNode
    , checkFinal'
    , checkMateExampleNode
    , checkMateExampleNode2
    , castlingNode
    , inCheck
    , locsForColor
    , mateInTwo01TestData
    , mateInTwo02TestData
    , moveChecksOpponent
    , pairToIndexes
    , promotion01TestData
    , showScoreDetails
    , startingBoard
    , wK, wKB, wKN, wKR, wQB, wQN, wQR
    , bK, bKB, bKN, bKR, bQB, bQN, bQR
    ) where

import Control.Lens hiding (Empty)

import Data.Char
import Data.List (foldl')
import Data.List.Extra (replace, notNull)
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mutable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Singletons
import Data.Singletons.TH
import Data.Tree
import Data.Tuple.Extra (fst3)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import System.Console.CmdArgs.Implicit (Data, Typeable)
-- import Text.Printf
-- import Debug.Trace

import qualified Parser8By8 as Parser
import Strat.Helpers
import Strat.StratTree.TreeNode
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
  deriving (Eq, Show, Ord)

data Color = Black | White | Unknown
    deriving (Show, Eq, Ord, Data, Typeable)

data Castle = QueenSide | KingSide
  deriving (Eq, Ord, Show)

data ChessMove
  = StdMove { _exchange :: Maybe Char, _startIdx :: Int, _endIdx :: Int, _stdNote :: String }
  | CastlingMove { _castle :: Castle, _kingStartIdx :: Int, _kingEndIdx :: Int
                 , _rookStartIdx :: Int, _rookEndIdx :: Int, _castleNote :: String }
  deriving (Eq, Ord)
makeLenses ''ChessMove

data UnChessMove
  = StdUnMove { unReplace :: Maybe Char, unStartIdx :: Int, unEndIdx :: Int, unStdNote :: String }
  | CastlingUnMove { unCastle :: Castle, kingUnStartIdx :: Int, kingUnEndIdx :: Int
                   , rookUnStartIdx :: Int, rookUnEndIdx :: Int, unCastleNote :: String }
  deriving (Eq, Ord)

data StdMoveTestData = StdMoveTestData
  { smtdBoardName :: String
  , colorToMoveNext :: Color
  , smtdDepth :: Int
  , smtdStartIdx :: Int
  , smtdEndIdx :: Int }
  deriving (Eq, Ord)

data ChessPos = ChessPos
  { _cpGrid :: Vector Char
  , _cpWhitePieceLocs :: [(Int, Char, Color)]
  , _cpBlackPieceLocs :: [(Int, Char, Color)]
  , _cpColor :: Color
  , _cpKingLoc :: (Int, Int)
  , _cpInCheck :: (Bool, Bool)
  , _cpFin :: FinalState }
  deriving (Show, Eq, Ord)
makeLenses ''ChessPos

data ChessEval = ChessEval {_total :: Float, _details :: String}
    deriving (Eq, Ord)
makeLenses ''ChessEval

instance Show ChessEval where
    show e = show (e ^. total)

showScoreDetails :: ChessEval -> String
showScoreDetails e =  "Total " ++ show e ++ " made up of " ++ (e ^. details)

data ChessNode = ChessNode { _chessTreeLoc :: TreeLocation, _chessMv :: ChessMove
                           , _chessVal :: ChessEval , _chessErrorVal :: ChessEval , _chessPos :: ChessPos
                           , _chessMvSeq :: [ChessMove] , _chessIsEvaluated :: Bool }
    deriving (Eq)
makeLenses ''ChessNode

instance Show ChessNode where
    -- show n = "move: " ++ show (n ^. chessMv) ++ ", value: " ++ show (n ^. chessVal)
    --          ++ ", history: " ++ show (n ^. chessMvSeq)
  show = cnShowMoveOnly

cnShowMoveOnly :: ChessNode -> String
cnShowMoveOnly cn = show (cn ^. chessMv)

evalChessNode :: ChessNode -> Float
evalChessNode cn = cn ^. (chessVal . total)

instance Z.ZipTreeNode ChessNode where
  ztnEvaluate = evalChessNode
  ztnMakeChildren = makeChildren
  ztnSign cn = colorToSign (cn ^. (chessPos . cpColor))
  ztnFinal cn = cn ^. (chessPos . cpFin) /= NotFinal

colorToSign :: Color -> Z.Sign
colorToSign White = Z.Pos
colorToSign _ = Z.Neg

toParserMove :: ChessMove -> Parser.Entry
toParserMove StdMove {..} = Parser.Move $ intToParserLoc _startIdx : [intToParserLoc _endIdx]
toParserMove CastlingMove{..} = Parser.Move $ intToParserLoc _kingStartIdx : [intToParserLoc _kingEndIdx]

instance Show ChessMove where
  show (stm@StdMove {..}) =
      let nonCaptureStr = show $ toParserMove stm
      in case _exchange of
           Just _ -> replace "-" "x" nonCaptureStr
           Nothing -> nonCaptureStr
  show cm = show $ toParserMove cm

intToParserLoc :: Int -> Parser.Loc
intToParserLoc n =
    let r = n `div` 10
        c = chr $ 64 + (n - r * 10)
    in Parser.Loc c r

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

colorToTupleElem :: Color -> (a, a) -> a
colorToTupleElem White (x, _y) = x
colorToTupleElem _ (_x, y) = y

colorToTuple :: Color -> (a, a) -> a -> (a, a)
colorToTuple White (_x, y) x' = (x', y)
colorToTuple _ (x, _y) y' = (x, y')

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

indexToColor2 :: Vector Char -> Int -> Color
indexToColor2 g idx = charToColor (g ! idx)

indexToPiece :: Vector Char -> Int -> ChessPiece (k :: SomeSing Piece)
indexToPiece g idx =
    let gridVal =  fromMaybe ' ' (g ^? ix idx)
    in charToPiece gridVal

indexToChar :: Vector Char -> Int -> Char
indexToChar g idx = fromMaybe ' ' (g ^? ix idx)

pieceVal :: ChessPiece (k :: SomeSing Piece) -> Float
pieceVal piece@(MkChessPiece c _) =
  let absVal = pieceAbsVal piece
  in case c of
    White -> absVal
    Black -> negate absVal
    Unknown -> 0.0

pieceAbsVal :: ChessPiece (k :: SomeSing Piece) -> Float
pieceAbsVal (MkChessPiece _c (SomeSing SKing)) = Z.maxValue
pieceAbsVal (MkChessPiece _c (SomeSing SQueen)) = 9.0
pieceAbsVal (MkChessPiece _c (SomeSing SRook)) = 5.0
pieceAbsVal (MkChessPiece _c (SomeSing SKnight)) = 3.0
pieceAbsVal (MkChessPiece _c (SomeSing SBishop)) = 3.0
pieceAbsVal (MkChessPiece _c (SomeSing SPawn)) = 1.0
pieceAbsVal (MkChessPiece _c (SomeSing SNone)) = 0.0
pieceAbsVal (MkChessPiece _c (SomeSing SOffBoardNone)) = 0.0

instance TreeNode ChessNode ChessMove where
    newNode = calcNewNode
    possibleMoves = legalMoves
    color = colorToInt . view (chessPos . cpColor)
    -- final = view (chessPos . cpFin)
    final = checkFinal
    parseEntry = parseChessEntry
    getMove = _chessMv
    treeLoc = _chessTreeLoc
    undoMove = undoChessMove

instance Mutable s ChessNode where

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
-- TODO: nextColorToMove should be Maybe Color, which overloads the defaults below
---------------------------------------------------------------------------------------------------
getStartNode :: String -> Color -> Tree ChessNode
getStartNode restoreGame nextColorToMove =
    let bottomColor = White -- TBD allow either color
    in case restoreGame of
      "newgame" ->
        let (wLocs, bLocs) = calcLocsForColor $ mkStartGrid bottomColor
            cPos = ChessPos { _cpGrid = mkStartGrid White, _cpColor = nextColorToMove
                        , _cpKingLoc = (15, 85)
                        , _cpInCheck = (False, False)
                        , _cpWhitePieceLocs = wLocs
                        , _cpBlackPieceLocs = bLocs
                        , _cpFin = NotFinal }
        in Node ChessNode
            { _chessTreeLoc = TreeLocation {tlDepth = 0}
            , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
            , _chessVal = ChessEval { _total = 0.0, _details = "" }
            , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
            , _chessPos = cPos
            , _chessMvSeq = []
            , _chessIsEvaluated = False } []
      "alphabeta" ->
        let (wLocs, bLocs) = calcLocsForColor alphaBetaBoard
            cPos = ChessPos
              { _cpGrid = alphaBetaBoard, _cpColor = White
              , _cpKingLoc = (11, 31)
              , _cpInCheck = (False, False)
              , _cpWhitePieceLocs = wLocs
              , _cpBlackPieceLocs = bLocs
              , _cpFin = NotFinal }
        in Node ChessNode
            { _chessTreeLoc = TreeLocation {tlDepth = 0}
            , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
            , _chessVal = ChessEval { _total = 0.0, _details = "" }
            , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
            , _chessPos = cPos
            , _chessMvSeq = []
            , _chessIsEvaluated = False } []
      "discovered" -> Node discoveredCheckNode []
      "checkmate"  -> Node checkMateExampleNode []
      "checkmate2" -> Node checkMateExampleNode2 []
      "checkmate3" -> Node checkMateExampleNode3 []
      "checkmate4" -> Node checkMateExampleNode4 []
      "mateInTwo01" -> Node mateInTwoExampleNode01 []
      "mateInTwo02" -> Node mateInTwoExampleNode02 []
      "mateInTwo02b" -> Node mateInTwoExampleNode02b []
      "promotion01" -> Node promotionNode01 []
      "debug"       -> Node debugExampleNode []
      "draw"       -> Node drawnExampleNode []
      "castling"   -> Node castlingNode []
      _ -> error "unknown restore game string - choices are:\n newgame, alphabeta, discovered, \
                 \ checkmate, checkmate2, checkmate3 ,checkmate4 ,mateInTwo01, mateInTwo02, mateInTwo02b \
                 \ promotion01 debug, draw, castling"

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
calcNewNode :: ChessNode -> ChessMove -> TreeLocation -> ChessNode
calcNewNode node mv tLoc =
    let curPos = node ^. chessPos
        curMoveSeq = node ^. chessMvSeq
        curGrid = curPos ^. cpGrid
        curColor = curPos ^. cpColor
        (newGrid, mvStartIdx, mvEndIdx) = case mv of
            StdMove _isExch start end _s -> (movePiece' curGrid start end, start, end)
            cm@CastlingMove{..} -> (castle' curGrid cm, _kingStartIdx, _kingEndIdx)
        clrFlipped = flipPieceColor curColor

        kingLocs = curPos ^. cpKingLoc
        kingLocs' = updateKingLocs newGrid kingLocs mvEndIdx

        -- after applying the current move, is the side to play next (w/color 'clrFlipped') in check?
        inCheckPair = curPos ^. cpInCheck
        isInCheck = inCheck newGrid clrFlipped (colorToTupleElem clrFlipped kingLocs')
        inCheckPair' = colorToTuple clrFlipped inCheckPair isInCheck
        (whiteLocs, blackLocs) = calcLocsForColor newGrid
        newPos = curPos { _cpGrid = newGrid
                         , _cpColor = clrFlipped
                         , _cpKingLoc = kingLocs'
                         , _cpWhitePieceLocs = whiteLocs
                         , _cpBlackPieceLocs = blackLocs
                         , _cpInCheck = inCheckPair' }
                         -- , _cpHasMoved = newHasMoved }
        (eval, finalSt) = evalPos newPos
        updatedPos = newPos { _cpFin = finalSt }
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
updateKingLocs :: Vector Char -> (Int, Int) -> Int -> (Int, Int)
updateKingLocs grid kingLocs mvEndIdx =
    case indexToPiece grid mvEndIdx of
        MkChessPiece c (SomeSing SKing) -> colorToTuple c kingLocs mvEndIdx
        MkChessPiece _c (SomeSing _) -> kingLocs

---------------------------------------------------------------------------------------------------
-- Calulate if applying the given move results in a check on the opponent's king
---------------------------------------------------------------------------------------------------
moveChecksOpponent :: ChessNode -> ChessMove -> Bool
moveChecksOpponent node mv =
  let flippedClr = flipPieceColor $ node ^. (chessPos . cpColor)
  in moveIsCheckAgainst node mv flippedClr

---------------------------------------------------------------------------------------------------
-- Calulate if applying the given move results in the moving player's king being exposed to capture
---------------------------------------------------------------------------------------------------
moveExposesKing :: ChessNode -> ChessMove -> Bool
moveExposesKing node mv =
    let pos = node ^. chessPos
    in moveExposesKing' pos mv

moveExposesKing' :: ChessPos -> ChessMove -> Bool
moveExposesKing' pos mv =
  moveIsCheckAgainst' pos mv (pos ^. cpColor)

---------------------------------------------------------------------------------------------------
-- Is the side with the given color in check after a move is applied?
---------------------------------------------------------------------------------------------------
moveIsCheckAgainst :: ChessNode -> ChessMove -> Color -> Bool
moveIsCheckAgainst node mv kingsColor =
    let curPos = node ^. chessPos
    in moveIsCheckAgainst' curPos mv kingsColor

moveIsCheckAgainst' :: ChessPos -> ChessMove -> Color -> Bool
moveIsCheckAgainst' curPos mv kingsColor =
    let curGrid = curPos ^. cpGrid
        kingLocs = curPos ^. cpKingLoc
        (newGrid, mvStartIdx, mvEndIdx) = case mv of
            StdMove _isExch start end _s -> (movePiece' curGrid start end, start, end)
            cm@CastlingMove{..} -> (castle' curGrid cm, _kingStartIdx, _kingEndIdx)
        kingLocs' = updateKingLocs newGrid kingLocs mvEndIdx
     in inCheck newGrid kingsColor (colorToTupleElem kingsColor kingLocs')

---------------------------------------------------------------------------------------------------
-- Calulate if the king at the given location is in check
---------------------------------------------------------------------------------------------------
inCheck :: Vector Char -> Color -> Int -> Bool
inCheck g c kingLoc =
   let ch = if c == White then 'K' else 'k'
       bLocs = multiMoveCaptureLocs bishopDirs g (kingLoc, ch, c)
       rLocs = multiMoveCaptureLocs rookDirs g (kingLoc, ch, c)
       nLocs = singleMoveCaptureLocs knightDirs g (kingLoc, ch, c)
       kLocs = singleMoveCaptureLocs kingDirs g (kingLoc, ch, c)
       pLocs = if c == White
         then singleMoveCaptureLocs whitePawnCaptureDirs g (kingLoc, ch, c)
         else singleMoveCaptureLocs blackPawnCaptureDirs g (kingLoc, ch, c)
       ec = flipPieceColor c

       in matchesChar g (pieceToChar Bishop ec) bLocs
          || matchesChar g (pieceToChar Rook ec) rLocs
          || matchesChar g (pieceToChar Knight ec) nLocs
          || matchesChar g (pieceToChar King ec) kLocs
          || matchesChar g (pieceToChar Pawn ec) pLocs
          || matchesChar g (pieceToChar Queen ec) (bLocs ++ rLocs)

matchesChar :: Vector Char -> Char -> [Int] -> Bool
matchesChar g ch xs = foldr f False xs
  where
    f x r = if indexToChar g x == ch then True else r

---------------------------------------------------------------------------------------------------
checkQueen
  :: Vector Char
  -> Bool
  -> ChessMove
  -> Bool
checkQueen _ True _ = True
checkQueen g False mv =
    case mv of
        StdMove _isExch start _end _s ->
            let piece = indexToPiece g start
            in case piece of
                (MkChessPiece _c (SomeSing SQueen)) -> True
                (MkChessPiece _c (SomeSing _)) -> False
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
        clr = pos ^. cpColor
    {- this turns out to be a bad idea...
         isInCheck = colorToTupleElem clr (pos ^. cpInCheck)
    in isAnExchange || isInCheck -}
    in isAnExchange

---------------------------------------------------------------------------------------------------
flipPieceColor :: Color -> Color
flipPieceColor White = Black
flipPieceColor Black = White
flipPieceColor Unknown = Unknown

{- TODOs:
-- Try running 'stan': https://hackage.haskell.org/package/stan
-- Add pawn promotion
-- Add mate in n tests
-- Determine mate in n and display
-- Add parallel processing
-- Various command line debug tools
--    undo
--    load
--    save
--    show eval detail
--    change level
--
-- And add evaluation scores for:
      outposts
      half-empty, empty file rooks
      doubled rooks
      doubled pawns
      having both bishops
      slight pref. to kingside castle over queenside
      pawn advancement
      trading (esp. queens) only if ahead
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
         finalState = checkFinal' pos
         (t, detailsStr) =
            if finalState /= NotFinal then
                let finalScore = finalStateToScore finalState
                in (finalScore, "\n\tThe position is final: " ++ show finalScore )
            else
              ( (material * 30.0 ) + mobility + (castling * 10.0)
              + (development * 5.0) + (earlyQueen * 15.0) + (pawnPositionScore * 2)
              + knightPositionScore
              ,  "\n\tMaterial (x 30.0): " ++ show (material * 30.0)
                  ++ "\n\tMobility (x 1): " ++ show mobility
                  ++ "\n\tCastling (x 10): " ++ show (castling * 10.0)
                  ++ "\n\tDevelopment (x 5): " ++ show (development * 5.0)
                  ++ "\n\tQueen dev. early (x 15): " ++ show (earlyQueen * 15.0)
                  ++ "\n\tPawn position score (x 2): " ++ show (pawnPositionScore * 2.0)
                  ++ "\n\tKnight position score (x 1): " ++ show knightPositionScore)
         eval = ChessEval { _total = t
                          , _details = detailsStr
                          }
     in (eval, finalState)

finalStateToScore :: FinalState -> Float
finalStateToScore WWins = Z.maxValue
finalStateToScore BWins = - Z.maxValue
finalStateToScore _ = 0.0

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
  in (4 - (bKnTotal + bBsTotal))

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
        wqEarly =
            -- no queen, or multiple queen endgame, dont bother with this
            if length wqs /= 1 then 0.0 else
                if fst3 (head wqs) /= wQ
                    then whiteDev cp - 4.0 else 0.0
        bqEarly =
            if length bqs /= 1 then 0.0 else
                if fst3 (head bqs) /= bQ
                    then blackDev cp - 4.0 else 0.0
    in wqEarly - bqEarly

---------------------------------------------------------------------------------------------------
-- Calculate a score for pawn positioning
--------------------------------------------------------------------------------------------------
calcPawnPositionScore :: ChessPos -> Float
calcPawnPositionScore cp =
    let g = cp ^. cpGrid
        (wLocs, bLocs) = locsForColor cp
        wPawns = filter (filterF g) (fst3 <$> wLocs)
        bPawns = filter (filterF g) (fst3 <$> bLocs)
        bPawns' = invertY <$> bPawns -- invert the black pawn pos vert. and use the white lookup
        wTotal = foldr foldF 0 wPawns
        bTotal = foldr foldF 0 bPawns'
    in wTotal - bTotal
    where
      invertY n = 10 * (9 - (n `div` 10)) + n `mod` 10
      filterF g loc =
          case indexToPiece g loc of
              MkChessPiece _c (SomeSing SPawn) -> True
              MkChessPiece _c (SomeSing _)     -> False
      foldF xLoc r = M.findWithDefault 0 xLoc pawnAdjustments + r

pawnAdjustments :: Map Int Float
pawnAdjustments = M.fromList
    -- KP, QP
    [ (24, 0.0), (34, 2.0), (44, 8.0), (25, 0.0), (35, 2.0), (45, 8.0)
    -- KRP, QRP, KBP, QBP
    , (21, 0.0), (31, -1.0), (41, -2.0), (28, 0.0), (38, -1.0), (48, -2.0)
    , (23, 0.0), (33, -1.0), (43, -2.0), (26, 0.0), (36, -1.0), (46, -2.0)
    -- KNP, QNP
    , (22, 0.0), (32, 0.0), (42, -2.0), (27, 0.0), (37, 0.0), (47, -2.0) ]

---------------------------------------------------------------------------------------------------
-- TODO: move this to the correctly commented section of this file
--------------------------------------------------------------------------------------------------
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
      f c = \(_, ch, _) -> ch == c
  in (wFiltered, bFiltered)

---------------------------------------------------------------------------------------------------
-- Count the 'material' score for the pieces on the board
--------------------------------------------------------------------------------------------------
countMaterial :: Vector Char -> Float
countMaterial = V.foldr f 0
  where
    f ch theTotal =
        if ch == empty
          then theTotal
          else theTotal + pieceVal (charToPiece ch)

---------------------------------------------------------------------------------------------------
-- Calculate the 'mobility' score for the pieces on the board
--------------------------------------------------------------------------------------------------
calcMobility :: ChessPos -> Float
calcMobility cp =
  let g = cp ^. cpGrid
      (wLocs, bLocs) = locsForColor cp
      wCnt = moveCountFromLocs cp wLocs
      bCnt = moveCountFromLocs cp bLocs
  in fromIntegral (wCnt - bCnt)

---------------------------------------------------------------------------------------------------
-- Calculate the castling score for the position
--------------------------------------------------------------------------------------------------
-- calcCastling :: ChessPos -> Float
-- calcCastling pos =
--     castlingToAbsVal (pos ^. (cpHasMoved . _1 . castlingState))
--     - castlingToAbsVal (pos ^. (cpHasMoved . _2 . castlingState))


-- TODO - Move this elsewhere
---------------------------------------------------------------------------------------------------
-- Checks each sides castling status, then adds checking for the necessary empty squares
-- between king and rook to determine which castling moves are available
--------------------------------------------------------------------------------------------------
castlingAvailable :: ChessPos -> Color -> Castling
castlingAvailable pos c =
    let (wStatus, bStatus) = castlingStatus pos
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
   let (wStatus, bStatus) = castlingStatus pos
   in castlingToAbsVal wStatus - castlingToAbsVal bStatus

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

moveCountFromLocs :: ChessPos -> [(Int, Char, Color)] -> Int
moveCountFromLocs cp =
  foldr f 0 where
    f :: (Int, Char, Color) -> Int -> Int
    f loc r =
      let cnt = moveCountFromLoc cp loc
      in cnt + r

moveCountFromLoc :: ChessPos -> (Int, Char, Color) -> Int
moveCountFromLoc cp loc@(idx, _, _) =
  let g = cp ^. cpGrid
      piece = indexToPiece g idx -- ChessPiece (k :: SomeSing Piece)
  in case piece of
      MkChessPiece _c (SomeSing SQueen) -> queenMobility cp loc
      MkChessPiece _c (SomeSing SRook) -> rookMobility g loc
      MkChessPiece _c (SomeSing SKnight) -> knightMobility g loc
      MkChessPiece _c (SomeSing SBishop) -> bishopMobility g loc
      MkChessPiece _c (SomeSing _) -> 0

---------------------------------------------------------------------------------------------------
-- Move a piece on the board, removing any captured piece.  Returns the updated node
--------------------------------------------------------------------------------------------------
movePiece :: ChessNode -> Int -> Int -> ChessNode
movePiece node pFrom pTo =
    let newGrid = movePiece' (node ^. (chessPos . cpGrid)) pFrom pTo
    in set (chessPos . cpGrid) newGrid node

---------------------------------------------------------------------------------------------------
-- Move a piece on the grid vector, removing any captured piece.  Returns the updated grid
--------------------------------------------------------------------------------------------------
movePiece' :: Vector Char -> Int -> Int -> Vector Char
movePiece' g pFrom pTo =
    let pieceChar = g ^? ix pFrom
    in case pieceChar of
        Nothing -> g
        Just ch -> let z = checkPromote ch pTo
                       p = set (ix pTo) z g
                   in removePiece' p pFrom

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

castle' :: Vector Char -> ChessMove -> Vector Char
castle' _g StdMove{} = error "This shouldn't happen..."
castle' g CastlingMove{..} =
  let gTemp = movePiece' g _kingStartIdx _kingEndIdx
  in movePiece' gTemp _rookStartIdx _rookEndIdx

removePiece' :: Vector Char -> Int -> Vector Char
removePiece' g idx = set (ix idx) empty g

-- TODO: return different moves/positions for each possible promotion choice
-- for the moment, just choosing a queen
checkPromote :: Char -> Int -> Char
checkPromote chPiece toLoc =
    let c = charToColor chPiece
        b = lastRank toLoc c
    in if b then
         case charToPiece chPiece of
            MkChessPiece White (SomeSing SPawn) -> pieceToChar Queen White
            MkChessPiece Black (SomeSing SPawn) -> pieceToChar Queen Black
            _ -> chPiece
       else chPiece

lastRank :: Int -> Color -> Bool
lastRank loc c
    | c == White
    , loc `div` 10 == 8
      = True
    | c == Black
    , loc `div` 10 == 1
      = True
    | otherwise
      = False

---------------------------------------------------------------------------------------------------
-- check for checkmate / stalemate
--------------------------------------------------------------------------------------------------
checkFinal :: ChessNode -> FinalState
checkFinal cn =
    let cp = _chessPos cn
    in checkFinal' cp

checkFinal' :: ChessPos -> FinalState
checkFinal' cp =
    let c = _cpColor cp
        inCheckStatus = colorToTupleElem c (_cpInCheck cp)
        iLose = colorToTupleElem c (BWins, WWins)
        -- ^ e.g. colorToTupleElem White (BWins, WWins) = BWins -- White loses
        mvs = legalMoves' cp
    in if notNull mvs
           then NotFinal
           else if inCheckStatus then iLose else Draw

---------------------------------------------------------------------------------------------------
-- undo move
---------------------------------------------------------------------------------------------------
undoChessMove :: ChessNode -> ChessMove -> ChessNode
undoChessMove cn mv =
    let unMove = createUndoMove mv
    in applyUndo cn unMove

-- START HERE
{-
data ChessNode = ChessNode { _chessTreeLoc :: TreeLocation, _chessMv :: ChessMove
                           , _chessVal :: ChessEval , _chessErrorVal :: ChessEval , _chessPos :: ChessPos
                           , _chessMvSeq :: [ChessMove] , _chessIsEvaluated :: Bool }
-}
applyUndo :: ChessNode -> UnChessMove -> ChessNode
applyUndo cn _unMove = cn




createUndoMove :: ChessMove -> UnChessMove
createUndoMove StdMove{..} =
    StdUnMove { unReplace = _exchange
              , unStartIdx = _endIdx
              , unEndIdx = _startIdx
              , unStdNote = "undo of: " ++ _stdNote }
createUndoMove CastlingMove{..} =
    CastlingUnMove { unCastle = _castle
                   , kingUnStartIdx = _kingEndIdx
                   , kingUnEndIdx = _kingStartIdx
                   , rookUnStartIdx = _rookEndIdx
                   , rookUnEndIdx = _rookStartIdx
                   , unCastleNote = "undo of: " ++ _castleNote }

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
        c = pos ^. cpColor
        kingLoc = colorToTupleElem c (pos ^. cpKingLoc)
        foldf mv r =
            if moveExposesKing' pos mv
               then r
               else mv : r
        in foldr foldf [] xs

---------------------------------------------------------------------------------------------------
-- get piece locations for a given color from a board
-- (pre-calculated via calcLocsForColor)
--------------------------------------------------------------------------------------------------
locsForColor :: ChessPos -> ([(Int, Char, Color)], [(Int, Char, Color)])
locsForColor cp = (_cpWhitePieceLocs cp, _cpBlackPieceLocs cp)

calcLocsForColor :: Vector Char -> ([(Int, Char, Color)], [(Int, Char, Color)])
calcLocsForColor locs =
    V.ifoldr f ([], []) locs where
        f :: Int -> Char -> ([(Int, Char, Color)], [(Int, Char, Color)])
                         -> ([(Int, Char, Color)], [(Int, Char, Color)])
        f n c (wLocs, bLocs) =
            case charToColor c of
                White -> ((n, c, White) : wLocs, bLocs)
                Black -> (wLocs, (n, c, Black) : bLocs)
                _     -> (wLocs, bLocs)


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

allowableKingMoves :: ChessPos -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableKingMoves pos loc =
    let g = pos ^. cpGrid
        castleMvs = castleMoves pos
        (empties, enemies) = allowableSingleMoves kingDirs g loc
    in (castleMvs ++ empties, enemies)

castleMoves :: ChessPos -> [ChessMove]
castleMoves pos =
  let c = pos ^. cpColor
      avail = castlingAvailable pos c
      -- (wStatus, bStatus) = castlingStatus pos
      -- castling = if c == White
      --   then wStatus
      --   else bStatus
  in case avail of
      Unavailable -> []
      Castled -> []
      KingSideOnlyAvailable -> kingSideCastlingMove pos c
      QueenSideOnlyAvailable -> queenSideCastlingMove pos c
      BothAvailable -> kingSideCastlingMove pos c
                    ++ queenSideCastlingMove pos c

-- singleton list or empty list
-- TODO: prevent Castling while in check & king moving though attacked squares
--       & make sure friendly pieces between king and rook prevent castling
kingSideCastlingMove :: ChessPos -> Color -> [ChessMove]
kingSideCastlingMove pos White =
  let g = _cpGrid pos
  in [mkCastleMove White KingSide |
        isEmpty pos 16
        && isEmpty pos 17
        && not (inCheck g White 15)
        && not (inCheck g White 16)
        && not (inCheck g White 17)]

kingSideCastlingMove pos _ =
  let g = _cpGrid pos
  in [mkCastleMove Black KingSide |
        isEmpty pos 86
        && isEmpty pos 87
        && not (inCheck g Black 85)
        && not (inCheck g Black 86)
        && not (inCheck g Black 87)]

queenSideCastlingMove :: ChessPos -> Color -> [ChessMove]
queenSideCastlingMove pos White =
  let g = _cpGrid pos
  in [mkCastleMove White QueenSide |
        isEmpty pos 14
        && isEmpty pos 13
        && isEmpty pos 12
        && not (inCheck g White 15)
        && not (inCheck g White 14)
        && not (inCheck g White 13)]

queenSideCastlingMove pos _ =
  let g = _cpGrid pos
  in [mkCastleMove Black QueenSide |
        isEmpty pos 16
        && isEmpty pos 17
        && isEmpty pos 18
        && not (inCheck g Black 15)
        && not (inCheck g Black 16)
        && not (inCheck g Black 17)]

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos = isEmptyGrid (_cpGrid pos)

isEmpty' :: ChessNode -> Int -> Bool
isEmpty' node = isEmptyGrid (_cpGrid (_chessPos node))

isEmptyGrid :: Vector Char -> Int -> Bool
isEmptyGrid g idx =  (g V.! idx) == empty

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
    let c = _cpColor pos
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
  let cp = indexToPiece g idx -- ChessPiece (k :: SomeSing Piece)
      g = _cpGrid pos
  in case cp of
      MkChessPiece _c (SomeSing SKing) -> allowableKingMoves pos loc
      MkChessPiece _c (SomeSing SQueen) -> allowableQueenMoves g loc
      MkChessPiece _c (SomeSing SRook) -> allowableRookMoves g loc
      MkChessPiece _c (SomeSing SKnight) -> allowableKnightMoves g loc
      MkChessPiece _c (SomeSing SBishop) -> allowableBishopMoves g loc
      MkChessPiece _c (SomeSing SPawn) -> allowablePawnMoves g loc
      MkChessPiece _c (SomeSing _) -> ([], [])

allowableMultiMoves :: [Dir] -> Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableMultiMoves pieceDirs g loc =
  foldl' f ([], []) pieceDirs
    where
      f :: ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f (r, r') x =
        let (freeLocs, captureLocs) = dirLocs g loc x
        in (freeLocs ++ r, captureLocs ++ r')

multiMoveMobility :: [Dir] -> Vector Char -> (Int, Char, Color) -> Int
multiMoveMobility pieceDirs g loc =
  foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsCount g loc x
        in count + r

multiMoveCaptureLocs :: [Dir] -> Vector Char -> (Int, Char, Color) -> [Int]
multiMoveCaptureLocs pieceDirs g loc =
  foldl' f [] pieceDirs
    where
      f :: [Int] -> Dir -> [Int]
      f r x =
        case dirCaptureLoc g loc x of
            Just z  -> z : r
            Nothing -> r

 -- find the destination locs for pieces that move one square in a given
-- direction (i.e., King and Knight). See @allowableMultiMoves
allowableSingleMoves :: [Dir] -> Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableSingleMoves pieceDirs g loc =
  foldl' f ([], []) pieceDirs
    where
      f :: ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f (r, r') x =
        let (freeLocs, captureLocs) = dirLocsSingle g loc x
        in (freeLocs ++ r, captureLocs ++ r')

singleMoveMobility :: [Dir] -> Vector Char -> (Int, Char, Color) -> Int
singleMoveMobility pieceDirs g (idx, _, _clr) =
  foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsSingleCount g idx x
        in count + r

singleMoveCaptureLocs :: [Dir] -> Vector Char -> (Int, Char, Color) -> [Int]
singleMoveCaptureLocs pieceDirs g loc =
  foldl' f [] pieceDirs
    where
      f :: [Int] -> Dir -> [Int]
      f r x =
        case dirCaptureLocSingle g loc x of
            Just z -> z : r
            Nothing -> r

-- find the allowable destination locs for a queen.
allowableQueenMoves :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableQueenMoves = allowableMultiMoves queenDirs

-- Queen mobility is penalized for each undeveloped minor piece - to discourage
-- bringing out the Queen too early...
queenMobility :: ChessPos -> (Int, Char, Color) -> Int
queenMobility cp loc@(_, _, clr) =
    let g = cp ^. cpGrid
        minorPieceDev = case clr of
            White -> whiteDev cp
            _ -> blackDev cp
        penalty = case minorPieceDev of
            4 -> 0
            3 -> 3
            2 -> 6
            1 -> 9
            _ -> 12
        qMobility = multiMoveMobility queenDirs g loc
    in minimum (qMobility - penalty, 0)

-- find the allowable destination locs for a rook
allowableRookMoves :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableRookMoves = allowableMultiMoves rookDirs

rookMobility :: Vector Char -> (Int, Char, Color) -> Int
rookMobility = multiMoveMobility rookDirs

-- find the allowable destination locs for a bishop
allowableBishopMoves :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableBishopMoves = allowableMultiMoves bishopDirs

bishopMobility :: Vector Char -> (Int, Char, Color) -> Int
bishopMobility = multiMoveMobility bishopDirs

-- find the possible destination locs for a knight
allowableKnightMoves :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowableKnightMoves = allowableSingleMoves knightDirs

knightMobility :: Vector Char -> (Int, Char, Color) -> Int
knightMobility = singleMoveMobility knightDirs

allowablePawnMoves :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowablePawnMoves g loc =
   let (_, enemies) = allowablePawnCaptures g loc
   in (allowablePawnNonCaptures g loc, enemies)

-- EnPassant captures are handled elsewhere and are not included here
allowablePawnCaptures :: Vector Char -> (Int, Char, Color) -> ([ChessMove], [ChessMove])
allowablePawnCaptures g loc@(_, _, clr) =
      let dirs = case clr of
              White -> whitePawnCaptureDirs
              Black -> blackPawnCaptureDirs
              Unknown -> []
          (empties, enemies) = allowableSingleMoves dirs g loc
      in (empties, enemies)

-- find the allowable destination locs for a pawn (non-capturing moves)
allowablePawnNonCaptures :: Vector Char -> (Int, Char, Color) -> [ChessMove]
allowablePawnNonCaptures g loc@(_, _, clr) =
    let hasMoved = hasPawnMoved loc
    in case clr of
        Unknown -> []
        White -> pawnMoves g whitePawnDir hasMoved loc
        Black -> pawnMoves g blackPawnDir hasMoved loc

pawnMoves :: Vector Char -> Dir -> Bool -> (Int, Char, Color) -> [ChessMove]
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
hasPawnMoved (_, _, Unknown) = False
hasPawnMoved (idx, _, White) = idx > 28
hasPawnMoved (idx, _, Black) = idx < 71

-- TODO: not currently called
-- find the allowable enPassant destination capture locs for a pawn
-- only enemy squares containing pawns are returned
allowableEnPassant :: Vector Char -> (Int, Char, Color) -> [ChessMove]
allowableEnPassant = enPassantCaptures

enPassantCaptures :: Vector Char -> (Int, Char, Color) -> [ChessMove]
enPassantCaptures g loc@(idx, _, clr) =
    if hasPawnMoved loc then []
    else
      let dirs = case clr of
            White -> whitePawnEnPassantDirs
            Black -> blackPawnEnPassantDirs
            Unknown -> []
          enemies = snd $ allowableSingleMoves dirs g loc
          thisPawn = indexToChar g idx
      -- only pawns can be captured this way
      in filter (\n -> indexToChar g (_endIdx n) == flipCharColor thisPawn) enemies

data SquareState = Empty | HasFriendly | HasEnemy | OffBoard
   deriving Show

dirLocs :: Vector Char -> (Int, Char, Color) -> Dir ->([ChessMove], [ChessMove])
dirLocs g (idx0, _, c0) dir =
    loop (apply dir idx0) ([], [])
      where
        loop idx (empties, enemies) =
            let cx = indexToColor2 g idx
                ob = onBoard idx
                friendly = ob && c0 == cx
                enemy = ob && (enemyColor c0) == cx
                (sqState, (newEmpties, newEnemies))
                    | not ob = (OffBoard ,(empties, enemies))
                    | enemy = (HasEnemy, (empties, StdMove (Just (indexToChar g idx)) idx0 idx "": enemies))
                    | friendly = (HasFriendly, (empties, enemies))
                    | otherwise = (Empty, (StdMove Nothing idx0 idx "": empties, enemies))
            in (case sqState of
                Empty -> loop (apply dir idx) (newEmpties, newEnemies)
                _ -> (newEmpties, newEnemies))



dirCaptureLoc :: Vector Char -> (Int, Char, Color) -> Dir -> Maybe Int
dirCaptureLoc g (idx0, _, clr0) dir =
   let clr0Enemy = enemyColor clr0
       loop idx =
           let clr = indexToColor2 g idx
           -- case hasEnemy g clr idx of
           in case clr == clr0Enemy of
               True -> Just idx
               False | not (onBoard idx) -> Nothing
                     | isEmptyGrid g idx -> loop (apply dir idx)
                     | otherwise -> Nothing
   in loop (apply dir idx0)

dirLocsCount :: Vector Char -> (Int, Char, Color) -> Dir -> Int
dirLocsCount g (idx, _, _) dir =
    loop (apply dir idx) 0
      where
        loop x count
            | not (onBoard x) = count
            | isEmptyGrid g x = loop (apply dir x) count+1
            | otherwise = count

-- Same as dirLocs, but for pieces that move only one square in a given "direction"
-- (aka King and Knight) -- some code intentionally duplicated with 'dirLocs'
dirLocsSingle :: Vector Char -> (Int, Char, Color) -> Dir ->([ChessMove], [ChessMove])
dirLocsSingle g (idx0, _, c0) dir =
    let x = apply dir idx0
        cx = indexToColor2 g x
        cEnemy0 = enemyColor c0
    in
      if not (onBoard x) then ([], [])
      else if cx == cEnemy0 then ([], [StdMove (Just (indexToChar g x)) idx0 x ""])
      else if cx == c0 then ([], [])
      else ([StdMove Nothing idx0 x ""],[]) -- empty square

dirLocsSingleCount :: Vector Char -> Int -> Dir -> Int
dirLocsSingleCount g idx dir =
    let x = apply dir idx
    in
      if not (onBoard x) then 0
      else if isEmptyGrid g x then 1
      else 0

dirCaptureLocSingle :: Vector Char -> (Int, Char, Color) -> Dir -> Maybe Int
dirCaptureLocSingle g (idx, _, clr) dir =
    let enemyClr = enemyColor clr
    in case apply dir idx of
        x | not (onBoard x) -> Nothing
          | indexToColor2 g x == enemyClr -> Just x
          | otherwise -> Nothing

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

----------------------------------------------------------------------------------------------------
{-
 Convert a list of to/from move indexes to a ChessMove
 Assumptions:
 1)  This should always be a list of length two
 2)  the move is legal, e.g., any piece found at the destination
     square of the move must be an opponent's piece being captured.
-}
----------------------------------------------------------------------------------------------------
indexesToMove :: ChessNode -> [Int] -> Either String ChessMove
indexesToMove node [fromLoc, toLoc] =
    case checkIndexesToCastle [fromLoc, toLoc] of
        Just cm -> Right cm
        Nothing ->
            let g = node ^. (chessPos . cpGrid)
                ch = indexToChar g toLoc
                exch = if ch == empty
                  then Nothing
                  else Just ch
                removed = case exch of
                    Nothing -> -1
                    Just _ -> toLoc
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

---------------------------------------------------------------------------------------------------
-- Pieces with tracked "have they moved yet" state
---------------------------------------------------------------------------------------------------
-- starting locs...
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

-- starting locs...
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
parseChessEntry :: ChessNode -> String -> Either String (Entry ChessMove s)
parseChessEntry n s
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
    | otherwise      = Left "parserToChessMove - expected 2 element list as input, e.g. [E2, E4]"
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
alphaBetaBoard :: V.Vector Char
alphaBetaBoard = V.fromList
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

alphaBetaBoard2 :: V.Vector Char
alphaBetaBoard2 = V.fromList
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

discoveredCheckBoard :: V.Vector Char
discoveredCheckBoard = V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  'K',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'B',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'b',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'r',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-- Moves to verify:
-- (W) E5-C5 (discovered) check
-- (b) D6 x C5 (discovered) check
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
-}
checkMateExampleBoard :: V.Vector Char
checkMateExampleBoard = V.fromList
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

checkMateExampleBoard2 :: V.Vector Char
checkMateExampleBoard2 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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




checkMateExampleBoard3 :: V.Vector Char
checkMateExampleBoard3 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

checkMateExampleBoard4 :: V.Vector Char
checkMateExampleBoard4 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

mateInTwoBoard01:: V.Vector Char
mateInTwoBoard01 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  'K',  'B',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  'P',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  'r',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

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
    , colorToMoveNext = Black
    , smtdDepth = 4
    , smtdStartIdx = 81
    , smtdEndIdx = 31 }

mateInTwoBoard02:: V.Vector Char
mateInTwoBoard02 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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
    , colorToMoveNext = Black
    , smtdDepth = 4
    , smtdStartIdx = 42
    , smtdEndIdx = 32 }

mateInTwoBoard02b:: V.Vector Char
mateInTwoBoard02b = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

----------------------------------------------------------------------------------------------------
-- Boards to check pawn promotion
----------------------------------------------------------------------------------------------------
promotionBoard01 :: V.Vector Char
promotionBoard01 = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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
    , colorToMoveNext = White
    , smtdDepth = 4
    , smtdStartIdx = 74
    , smtdEndIdx = 84 }


----------------------------------------------------------------------------------------------------
-- Board to check handling of drawn game
----------------------------------------------------------------------------------------------------
drawnBoard :: V.Vector Char
drawnBoard = V.fromList
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
-- Board for debugging
----------------------------------------------------------------------------------------------------
debugBoard :: V.Vector Char
debugBoard = V.fromList
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

debugBoard_2 :: V.Vector Char
debugBoard_2 = V.fromList
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




castlingBoard :: V.Vector Char
castlingBoard = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

--TODO: get rid of these castling states...
discoveredCheckNode :: ChessNode
discoveredCheckNode = preCastlingGameNode discoveredCheckBoard White (14, 64)

checkMateExampleNode :: ChessNode
checkMateExampleNode = preCastlingGameNode checkMateExampleBoard White (15, 85)

checkMateExampleNode2 :: ChessNode
checkMateExampleNode2 = preCastlingGameNode checkMateExampleBoard2 White (15, 85)

checkMateExampleNode3 :: ChessNode
checkMateExampleNode3 = preCastlingGameNode checkMateExampleBoard3 White (15, 87)

checkMateExampleNode4 :: ChessNode
checkMateExampleNode4 = preCastlingGameNode checkMateExampleBoard4 White (15, 87)

mateInTwoExampleNode01 :: ChessNode
mateInTwoExampleNode01 = preCastlingGameNode mateInTwoBoard01 Black (11, 13)

mateInTwoExampleNode02 :: ChessNode
mateInTwoExampleNode02 = preCastlingGameNode mateInTwoBoard02 Black (21, 23)

mateInTwoExampleNode02b :: ChessNode
mateInTwoExampleNode02b = preCastlingGameNode mateInTwoBoard02b Black (11, 23)

promotionNode01 :: ChessNode
promotionNode01 = postCastlingGameNode promotionBoard01 White (16, 86)

drawnExampleNode :: ChessNode
drawnExampleNode = preCastlingGameNode drawnBoard White (68, 88)

debugExampleNode :: ChessNode
debugExampleNode = preCastlingGameNode debugBoard White (15, 85)

castlingNode :: ChessNode
castlingNode = preCastlingGameNode castlingBoard Black (27, 85)

-- White has K and Q side castling available, Black has King side only
preCastlingGameNode :: Vector Char -> Color -> (Int, Int) -> ChessNode
preCastlingGameNode grid the_color kingLocs =
    let (wLocs, bLocs) = calcLocsForColor grid
        cPos = ChessPos
          { _cpGrid = grid
          , _cpColor = the_color
          , _cpKingLoc = kingLocs
          , _cpInCheck = (False, False)
          , _cpWhitePieceLocs = wLocs
          , _cpBlackPieceLocs = bLocs
          , _cpFin = NotFinal }
    in ChessNode
        { _chessTreeLoc = TreeLocation {tlDepth = 0}
        , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
        , _chessVal = ChessEval { _total = 0.0, _details = "" }
        , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
        , _chessPos = cPos
        , _chessMvSeq = []
        , _chessIsEvaluated = False }

-- No castling available
postCastlingGameNode :: Vector Char -> Color -> (Int, Int) -> ChessNode
postCastlingGameNode grid the_color kingLocs =
    let (wLocs, bLocs) = calcLocsForColor grid
        cPos = ChessPos
          { _cpGrid = grid
          , _cpColor = the_color
          , _cpKingLoc = kingLocs
          , _cpInCheck = (False, False)
          , _cpWhitePieceLocs = wLocs
          , _cpBlackPieceLocs = bLocs
          , _cpFin = NotFinal }
    in ChessNode
        { _chessTreeLoc = TreeLocation {tlDepth = 0}
        , _chessMv = StdMove {_exchange = Nothing, _startIdx = -1, _endIdx = -1, _stdNote = ""}
        , _chessVal = ChessEval { _total = 0.0, _details = "" }
        , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
        , _chessPos = cPos
        , _chessMvSeq = []
        , _chessIsEvaluated = False }
