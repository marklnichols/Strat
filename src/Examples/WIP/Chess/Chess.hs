{-# LANGUAGE BangPatterns #-}
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
    , ChessMove(..), isExchange, startIdx, endIdx
    , ChessMoves(..), cmEmpty, cmEnemy
    , ChessNode(..), chessMv, chessVal, chessErrorVal, chessPos
    , ChessPos(..), cpGrid, cpColor, cpFin
    , Color(..)
    , HasMoved(..)
    , colorFromInt
    , colorToInt
    , countMaterial
    , getStartNode
    , startingHasMovedBlack
    , startingHasMovedWhite
    , toParserMove
    -- exported for testing only
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
    , castleMoves
    , checkCastling
    , cnShowMoveOnly
    , discoveredCheckNode
    , checkMateExampleNode
    , checkMateExampleNode2
    , castlingNode
    , inCheck
    , locsForColor
    , moveChecksOpponent
    , pairToIndexes
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
import Data.Set (Set, member, notMember)
import qualified Data.Set as S
import Data.Singletons
import Data.Singletons.TH
import Data.Tree
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
-- import Text.Printf
-- import Debug.Trace

import qualified CkParser as Parser
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

data HasMoved = HasMoved
  { _unMovedDev :: Set Int
  , _unMovedCenterPawns :: Set Int
  , _unMovedCastling :: Set Int
  , _unMovedQueen :: Bool

  , _castlingState :: Castling }
  deriving (Show, Eq, Ord)

makeLenses ''HasMoved

data Color = Black | White | Unknown
    deriving (Show, Eq, Ord)

data Castle = QueenSide | KingSide
  deriving (Eq, Ord, Show)

data ChessMove
  = StdMove { _isExchange :: Bool, _startIdx :: Int, _endIdx :: Int, _stdNote :: String }
  | CastlingMove { _castle :: Castle, _kingStartIdx :: Int, _kingEndIdx :: Int
                 , _rookStartIdx :: Int, _rookEndIdx :: Int, _castleNote :: String }
  deriving (Eq, Ord)
makeLenses ''ChessMove

data ChessPos = ChessPos
  { _cpGrid :: Vector Char
  , _cpWhitePieceLocs :: [Int]
  , _cpBlackPieceLocs :: [Int]
  , _cpColor :: Color
  , _cpHasMoved :: (HasMoved, HasMoved) -- (white value, black value) for these pairs...
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

data ChessNode = ChessNode { _chessId :: Int, _chessTreeLoc :: TreeLocation, _chessMv :: ChessMove
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

toParserMove :: ChessMove -> Parser.Move
toParserMove StdMove {..} = Parser.Move $ intToParserLoc _startIdx : [intToParserLoc _endIdx]
toParserMove CastlingMove{..} = Parser.Move $ intToParserLoc _kingStartIdx : [intToParserLoc _kingEndIdx]

instance Show ChessMove where
  show (stm@StdMove {..}) =
      let nonCaptureStr = show $ toParserMove stm
      in if _isExchange
           then replace "-" "x" nonCaptureStr
           else nonCaptureStr
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
    parseMove = parseChessMove
    getMove = _chessMv
    nodeId = _chessId
    treeLoc = _chessTreeLoc

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
-- TODO: let this be either color
---------------------------------------------------------------------------------------------------
getStartNode :: String -> Tree ChessNode
getStartNode restoreGame =
    case restoreGame of
      "newgame" ->
        let nextColor = White
            (wLocs, bLocs) = calcLocsForColor $ mkStartGrid White
            cPos = ChessPos { _cpGrid = mkStartGrid White, _cpColor = nextColor
                        , _cpHasMoved = (startingHasMovedWhite, startingHasMovedBlack)
                        , _cpKingLoc = (15, 85)
                        , _cpInCheck = (False, False)
                        , _cpWhitePieceLocs = wLocs
                        , _cpBlackPieceLocs = bLocs
                        , _cpFin = NotFinal }
        in Node ChessNode
            { _chessId = 0
            , _chessTreeLoc = TreeLocation {tlDepth = 0, tlIndexForDepth  = 0}
            , _chessMv = StdMove {_isExchange = False, _startIdx = -1, _endIdx = -1, _stdNote = ""}
            , _chessVal = ChessEval { _total = 0.0, _details = "" }
            , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
            , _chessPos = cPos
            , _chessMvSeq = []
            , _chessIsEvaluated = False } []
      "alphabeta" ->
        let (wLocs, bLocs) = calcLocsForColor alphaBetaBoard
            cPos = ChessPos
              { _cpGrid = alphaBetaBoard, _cpColor = White
              , _cpHasMoved = (abHasMovedWhite, abHasMovedBlack)
              , _cpKingLoc = (11, 31)
              , _cpInCheck = (False, False)
              , _cpWhitePieceLocs = wLocs
              , _cpBlackPieceLocs = bLocs
              , _cpFin = NotFinal }
        in Node ChessNode
            { _chessId = 0
            , _chessTreeLoc = TreeLocation {tlDepth = 0, tlIndexForDepth  = 0}
            , _chessMv = StdMove {_isExchange = False, _startIdx = -1, _endIdx = -1, _stdNote = ""}
            , _chessVal = ChessEval { _total = 0.0, _details = "" }
            , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
            , _chessPos = cPos
            , _chessMvSeq = []
            , _chessIsEvaluated = False } []
          where
            abHasMovedWhite = HasMoved
                { _unMovedDev = S.empty
                , _unMovedCenterPawns = S.empty
                , _unMovedCastling = S.empty
                , _castlingState = Castled
                , _unMovedQueen = False }
            abHasMovedBlack = HasMoved
                { _unMovedDev = S.empty
                , _unMovedCenterPawns = S.empty
                , _unMovedCastling = S.empty
                , _castlingState = Castled
                , _unMovedQueen = False }
      "discovered" -> Node discoveredCheckNode []
      "checkmate"  -> Node checkMateExampleNode []
      "checkmate2" -> Node checkMateExampleNode2 []
      "debug"       -> Node debugExampleNode []
      "draw"       -> Node drawnExampleNode []
      "castling"   -> Node castlingNode []
      _ -> error "unknown restore game string - choices are:\n newgame, alphabeta, discovered, \
                 \ checkmate, castling"

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
calcNewNode node mv tLoc@TreeLocation{..} =
    let newId = tlDepth * 1000 + tlIndexForDepth
        curPos = node ^. chessPos
        curMoveSeq = node ^. chessMvSeq
        curGrid = curPos ^. cpGrid
        curColor = curPos ^. cpColor
        curHasMoved = _cpHasMoved curPos
        newHasMoved = checkHasMoved curColor curGrid curHasMoved mv
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
                         , _cpInCheck = inCheckPair'
                         , _cpHasMoved = newHasMoved }
        (eval, finalSt) = evalPos newPos
        updatedPos = newPos { _cpFin = finalSt }
    in ChessNode
        { _chessId = newId
        , _chessTreeLoc = tLoc
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
   let bLocs = multiMoveCaptureLocs bishopDirs g kingLoc
       rLocs = multiMoveCaptureLocs rookDirs g kingLoc
       nLocs = singleMoveCaptureLocs knightDirs g kingLoc
       kLocs = singleMoveCaptureLocs kingDirs g kingLoc
       pLocs = case c of
         White -> singleMoveCaptureLocs whitePawnCaptureDirs g kingLoc
         _ -> singleMoveCaptureLocs blackPawnCaptureDirs g kingLoc
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
checkHasMoved
  :: Color
  -> Vector Char
  -> (HasMoved, HasMoved)
  -> ChessMove
  -> (HasMoved, HasMoved)
checkHasMoved c g (w, b) mv =
  let prevHasMoved = case c of
        White -> w
        _ ->     b
      (prevUnCastl , prevStateCastl) = (_unMovedCastling prevHasMoved, _castlingState prevHasMoved)
      (newUnCastl, newStateCastl) =
        case checkCastling c g prevHasMoved mv of
            Nothing -> (prevUnCastl, prevStateCastl)
            Just (u, s) -> (u, s)
      prevUnDev = _unMovedDev prevHasMoved
      newUnDev = fromMaybe prevUnDev $ checkDevelopment g prevHasMoved mv

      prevUnPawns = _unMovedCenterPawns prevHasMoved
      newUnPawns = fromMaybe prevUnPawns $ case c of
          White -> checkCenterPawns g prevHasMoved mv wKP wQP
          _ -> checkCenterPawns g prevHasMoved mv bKP bQP

      prevUnQueen = _unMovedQueen prevHasMoved
      newUnQueen = checkQueen g prevUnQueen mv

      newHasMoved = HasMoved
        { _unMovedDev = newUnDev
        , _unMovedCenterPawns = newUnPawns
        , _unMovedCastling = newUnCastl
        , _castlingState = newStateCastl
        , _unMovedQueen = newUnQueen }
  in case c of
    White -> (newHasMoved, b)
    _     -> (w, newHasMoved)

checkDevelopment
  :: Vector Char
  -> HasMoved
  -> ChessMove
  -> Maybe (Set Int)
checkDevelopment g prevHasMoved mv =
    case mv of
        StdMove _isExch start _end _s ->
            let piece = indexToPiece g start
            in case piece of
                (MkChessPiece _c (SomeSing SBishop)) -> Just $ updateDevelopment prevHasMoved start
                (MkChessPiece _c (SomeSing SKnight)) -> Just $ updateDevelopment prevHasMoved start
                (MkChessPiece _c (SomeSing _)) -> Nothing
        CastlingMove{}  -> Nothing

checkCenterPawns
  :: Vector Char
  -> HasMoved
  -> ChessMove
  -> Int
  -> Int
  -> Maybe (Set Int)
checkCenterPawns g prevHasMoved mv kpLoc qpLoc =
    case mv of
        StdMove _isExch start _end _s ->
            let piece = indexToPiece g start
            in case piece of
                (MkChessPiece _c (SomeSing SPawn)) ->
                    if start == kpLoc || start == qpLoc
                       then Just $ updateCenterPawns prevHasMoved start
                       else Nothing
                (MkChessPiece _c (SomeSing _)) -> Nothing
        CastlingMove{}  -> Nothing

updateDevelopment
  :: HasMoved
  -> Int
  -> Set Int
updateDevelopment prevHasMoved start =
  let prevSet = _unMovedDev prevHasMoved
  in S.delete start prevSet

updateCenterPawns
  :: HasMoved
  -> Int
  -> Set Int
updateCenterPawns prevHasMoved start =
  let prevSet = _unMovedCenterPawns prevHasMoved
  in S.delete start prevSet

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

checkCastling
  :: Color
  -> Vector Char
  -> HasMoved
  -> ChessMove
  -> Maybe (Set Int, Castling)
checkCastling c g prevHasMoved mv =
    case mv of
      StdMove _isExch start _end _s ->
        let piece = indexToPiece g start
        in case piece of
          (MkChessPiece _c (SomeSing SKing)) -> Just $ updateCastling c prevHasMoved mv
          (MkChessPiece _c (SomeSing SRook)) -> Just $ updateCastling c prevHasMoved mv
          (MkChessPiece _c (SomeSing _)) -> Nothing
      CastlingMove{} -> Just $ updateCastling c prevHasMoved mv

updateCastling
  :: Color
  -> HasMoved
  -> ChessMove
  -> (Set Int, Castling)
updateCastling c prevHasMoved mv =
    case mv of
        CastlingMove {} -> (S.empty, Castled)
        StdMove _isExch start _end _s ->
            let unMovedC = _unMovedCastling prevHasMoved
                cState = _castlingState prevHasMoved
            in if cState == Castled
                then (_unMovedDev prevHasMoved, _castlingState prevHasMoved)
                else
                    let newSet = S.delete start unMovedC
                        (k, kr, qr) = if c == White
                            then (wK, wKR, wQR)
                            else (bK, bKR, bQR)
                        newState = case newSet of
                            s | k `notMember` s ->  Unavailable  -- king has moved
                              | qr `notMember` s
                              , kr `member` s -> KingSideOnlyAvailable -- k and kr haven't moved
                              | kr `notMember` s
                              , qr `member` s -> QueenSideOnlyAvailable -- k and qr haven't moved
                              | kr `notMember` s
                              , qr `notMember` s -> Unavailable -- kr and qr have moved
                              | otherwise -> BothAvailable -- k, kr, and qr all have not moved
                    in (newSet , newState)

isCritical :: ChessNode -> Bool
isCritical cn =
    let isAnExchange =
          case _chessMv cn of
              StdMove{..} -> _isExchange
              _           -> False
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
-- Determine mate in n
-- Add parallel processing
-- Various command line debug tools
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
     let finalState = checkFinal' pos
         finalScore =
           if finalState == NotFinal
             then 0
             else
               let colorMult = fromIntegral (colorToInt (_cpColor pos)) :: Float
               in Z.maxValue * colorMult
         material = countMaterial (pos ^. cpGrid)
         mobility = calcMobility pos
         castling = calcCastling pos
         development = calcDevelopment pos
         earlyQueen = calcEarlyQueen pos
         pawnPositionScore = calcPawnPositionScore pos
         knightPositionScore = calcKnightPositionScore pos
         t = (material * 30.0 ) + mobility + (castling * 10.0)
           + (development * 5.0) + (earlyQueen * 15.0) + (pawnPositionScore * 2)
           + knightPositionScore + finalScore
         detailsStr = "\n\tMaterial (x 30.0): " ++ show (material * 30.0)
                ++ "\n\tMobility (x 1): " ++ show mobility
                ++ "\n\tCastling (x 10): " ++ show (castling * 10.0)
                ++ "\n\tDevelopment (x 5): " ++ show (development * 5.0)
                ++ "\n\tQueen dev. early (x 15): " ++ show (earlyQueen * 15.0)
                ++ "\n\tPawn position score (x 2): " ++ show (pawnPositionScore * 2.0)
                ++ "\n\tKnight position score (x 1): " ++ show knightPositionScore
                ++ "\n\tThe position is final (x 1): " ++ show finalScore
         eval = ChessEval { _total = t
                          , _details = detailsStr
                          }
     in (eval, finalState)

---------------------------------------------------------------------------------------------------
-- Calculate the 'development' score for the position (White - Black)
--------------------------------------------------------------------------------------------------
calcDevelopment :: ChessPos -> Float
calcDevelopment cp = whiteDev cp - blackDev cp

whiteDev :: ChessPos -> Float
whiteDev cp = 4.0 - (fromIntegral $ S.size (cp ^. (cpHasMoved . _1 . unMovedDev))) :: Float

blackDev :: ChessPos -> Float
blackDev cp = 4.0 - (fromIntegral $ S.size (cp ^. (cpHasMoved . _2 . unMovedDev))) :: Float

---------------------------------------------------------------------------------------------------
-- Calculate a penalty for developing the queen prematurely (White - Black)
--------------------------------------------------------------------------------------------------
calcEarlyQueen :: ChessPos -> Float
calcEarlyQueen cp =
    let wqMoved = cp ^. (cpHasMoved . _1 . unMovedQueen)
        bqMoved = cp ^. (cpHasMoved . _2 . unMovedQueen)
        wqEarly = if wqMoved then whiteDev cp - 4.0
                  else 0
        bqEarly = if bqMoved then blackDev cp - 4.0
                  else 0
    in wqEarly - bqEarly;

---------------------------------------------------------------------------------------------------
-- Calculate a score for pawn positioning
--------------------------------------------------------------------------------------------------
calcPawnPositionScore :: ChessPos -> Float
calcPawnPositionScore cp =
    let g = cp ^. cpGrid
        (wLocs, bLocs) = locsForColor cp
        wPawns = filter (filterF g) wLocs
        bPawns = filter (filterF g) bLocs
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
      foldF x r = M.findWithDefault 0 x pawnAdjustments + r

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
-- Calculate a score for Knight positioning
--------------------------------------------------------------------------------------------------
calcKnightPositionScore :: ChessPos -> Float
calcKnightPositionScore cp =
    let g = cp ^. cpGrid
        (wLocs, bLocs) = locsForColor cp
        wKnights = filter (filterF g) wLocs
        bKnights = filter (filterF g) bLocs
        wTotal = foldr foldF 0 wKnights
        bTotal = foldr foldF 0 bKnights
   in wTotal - bTotal
     where
       filterF g loc =
           case indexToPiece g loc of
              MkChessPiece _c (SomeSing SKnight) -> True
              MkChessPiece _c (SomeSing _)     -> False
       foldF x r = M.findWithDefault 0 x knightAdjustments + r

knightAdjustments :: Map Int Float
knightAdjustments = M.fromList
    [ (23, 4.0), (32, 4.0), (52, 4.0), (63, 4.0), (65, 4.0), (56, 4.0), (36, 4.0), (25, 4.0)
    , (24, 4.0), (33, 4.0), (53, 4.0), (64, 4.0), (66, 4.0), (57, 4.0), (37, 4.0), (26, 4.0)
    , (33, 4.0), (42, 4.0), (62, 4.0), (73, 4.0), (75, 4.0), (66, 4.0), (46, 4.0), (35, 4.0)
    , (34, 4.0), (43, 4.0), (63, 4.0), (74, 4.0), (76, 4.0), (67, 4.0), (47, 4.0), (36, 4.0) ]

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
calcCastling :: ChessPos -> Float
calcCastling pos =
    castlingToAbsVal (pos ^. (cpHasMoved . _1 . castlingState))
    - castlingToAbsVal (pos ^. (cpHasMoved . _2 . castlingState))

castlingToAbsVal :: Castling -> Float
castlingToAbsVal Castled = 3.0
castlingToAbsVal Unavailable = -3.0
castlingToAbsVal BothAvailable = 0.0
castlingToAbsVal KingSideOnlyAvailable = -1.0
castlingToAbsVal QueenSideOnlyAvailable = -1.0

moveCountFromLocs :: ChessPos -> [Int] -> Int
moveCountFromLocs cp =
  foldr f 0 where
    f :: Int -> Int -> Int
    f loc r =
      let cnt = moveCountFromLoc cp loc
      in cnt + r

moveCountFromLoc :: ChessPos -> Int -> Int
moveCountFromLoc cp loc =
  let g = cp ^. cpGrid
      piece = indexToPiece g loc -- ChessPiece (k :: SomeSing Piece)
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
        Just ch -> let z = checkPromote g ch pTo
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

-- TODO: currently not implemented
checkPromote :: Vector Char -> Char -> Int -> Char
checkPromote _g chPiece _toLoc = chPiece

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
locsForColor :: ChessPos -> ([Int], [Int])
locsForColor cp = (_cpWhitePieceLocs cp, _cpBlackPieceLocs cp)

calcLocsForColor :: Vector Char -> ([Int], [Int])
calcLocsForColor locs =
    V.ifoldr f ([], []) locs where
        f :: Int -> Char -> ([Int], [Int]) -> ([Int], [Int])
        f n c (wLocs, bLocs) =
            case charToColor c of
                White -> (n : wLocs, bLocs)
                Black -> (wLocs, n : bLocs)
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

allowableKingMoves :: ChessPos -> Int -> ([ChessMove], [ChessMove])
allowableKingMoves pos loc =
    let g = pos ^. cpGrid
        castleMvs = castleMoves pos
        (empties, enemies) = allowableSingleMoves kingDirs g loc
    in (castleMvs ++ empties, enemies)

castleMoves :: ChessPos -> [ChessMove]
castleMoves pos =
  let c = pos ^. cpColor
      castling = if c == White
        then pos ^. (cpHasMoved . _1 . castlingState )
        else pos ^. (cpHasMoved . _2 . castlingState )
  in case castling of
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

hasFriendly :: Vector Char -> Color -> Int -> Bool
hasFriendly g c idx = (onBoard idx) && indexToColor g idx == c

hasEnemy :: Vector Char -> Color -> Int -> Bool
hasEnemy g c idx = (onBoard idx) &&  indexToColor g idx == enemyColor c

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

movesFromLocs :: ChessPos -> [Int] -> ([ChessMove], [ChessMove])
movesFromLocs pos =
  let f :: Int -> ([ChessMove], [ChessMove]) -> ([ChessMove], [ChessMove])
      f loc (r1, r2) =
        let (xs, ys) = movesFromLoc pos loc
        in (xs ++ r1, ys ++ r2)
  in foldr f ([], [])

movesFromLoc :: ChessPos -> Int -> ([ChessMove], [ChessMove])
movesFromLoc pos loc =
  let cp = indexToPiece g loc -- ChessPiece (k :: SomeSing Piece)
      g = _cpGrid pos
  in case cp of
      MkChessPiece _c (SomeSing SKing) -> allowableKingMoves pos loc
      MkChessPiece _c (SomeSing SQueen) -> allowableQueenMoves g loc
      MkChessPiece _c (SomeSing SRook) -> allowableRookMoves g loc
      MkChessPiece _c (SomeSing SKnight) -> allowableKnightMoves g loc
      MkChessPiece _c (SomeSing SBishop) -> allowableBishopMoves g loc
      MkChessPiece _c (SomeSing SPawn) -> allowablePawnMoves g loc
      MkChessPiece _c (SomeSing _) -> ([], [])

allowableMultiMoves :: [Dir] -> Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableMultiMoves pieceDirs g idx =
  foldl' f ([], []) pieceDirs
    where
      f :: ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f (r, r') x =
        let (freeLocs, captureLocs) = dirLocs g idx x
        in (freeLocs ++ r, captureLocs ++ r')

multiMoveMobility :: [Dir] -> Vector Char -> Int -> Int
multiMoveMobility pieceDirs g idx =
  foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsCount g idx x
        in count + r

multiMoveCaptureLocs :: [Dir] -> Vector Char -> Int -> [Int]
multiMoveCaptureLocs pieceDirs g idx =
  foldl' f [] pieceDirs
    where
      f :: [Int] -> Dir -> [Int]
      f r x =
        case dirCaptureLoc g idx x of
            Just z  -> z : r
            Nothing -> r

 -- find the destination locs for pieces that move one square in a given
-- direction (i.e., King and Knight). See @allowableMultiMoves
allowableSingleMoves :: [Dir] -> Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableSingleMoves pieceDirs g idx =
  foldl' (f (indexToColor g idx)) ([], []) pieceDirs
    where
      -- fold function: curried f with Color applied
      f :: Color -> ([ChessMove], [ChessMove]) -> Dir -> ([ChessMove], [ChessMove])
      f c (r, r') x =
        let (freeLocs, captureLocs) = dirLocsSingle g idx c x
        in (freeLocs ++ r, captureLocs ++ r')

singleMoveMobility :: [Dir] -> Vector Char -> Int -> Int
singleMoveMobility pieceDirs g idx =
  foldl' f 0 pieceDirs
    where
      f :: Int -> Dir -> Int
      f r x =
        let count = dirLocsSingleCount g idx x
        in count + r

-- This needs optimizing (4.2)
singleMoveCaptureLocs :: [Dir] -> Vector Char -> Int -> [Int]
singleMoveCaptureLocs pieceDirs g idx =
  foldl' (f (indexToColor g idx)) [] pieceDirs
    where
      -- fold function: curried f with Color applied
      f :: Color -> [Int] -> Dir -> [Int]
      f c r x =
        case dirCaptureLocSingle g idx c x of
            Just z -> z : r
            Nothing -> r

-- find the allowable destination locs for a queen.
allowableQueenMoves :: Vector Char -> Int -> ([ChessMove], [ChessMove])
allowableQueenMoves = allowableMultiMoves queenDirs

-- Queen mobility is penalized for each undeveloped minor piece - to discourage
-- bringing out the Queen too early...
queenMobility :: ChessPos -> Int -> Int
queenMobility cp idx =
    let g = cp ^. cpGrid
        minorPieceDev = case indexToColor g idx of
            White -> whiteDev cp
            _ -> blackDev cp
        penalty = case minorPieceDev of
            4 -> 0
            3 -> 3
            2 -> 6
            1 -> 9
            _ -> 12
        qMobility = multiMoveMobility queenDirs g idx
    in minimum (qMobility - penalty, 0)

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

-- TODO: This needs optimizing!
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
-- TODO: This needs optimizing
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
                    | enemy = (HasEnemy, (empties, StdMove True idx x "": enemies))
                    | friendly = (HasFriendly, (empties, enemies))
                    | otherwise = (Empty, (StdMove False idx x "": empties, enemies))
            in (case sqState of
                Empty -> loop (apply dir x) (newEmpties, newEnemies)
                _ -> (newEmpties, newEnemies))

-- TODO: this needs optimizing (10.1 indiv)
dirCaptureLoc :: Vector Char -> Int -> Dir -> Maybe Int
dirCaptureLoc g x dir =
    let c = indexToColor g x
        loop idx = case hasEnemy g c idx of
                      True -> Just idx
                      False | not (onBoard idx) -> Nothing
                            | isEmptyGrid g idx -> loop (apply dir idx)
                            | otherwise -> Nothing
    in loop (apply dir x)

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
      else if hasEnemy g c x then ([], [StdMove True idx x ""])
      else if hasFriendly g c x then ([], [])
      else ([StdMove False idx x ""],[]) -- empty square

dirLocsSingleCount :: Vector Char -> Int -> Dir -> Int
dirLocsSingleCount g idx dir =
    let x = apply dir idx
    in
      if not (onBoard x) then 0
      else if isEmptyGrid g x then 1
      else 0

-- This needs optimizing (4.2)
dirCaptureLocSingle :: Vector Char -> Int -> Color -> Dir -> Maybe Int
dirCaptureLocSingle g idx c dir =
    case apply dir idx of
        x | not (onBoard x) -> Nothing
          | hasEnemy g c x -> Just x
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

-- This should always be a list of length two
indexesToMove :: ChessNode -> [Int] -> Either String ChessMove
indexesToMove node [fromLoc, toLoc] =
    case checkIndexesToCastle [fromLoc, toLoc] of
        Just cm -> Right cm
        Nothing ->
            let g = node ^. (chessPos . cpGrid)
                cFrom = indexToColor g fromLoc
                cTo = indexToColor g toLoc
                isExch = cFrom /= Unknown && enemyColor cFrom == cTo
                removed = case isExch of
                    True -> toLoc
                    False -> -1
            in Right $ StdMove { _isExchange = isExch
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
wK, wKB, wKN, wKR, wQB, wQN, wQR ,wKP, wQP :: Int
wK = 15
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
bK, bKB, bKN, bKR, bQB, bQN, bQR, bKP, bQP :: Int
bK = 85
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

startingHasMovedWhite :: HasMoved
startingHasMovedWhite = HasMoved
    { _unMovedDev = wDevPieces
    , _unMovedCenterPawns = wCenterPawns
    , _unMovedCastling = wCastlingPieces
    , _castlingState = BothAvailable
    , _unMovedQueen = False }

startingHasMovedBlack :: HasMoved
startingHasMovedBlack = HasMoved
    { _unMovedDev = bDevPieces
    , _unMovedCenterPawns = bCenterPawns
    , _unMovedCastling = bCastlingPieces
    , _castlingState = BothAvailable
    , _unMovedQueen = False }

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

discoveredCheckNode :: ChessNode
discoveredCheckNode = preCastlingGameNode discoveredCheckBoard White (14, 64)

checkMateExampleNode :: ChessNode
checkMateExampleNode = preCastlingGameNode checkMateExampleBoard White (15, 85)

checkMateExampleNode2 :: ChessNode
checkMateExampleNode2 = preCastlingGameNode checkMateExampleBoard White (15, 85)

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
          , _cpHasMoved = (pcgnHasMovedWhite, pcgnHasMovedBlack)
          , _cpKingLoc = kingLocs
          , _cpInCheck = (False, False)
          , _cpWhitePieceLocs = wLocs
          , _cpBlackPieceLocs = bLocs
          , _cpFin = NotFinal }
    in ChessNode
        { _chessId = 0
        , _chessTreeLoc = TreeLocation {tlDepth = 0, tlIndexForDepth  = 0}
        , _chessMv = StdMove {_isExchange = False, _startIdx = -1, _endIdx = -1, _stdNote = ""}
        , _chessVal = ChessEval { _total = 0.0, _details = "" }
        , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
        , _chessPos = cPos
        , _chessMvSeq = []
        , _chessIsEvaluated = False }
      where
        pcgnHasMovedWhite = HasMoved
            { _unMovedDev = S.empty
            , _unMovedCenterPawns = S.empty
            , _unMovedCastling = S.empty
            , _castlingState = BothAvailable
            , _unMovedQueen = False }
        pcgnHasMovedBlack = HasMoved
            { _unMovedDev = S.empty
            , _unMovedCenterPawns = S.empty
            , _unMovedCastling = S.empty
            , _castlingState = KingSideOnlyAvailable
            , _unMovedQueen = False }

-- No castling available
postCastlingGameNode :: Vector Char -> Color -> (Int, Int) -> ChessNode
postCastlingGameNode grid the_color kingLocs =
    let (wLocs, bLocs) = calcLocsForColor grid
        cPos = ChessPos
          { _cpGrid = grid
          , _cpColor = the_color
          , _cpHasMoved = (postHasMovedWhite, postHasMovedBlack)
          , _cpKingLoc = kingLocs
          , _cpInCheck = (False, False)
          , _cpWhitePieceLocs = wLocs
          , _cpBlackPieceLocs = bLocs
          , _cpFin = NotFinal }
    in ChessNode
        { _chessId = 0
        , _chessTreeLoc = TreeLocation {tlDepth = 0, tlIndexForDepth  = 0}
        , _chessMv = StdMove {_isExchange = False, _startIdx = -1, _endIdx = -1, _stdNote = ""}
        , _chessVal = ChessEval { _total = 0.0, _details = "" }
        , _chessErrorVal = ChessEval { _total = 0.0, _details = "" }
        , _chessPos = cPos
        , _chessMvSeq = []
        , _chessIsEvaluated = False }
      where
        postHasMovedWhite = HasMoved
            { _unMovedDev = S.empty
            , _unMovedCenterPawns = S.empty
            , _unMovedCastling = S.empty
            , _castlingState = Castled
            , _unMovedQueen = False }
        postHasMovedBlack = HasMoved
            { _unMovedDev = S.empty
            , _unMovedCenterPawns = S.empty
            , _unMovedCastling = S.empty
            , _castlingState = Castled
            , _unMovedQueen = False }
