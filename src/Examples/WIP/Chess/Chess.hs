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
    , allowableBishopMoves
    , allowableKnightMoves
    , possibleKingMoves
    , allowablePawnMoves
    , allowablePawnCaptures
    , allowableQueenMoves
    , allowableRookMoves
    ) where

import Control.Lens hiding (Empty)

import Data.Char
-- import Data.Foldable
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
import Debug.Trace

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

enemyColor :: Color -> Color
enemyColor White = Black
enemyColor Black = White
enemyColor Unknown = Unknown

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
  show d = _dirName d
{-
newtype IntMove = IntMove {theInt :: Int}

instance Show IntMove where
    show m = show $ theInt m
-}

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
  | ord c > 96 && ord c < 123 = Black -- a - z : 97 - 122
  | otherwise = Unknown

pieceMoves :: ChessNode -> Int -> [ChessMv]
pieceMoves _ _ = undefined

-- movesFromDir :: Dir -> Int -> MoveType -> [ChessMv]
-- movesFromDir _ _ _ = undefined  -- dir idx moveType



-- find the possible destination locs for a king
-- The function 'allowableKingMoves' later filters out the moves that would allow the enemy to
-- capture the king
possibleKingMoves :: Vector Char -> Int -> ([Int], [Int])
possibleKingMoves g idx = allowableSingleMoves kingDirs g idx

-- TODO: this will probably become 'allowableKingMoves'
legalKingMoves :: ChessPos -> (Map Int Bool) -> Int -> [ChessMv]
legalKingMoves _pos _defendedMap _idx = undefined
    -- let locs = possibleKingMoves (pos^.cpGrid) idx
    --     indexes = filter (kingFilter (pos ^.cpGrid) defendedMap (pos^.cpColor)) locs
    -- in  destinationsToMoves idx indexes

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
hasEnemy g c idx = indexToColor g idx == enemyColor c

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos idx = fromMaybe empty ((_cpGrid pos) ^? ix idx) == empty

  -- for each opposing piece,
  -- build list of legal moves
  -- build Set consisting of each ending loc
  -- (for pawns this must be only the capturing moves)

----------------------------------------------------------------------------------------------------
-- Calculate the set of all locations that are 'defended' by the given color
-- The intendtion is to use this only once per position / color
-- This can be used to filter lists of allowable moves in order to quickly resolve
-- captures to arbitrary depths
----------------------------------------------------------------------------------------------------
calcDefended :: Vector Char -> Color -> Set Int
calcDefended g c =
    let theLocs = locsForColor g c
        (destEmpty, destEnemy) = movesFromLocs g theLocs
    in S.fromList (destEmpty ++ destEnemy)

movesFromLocs :: Vector Char -> [Int] -> ([Int], [Int])
movesFromLocs g locs =
  foldr f ([], []) locs where
    f :: Int -> ([Int], [Int]) -> ([Int], [Int])
    f loc (r1, r2) =
      let (xs, ys) = movesFromLoc g loc
      in (xs ++ r1, ys ++ r2)

movesFromLoc :: Vector Char -> Int -> ([Int], [Int])
movesFromLoc g loc =
  let cp = indexToPiece g loc -- ChessPiece (k :: SomeSing Piece)
  in case cp of
      -- charToPiece :: Char -> ChessPiece (k :: SomeSing Piece)

      -- data ChessPiece :: SomeSing Piece -> Type where
      --   MkChessPiece :: Color -> SomeSing Piece -> ChessPiece k

      MkChessPiece _c (SomeSing SKing) -> possibleKingMoves g loc
      MkChessPiece _c (SomeSing SQueen) -> allowableQueenMoves g loc
      MkChessPiece _c (SomeSing SRook) -> allowableRookMoves g loc
      MkChessPiece _c (SomeSing SKnight) -> allowableKnightMoves g loc
      MkChessPiece _c (SomeSing SBishop) -> allowableBishopMoves g loc
      MkChessPiece c (SomeSing SPawn) -> allowablePawnCaptures g c loc

isDefended :: Map Int Bool -> Color -> Int -> Bool
isDefended _ =  undefined   --color index
  -- this is just lookup of map built from calcDefended, with Nothing converted to False


-- find the allowable destination locs for a pieces that move multiple squares in a given
-- direction(s) (i.e., Queen, Rook, Bishop). The first list contains the empty squares that
-- can be moved to. The second list contains squares with pieces that could be captured.
allowableMultiMoves :: [Dir] -> Vector Char -> Int -> ([Int], [Int])
allowableMultiMoves pieceDirs g idx =
  foldr f ([], []) pieceDirs
    where
      f :: Dir -> ([Int], [Int]) -> ([Int], [Int])
      f x (r, r') =
        let (freeLocs, captureLocs) = dirLocs g idx x
        in (freeLocs ++ r, captureLocs ++ r')

-- find the allowable destination locs for a pieces that move one square in a given
-- direction (i.e., King and Knight). The first list contains the empty squares that
-- can be moved to. The second list contains squares with pieces that could be captured.
allowableSingleMoves :: [Dir] -> Vector Char -> Int -> ([Int], [Int])
allowableSingleMoves pieceDirs g idx =
  foldr (f (indexToColor g idx)) ([], []) pieceDirs
    where
      -- fold function is curried f with Color applied
      f :: Color -> Dir -> ([Int], [Int]) -> ([Int], [Int])
      f c x (r, r') =
        let (freeLocs, captureLocs) = dirLocsSingle g idx c x
        in (freeLocs ++ r, captureLocs ++ r')

-- find the allowable destination locs for a queen.  The first list contains the empty squares that
-- can be moved to. The second list contains squares with pieces that could be captured.
allowableQueenMoves :: Vector Char -> Int -> ([Int], [Int])
allowableQueenMoves g idx = allowableMultiMoves queenDirs g idx

-- find the allowable destination locs for a rook
allowableRookMoves :: Vector Char -> Int -> ([Int], [Int])
allowableRookMoves g idx = allowableMultiMoves rookDirs g idx

-- find the allowable destination locs for a bishop
allowableBishopMoves :: Vector Char -> Int -> ([Int], [Int])
allowableBishopMoves g idx = allowableMultiMoves bishopDirs g idx

-- find the possible destination locs for a knight
allowableKnightMoves :: Vector Char -> Int -> ([Int], [Int])
allowableKnightMoves g idx = allowableSingleMoves knightDirs g idx

-- find the allowable destination locs for a pawn (non-capturing moves)
allowablePawnMoves :: Vector Char -> Int -> [Int]
allowablePawnMoves g idx =
    let c = indexToColor g idx
        hasMoved = hasPawnMoved c idx
    in case c of
        Unknown -> []
        White -> pawnMoves g c whitePawnDir hasMoved idx
        Black -> pawnMoves g c blackPawnDir hasMoved idx

-- pawnMoves :: Vector Char -> Dir -> Bool -> Int -> [Int]
-- pawnMoves g dir hasMoved idx =
--   let (firstMove : _, _) = dirLocsSingle g idx dir -- only take the 'empty' square
--   in firstMove :
--       if hasMoved then []
--       else fst $ dirLocsSingle g firstMove dir
pawnMoves :: Vector Char -> Color -> Dir -> Bool -> Int -> [Int]
pawnMoves g c dir hasMoved idx =
    case dirLocsSingle g idx c dir of
        ([], _) -> []
        (firstMove:_, _) ->  -- only take the 'empty' square
           let secondList =
                 (if hasMoved
                   then []
                   else fst $ dirLocsSingle g firstMove c dir)
           in firstMove : secondList

hasPawnMoved :: Color -> Int -> Bool
hasPawnMoved Unknown _ = False
hasPawnMoved White idx = idx > 28
hasPawnMoved Black idx = idx < 71

-- find the allowable destination capture locs for a pawn
allowablePawnCaptures :: Vector Char -> Color -> Int -> ([Int], [Int])
allowablePawnCaptures _g Unknown _idx = undefined -- []
allowablePawnCaptures _g White _idx = undefined
  -- let twoCaps = filter onBoard (fmap ($ idx) whitePawnCaptureDirs)
  -- in twoCaps ++ if hasPawnMoved White idx
  --                 then []
  --                 else filter onBoard (fmap ($ whitePawnDir idx) whitePawnCaptureDirs) --en passant
allowablePawnCaptures _g Black _idx = undefined
  -- let twoCaps = filter onBoard (fmap ($ idx) blackPawnCaptureDirs)
  -- in twoCaps ++ if hasPawnMoved Black idx
  --                 then []
  --                 else filter onBoard (fmap ($ blackPawnDir idx) blackPawnCaptureDirs) --en passant

data SquareState = Empty | HasFriendly | HasEnemy | OffBoard
   deriving Show

-- find the allowable destination locs for a piece, given a specified direction to move.
-- this function is appropriate for queens, rooks, and bishops
-- the search short-circuits when hitting a friendly or enemy pieces
-- the first list in the tuple are the empty that could be moved to
-- the second tuple contains squares where enemy pieces could be captured
dirLocs :: Vector Char -> Int -> Dir ->([Int], [Int])
dirLocs g idx dir =
  let str = "dirLocs called with idx: " ++ show idx
        ++ ", color: " ++ show c ++ ", dir: " ++ show dir
  in trace str (loop (apply dir idx) ([], []))
      where
        c = indexToColor g idx
        loop x (empties, enemies) =
            let friendly = hasFriendly g c x
                enemy = hasEnemy g c x
                (sqState, (newEmpties, newEnemies)) =
                    if not (onBoard x) then (OffBoard ,(empties, enemies))
                    else if enemy then (HasEnemy, (empties, x : enemies))
                    else if friendly then (HasFriendly, (empties, enemies))
                    else (Empty, (x : empties, enemies))
            -- in case sqState of
            --     Empty -> loop (apply dir x) (newEmpties, newEnemies)
            --     _ -> (newEmpties, newEnemies)
            in trace ("'LOOOOOP' in dirLocs call with index: " ++ show x
                      ++ " value at index: " ++ show ((V.!) g x)
                      ++ " dir: " ++ show dir
                    ++ " returning - sqState: " ++ show sqState ++ " empties: " ++ show empties
                    ++ ", enemies: " ++ show enemies)
                    (case sqState of
                        Empty -> loop (apply dir x) (newEmpties, newEnemies)
                        _ -> (newEmpties, newEnemies))

-- Same as dirLocs, but for pieces that move only one square in a given direction
-- (aka King and Knight) -- some code intentionally duplicated with 'dirLocs'
dirLocsSingle :: Vector Char -> Int -> Color -> Dir ->([Int], [Int])
dirLocsSingle g idx c dir =
    let x = apply dir idx
    in
      if not (onBoard x) then (([], []))
      else if hasEnemy g c x then (([], [x]))
      else if hasFriendly g c x then (([], []))
      else ([x],[]) -- empty square

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
