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
    , colorFromInt
    , colorToInt
    -- exported for testing only
    , getPieceLocs
    , possibleBishopMvs
    , possibleKnightMvs
    , possibleKingMvs
    , possiblePawnMvs
    , possiblePawnCaptures
    , possibleQueenMvs
    , possibleRookMvs
    ) where

import Control.Lens

import Data.Foldable
import Data.HashMap (Map)
-- import qualified Data.HashMap as M
import Data.Kind
import Data.Maybe
import Data.Singletons
import Data.Singletons.TH
import Data.Tree
import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Strat.StratTree.TreeNode
import qualified ChessParser as P
-- import Debug.Trace

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
empty :: Int
empty = 0

data Color = Black | White
    deriving (Show, Eq)

data ChessPos = ChessPos {_grid :: Vector Int, _clr :: Color, _fin :: FinalState} deriving (Show)
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

colorFromInt :: Int -> Color
colorFromInt 1 = White
colorFromInt _n = Black

-- flipColor :: Color -> Color
-- flipColor Black = White
-- flipColor _w = Black

----------------------------------------------------------------------------------------------------
-- New, new attempt with singletons
----------------------------------------------------------------------------------------------------
$(singletons [d|
  data Piece = King | Queen | Rook | Knight | Bishop | Pawn
    deriving (Show, Eq)
  |])

data ChessPiece :: Piece -> Type where
  UnsafeMkChessPiece :: { color :: Color } -> ChessPiece p

intToPiece :: Int -> Piece
intToPiece 1 = King
intToPiece 2 = Queen
intToPiece n = error $ "intToPiece not yet implemented for the value " ++ show n

----------------------------------------------------------------------------------------------------
indexToPiece :: Vector Int -> Int -> SomeChessPiece
indexToPiece g idx =
    let gridVal =  fromMaybe 0 (g ^? ix idx)
        piece = intToPiece gridVal
        _color = colorFromInt $ signum gridVal
    in  mkSomeChessPiece piece _color

mkSomeChessPiece :: Piece -> Color -> SomeChessPiece
mkSomeChessPiece piece c = case toSing piece of
  SomeSing p -> fromChessPiece p $ mkChessPiece p c

data SomeChessPiece :: Type where
  MkSomeChessPiece :: Sing p -> ChessPiece p -> SomeChessPiece

fromChessPiece :: Sing p -> ChessPiece p -> SomeChessPiece
fromChessPiece = MkSomeChessPiece

mkChessPiece :: Sing p -> Color -> ChessPiece p
mkChessPiece _ = UnsafeMkChessPiece

instance PositionNode ChessNode ChessMv ChessEval where
    newNode = calcNewNode
    possibleMoves = legalMoves
    color = colorToInt . view (chessPos . clr)
    -- color = view (chessPos . clr) `asTypeOf` _  -- :: ChessNode -> Color
    final = view (chessPos . fin)
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

getPieceLocs :: ChessNode -> [Int]
getPieceLocs node =
    let pos = node ^. chessPos
        c = colorToInt $ pos ^. clr
        range = V.fromList [0..100] :: Vector Int
        pairs = V.zip range (pos ^. grid) :: Vector (Int, Int)
        filtrd = V.filter (pMatch c) pairs :: Vector (Int, Int)
        first = V.map fst filtrd :: Vector Int
    in V.toList first
        where pMatch colr pair =
                let val = snd pair
                    av = abs val
                in (av > 0 && av <7 && (val * colr) > 0)

pieceMoves :: ChessNode -> Int -> [ChessMv]
pieceMoves _ _ = undefined

-- movesFromDir :: Dir -> Int -> MoveType -> [ChessMv]
-- movesFromDir _ _ _ = undefined  -- dir idx moveType

--possible destination squares for a king
possibleKingMvs :: Int -> [Int]
possibleKingMvs idx = filter onBoard (fmap ($ idx) queenDirs)

legalKingMoves :: ChessPos -> (Map Int Bool) -> Int -> [ChessMv]
legalKingMoves pos defendedMap idx =
    let locs = possibleKingMvs idx
        indexes = filter (kingFilter pos defendedMap (pos^.clr)) locs
    in  destinationsToMoves idx indexes

kingFilter :: ChessPos -> (Map Int Bool) -> Color -> Int -> Bool
kingFilter pos defendedMap c idx =
    if hasFriendly pos c idx
        then False
        else not $ isDefended defendedMap c idx

hasFriendly :: ChessPos -> Color -> Int -> Bool
hasFriendly pos c idx = case indexToPiece (_grid pos) idx of
    MkSomeChessPiece _theSing (UnsafeMkChessPiece aColor) -> aColor == c

hasEnemy :: ChessPos -> Color -> Int -> Bool
hasEnemy pos c idx = not $ hasFriendly pos c idx

isEmpty :: ChessPos -> Int -> Bool
isEmpty pos idx = fromMaybe empty ((_grid pos) ^? ix idx) == empty

--this can be used to filter lists of possible moves in order to quickly resolve
--captures to arbitrary depths
calcDefended :: ChessPos -> Color -> Map Int Bool
calcDefended _pos _c = undefined
{-
    let oppLocs = getOpposingLocs pos c
        oppMoves = movesFromLocs oppLocs
        destLocs = gatherDestLocs oppMoves
        defended = foldr f Map.empty destLocs where
          f loc theMap = M.insert loc True theMap
-}

movesFromLocs :: Vector Int -> Set ChessMv
movesFromLocs = undefined

gatherDestLocs :: Set ChessMv -> Set Int
gatherDestLocs _moves = undefined
{-
  foldr f S.empty moves where
    f :: ChessMv -> Set ChessMv -> Set ChessMv
    f mv mvs = S.insert (_endIdx mv) moves
-}

 -- empty :: Map k a
 -- insert :: (Hashable k, Ord k) => k -> a -> Map k a -> Map k a

  -- for each opposing piece,
  -- add list of legal moves to a Set
  -- build Map to True for each destination move loc in Set
  -- (for pawns this must be only the capturing moves)

getOpposingLocs :: ChessPos -> Color -> Vector Int
getOpposingLocs _pos _c = undefined
{-
    let oppColor = flipColor c
    in V.filter (\x -> colorFromInt x == oppColor) (_grid pos)

    -- filter :: (a -> Bool) -> Vector a -> Vector a
-}

isDefended :: Map Int Bool -> Color -> Int -> Bool
isDefended _ =  undefined   --color index
  -- this is just lookup of map built from calcDefended, with Nothing converted to False

-- find the legal destination locs for a queen
-- Note: 'legal' moves are a subset of 'possible' moves
legalQueenMvs :: Int -> [Int]
legalQueenMvs _idx = undefined
  -- let poss = possibleQueenMvs idx
  -- TODO: filter out moves w/Queen pinned to King
  -- filter out squares w/friendly pieces

-- find the possible destination locs for a queen
possibleQueenMvs :: Int -> [Int]
possibleQueenMvs idx = fold $ fmap (dirLocs idx) queenDirs

-- find the possible destination locs for a rook
possibleRookMvs :: Int -> [Int]
possibleRookMvs idx = fold $ fmap (dirLocs idx) rookDirs

-- find the possible destination locs for a bishop
possibleBishopMvs :: Int -> [Int]
possibleBishopMvs idx = fold $ fmap (dirLocs idx) bishopDirs

-- find the possible destination locs for a knight
possibleKnightMvs :: Int -> [Int]
possibleKnightMvs idx = filter onBoard (fmap ($ idx) knightDirs)

-- find the possible destination locs for a pawn (non-capturing moves)
possiblePawnMvs :: Int -> Color -> [Int]
possiblePawnMvs idx White =
  let oneSpace = whitePawnDir idx
  in oneSpace : if hasPawnMoved White idx
                  then []
                  else [whitePawnDir oneSpace] -- hasn't moved, 2 space move avail.
possiblePawnMvs idx Black =
  let oneSpace = blackPawnDir idx
  in oneSpace : if hasPawnMoved Black idx
                  then []
                  else [blackPawnDir oneSpace] -- hasn't moved, 2 space move avail.

hasPawnMoved :: Color -> Int -> Bool
hasPawnMoved White idx = idx > 28
hasPawnMoved Black idx = idx < 71

-- find the possible destination capture locs for a pawn
possiblePawnCaptures :: Int -> Color -> [Int]
possiblePawnCaptures idx White =
  let twoCaps = filter onBoard (fmap ($ idx) whitePawnCaptureDirs)
  in twoCaps ++ if hasPawnMoved White idx
                  then []
                  else filter onBoard (fmap ($ whitePawnDir idx) whitePawnCaptureDirs) --en passant
possiblePawnCaptures idx Black =
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

legalQueenMoves :: ChessPos -> Int -> [ChessMv]
legalQueenMoves _ _ = undefined

noMoves :: ChessPos -> Int -> [ChessMv]
noMoves _ _ = undefined

destinationsToMoves :: Int -> [Int] -> [ChessMv]
destinationsToMoves _ _ = undefined     -- idx dests
    --pair up idx with each index in dests to form moves

indexesToMove :: [Int] -> ChessMv
indexesToMove _ = undefined
