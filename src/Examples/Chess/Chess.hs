{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chess where
import Control.Monad
import Data.Tree
import StratTree.TreeNode
import Control.Lens
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified ChessParser as Parser

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------




--Data PieceType = KingType | QueenType | RookType | BishopType | KnightType | PawnType deriving (Show, EQ)


data ChessPos = ChessPos {_grid :: V.Vector Int, _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''ChessPos

data ChessMv = ChessMv {isExchange :: Bool, _startIdx :: Int, _endIdx :: Int, _removedIdx :: Int}
    deriving (Eq, Ord)
makeLenses ''ChessMv

data ChessEval = ChessEval {_total :: Int, _details :: String}
    deriving (Eq, Ord)
makeLenses ''ChessEval

data ChessNode = ChessNode {_chessMv :: ChessMv, _chessVal :: ChessEval, _chessErrorVal :: ChessEval, _chessPos :: ChessPos}
makeLenses ''ChessNode

class ChessPiece a where
    legalMoves :: a -> ChessPos -> Int -> [ChessMv]
    pVal :: a -> Int
    dirs :: a -> [Int -> Int]

data King = King deriving (Show, Eq)
instance ChessPiece King where
    pVal k = 1000
    dirs k = kingDirs
    --TBD: some type i.e., glide, hop, shuffle, pawnie
    legalMoves = legalKingMoves --still need this?

data Queen = Queen deriving (Show, Eq)
instance ChessPiece Queen where
    pVal q = 9
    dirs q = queenDirs
    --TBD: some type i.e., glide, hop, shuffle, pawnie
    legalMoves = legalQueenMoves --still need this?

--Rook | Bishop | Knight | Pawn

instance PositionNode ChessNode ChessMv ChessEval where
    newNode = calcNewNode
    possibleMoves = getAllowedMoves
    color = view (chessPos . clr)
    final = view (chessPos . fin)
    showPosition = format
    parseMove = parseChessMv

instance TreeNode ChessNode ChessMv ChessEval where
    getMove = _chessMv
    getValue = _chessVal
    getErrorValue = _chessErrorVal

instance Show ChessMv where
    show move = case toParserMove move of
                    Just m -> show m
                    Nothing -> show move

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
{-- how indexes relate to board position (indexes in parens are not on the board):

   (41) (42) (43) (44) (45)

H|     37  38  39  40
G|   32  33  34  35  (36)
F|     28  29  30  31
E|   23  24  25  26  (27)
D|     19  20  21  22
C| (30)  31   32   33   34   35   36   37   38  (39)
B| (20)  21   22   23   24   25   26   27   28  (29)
A| (10)  11   12   13   14   15   16   17   18  (19)

   (00) (01) (02) (03) (04) (05) (06) (07) (08) (09)
     ---------------
    1    2     3    4    5    6    7   8


    Direction   add/subtract        Direction   add/subtract
    Right       1                   Knight U/L+   +8
    Left        -1                  Knght  D/R+   -8
    Up          10                  Knight D/L+   -12
    Down        -10                 Knight U/R+   +12
    Diag U/L    +9                  Knight U+/L   +19
    Diag D/R    -9                  Knight D+/R   -19
    Diag U/R    +11                 Knight U+/R   +21
    Diag D/L    -11                 Knight D+/L   -21
--}

right :: Int -> Int
right = (+1)

left :: Int -> Int
left = ((-)1)

up :: Int -> Int
up = (+10)

down :: Int -> Int
down = ((-)10)

diagUL  :: Int -> Int
diagUL = (+9)

diagDR :: Int -> Int
diagDR = ((-)9)

diagUR :: Int -> Int
diagUR = (+11)

diagDL :: Int -> Int
diagDL = ((-)11)

queenDirs :: [Int -> Int]
queenDirs = [right, left, up, down, diagUL, diagDR, diagUR, diagDL]

kingDirs :: [Int -> Int]
kingDirs = queenDirs

rookDirs :: [Int -> Int]
rookDirs = [up, down, left, right]

bishopDirs :: [Int -> Int]
bishopDirs = [diagUL, diagDR, diagUR, diagDL]

--knightDirs -- tbd

--pawnDirs -- tbd


---------------------------------------------------------------------------------------------------
-- Convert ChessMv to Parser Move (for display)
---------------------------------------------------------------------------------------------------
toParserMove :: ChessMv -> Maybe Parser.Move
toParserMove move = undefined

---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
format :: ChessNode -> String
format node = undefined

---------------------------------------------------------------------------------------------------
-- parse string input to move
---------------------------------------------------------------------------------------------------
parseChessMv :: ChessNode -> String -> Either String ChessMv
parseChessMv n s = undefined

---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: ChessNode -> ChessMv -> ChessNode
calcNewNode node mv = undefined

---------------------------------------------------------------------------------------------------
-- get possible moves from a given position
--------------------------------------------------------------------------------------------------
getAllowedMoves :: ChessNode -> [ChessMv]
getAllowedMoves = undefined

legalKingMoves :: King -> ChessPos -> Int -> [ChessMv]
legalKingMoves king pos idx = undefined

legalQueenMoves :: Queen -> ChessPos -> Int -> [ChessMv]
legalQueenMoves queen pos idx = undefined
