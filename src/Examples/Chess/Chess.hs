{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chess where
import Data.Tree
import StratTree.TreeNode
import Control.Lens
import qualified Data.Vector.Unboxed as V
import qualified ChessParser as Parser

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
data PieceType = KingType | QueenType | RookType | BishopType | 
                 KnightType | PawnType deriving (Show, Eq)

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

data MoveType = Glide | Hop | Single | Pawny deriving (Eq, Show)

class ChessPiece a where
    pieceType :: a -> PieceType
    moveType :: a -> MoveType
    legalMoves :: a -> ChessPos -> Int -> [ChessMv]
    pVal :: a -> Int
    dirs :: a -> [Int -> Int]
    
data King = King deriving (Show, Eq)
instance ChessPiece King where
    pieceType _ = KingType
    moveType _ = Single
    pVal _ = 1000
    dirs _ = queenDirs
    legalMoves = legalKingMoves 

data Queen = Queen deriving (Show, Eq)
instance ChessPiece Queen where
    pieceType _ = QueenType
    moveType _ = Glide
    pVal _ = 9
    dirs _ = queenDirs
    legalMoves = legalQueenMoves

-- TODO: Rook | Bishop | Knight | Pawn

instance PositionNode ChessNode ChessMv ChessEval where
    newNode = calcNewNode
    possibleMoves = getLegalMoves
    color = view (chessPos . clr)
    final = view (chessPos . fin)
    parseMove = parseChessMv

instance TreeNode ChessNode ChessMv ChessEval where
    getMove = _chessMv
    getValue = _chessVal
    getErrorValue = _chessErrorVal

instance Show ChessMv where
    show mv = case toParserMove mv of
                    Just m -> show m
                    Nothing -> show mv

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
---------------------------------------------------------------------------------------------------}

right :: Int -> Int
right = (1+)

left :: Int -> Int
left x = x - 1

up :: Int -> Int
up = (10+)

down :: Int -> Int
down x = x - 10

diagUL  :: Int -> Int
diagUL = (9+)

diagDR :: Int -> Int
diagDR x = x - 9

diagUR :: Int -> Int
diagUR = (11+)

diagDL :: Int -> Int
diagDL x = x - 11

knightLU :: Int -> Int
knightLU = (8+)

knightRD :: Int -> Int
knightRD x = x - 8

knightLD :: Int -> Int
knightLD x = x - 12

knightRU :: Int -> Int
knightRU = (12+)

knightUL :: Int -> Int
knightUL = (19+)

knightDR :: Int -> Int
knightDR x = x - 19

knightUR :: Int -> Int
knightUR = (21+)

knightDL :: Int -> Int
knightDL x = x - 21

queenDirs :: [Int -> Int]
queenDirs = [right, left, up, down, diagUL, diagDR, diagUR, diagDL]

rookDirs :: [Int -> Int]
rookDirs = [up, down, left, right]

bishopDirs :: [Int -> Int]
bishopDirs = [diagUL, diagDR, diagUR, diagDL]

knightDirs :: [Int -> Int]
knightDirs = [knightLU, knightRD, knightLD, knightRU, knightUL, knightDR, knightUR, knightDL]

pawnDirs :: [Int -> Int]
pawnDirs = [up, diagUL, diagUR]

---------------------------------------------------------------------------------------------------
-- Convert ChessMv to Parser Move (for display)
---------------------------------------------------------------------------------------------------
toParserMove :: ChessMv -> Maybe Parser.Move
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
getLegalMoves :: ChessNode -> [ChessMv]
getLegalMoves = undefined

getPieceLocs :: ChessNode -> [Int]
getPieceLocs _ = undefined

pieceMoves :: ChessNode -> Int -> [ChessMv]
pieceMoves _ _ = undefined

{-
legalKingMoves :: King -> ChessPos -> Int -> [ChessMv]
legalKingMoves king pos idx = 
    let locs = getSingleLocs pos
    
    if isFriendly -> false
    if isFriendly or isEmpty
        if Sq is attackable
            False
        else true
-}          

--possible destination squares for a king, disregarding other pieces, check status, etc.
getSingleLocs :: Int -> [Int]
getSingleLocs idx = filter onBoard (fmap ($ idx) queenDirs)

onBoard :: Int -> Bool
onBoard x 
    | x < 10          = False
    | x > 88          = False
    | x `mod` 10 == 0 = False
    | x `mod` 10 == 9 = False
    | otherwise       = True

legalKingMoves :: King -> ChessPos -> Int -> [ChessMv]
legalKingMoves _ _ _ = undefined

legalQueenMoves :: Queen -> ChessPos -> Int -> [ChessMv]
legalQueenMoves _ _ _ = undefined
