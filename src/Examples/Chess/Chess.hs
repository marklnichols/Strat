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

data King = King deriving (Show, Eq)
instance ChessPiece King where
    legalMoves = legalKingMoves
    pVal k = 1000

data Queen = Queen deriving (Show, Eq)
instance ChessPiece Queen where
    legalMoves = legalQueenMoves
    pVal q = 9


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
