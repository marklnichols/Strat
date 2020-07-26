{-# LANGUAGE MultiParamTypeClasses #-}
module ChessTest (chessTest) where

import Chess
-- import qualified Data.Set as S
import Data.Tree
import Strat.StratTree.TreeNode
import Test.Hspec
import qualified Data.Vector.Unboxed as V

chessTest :: SpecWith ()
chessTest = do
    describe "getPieceLocs" $
        it "Gets the list of indexes of all chess pieces of a given color from the board" $ do
            getPieceLocs (nodeFromGridW board01) `shouldMatchList` [12, 21, 22, 26, 33, 46]
            getPieceLocs (nodeFromGridB board01) `shouldMatchList` [62, 67, 76, 78, 86, 87]
    describe "possibleKingMoves" $
        it "Gets the possible moves for a king" $ do
            let (empties, enemies) = possibleKingMoves board01 12
            empties `shouldMatchList` [11, 13, 23]
            enemies `shouldMatchList` []
            let (empties2, enemies2) = possibleKingMoves board01 87
            empties2 `shouldMatchList` [77, 88]
            enemies2 `shouldMatchList` []
    describe "allowableQueenMoves" $
        it "Gets the allowable moves for a queen" $ do
            let (empties, enemies) = allowableQueenMoves board01 46
            empties `shouldMatchList`
                [41,42,43,44,45,47,48 -- left/right
                ,36,56,66 -- up/down
                ,13,24,35,57,68 -- LL/UR
                ,28,37,55,64,73,82] -- LR, UL
            enemies `shouldMatchList` [76]
            let (empties2, enemies2) = allowableQueenMoves board01 62
            empties2 `shouldMatchList`
                [61, 63, 64, 65, 66 -- L/R
                ,32,42,52,72,82 -- U/D
                ,51,73,84 -- LL/UR
                ,35,44,53,71 ] -- LR/UL
            enemies2 `shouldMatchList` [22, 26]

    describe "allowableRookMoves" $
        it "Gets the allowable moves for a rook" $ do
            let (empties, enemies) = allowableRookMoves board01 26
            empties `shouldMatchList` [23,24,25,27,28
                                      ,16,36]
            enemies `shouldMatchList` []

    -- describe "allowableBishopMoves" $
    --     it "Gets the allowable moves for a bishop" $
    --         allowableBishopMoves board01 22 `shouldMatchList` [ 11, 33, 44, 55, 66, 77, 88
    --                                                , 13, 31]
    -- describe "allowableKnightMoves" $
    --     it "Gets the allowable moves for a knight" $ do
    --         allowableKnightMoves board01 63 `shouldMatchList` [71, 82, 84, 75, 55, 44, 42, 51]
    --         allowableKnightMoves board01 27 `shouldMatchList` [15, 35, 46, 48]

    -- describe "allowablePawnMoves" $
    --     it "Gets the allowable (non capturing) moves for a pawn" $ do
    --         allowablePawnMoves board01 22 White `shouldMatchList` [32, 42]
    --         allowablePawnMoves board01 54 White `shouldMatchList` [64]
    --         allowablePawnMoves board01 22 Black `shouldMatchList` [12]
    --         allowablePawnMoves board01 72 Black `shouldMatchList` [52, 62]

    -- describe "allowablePawnCaptures" $
    --      it "Gets the allowable capturing moves for a pawn" $ do
    --          allowablePawnCaptures board01 White 43 `shouldMatchList` [52, 54] -- regular captures
    --          allowablePawnCaptures board01 White 22 `shouldMatchList` [31, 33, 41, 43] -- en passant captures
    --          allowablePawnCaptures board01 Black 22 `shouldMatchList` [11, 13]
    --          allowablePawnCaptures board01 Black 77 `shouldMatchList` [56, 58, 66, 68]
    -- describe "calcDefended" $
    --   it "Creates a set w/all locations that are defended by the opposing color's pieces" $ do
    --    S.toList (calcDefended board01 White) `shouldMatchList` (S.toList $ S.fromList

    --        [ 11, 13, 23                 -- K
    --        , 41, 42, 43, 44, 45, 47, 48 -- Q (horizontal)
    --        , 36, 56, 66                 -- Q (vertical)
    --        , 28, 37, 55, 64, 73, 82     -- Q (diagonal 1)
    --        , 13, 24, 35, 57, 68         -- Q (diagonal 2)
    --        , 23, 24, 25, 27, 28         -- R (horizontal)
    --        , 16, 36                     -- R (vertical)
    --        , 31, 32, 42, 44 ])          -- P

       -- S.toList (calcDefended board01 Black) `shouldMatchList`
       --     [ 11, 13, 23, 56, 66, 36, 41, 42, 43, 44, 45, 47, 48, 55, 64, 74, 83, 37, 28, 57, 68, 35,
       --       24, 13, 36, 16, 23, 24, 25, 27, 28, 32, 31, 42, 44 ]

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------
treeFromGridW :: V.Vector Char -> Tree ChessNode
treeFromGridW g = Node ChessNode
    { _chessMv = noMove
    , _chessVal = ChessEval {_total = 0, _details = ""}
    , _chessErrorVal = ChessEval {_total = 0, _details = ""}
    , _chessPos = posFromGridW g } []

treeFromGridB :: V.Vector Char -> Tree ChessNode
treeFromGridB g = Node ChessNode
    { _chessMv = noMove
    , _chessVal = ChessEval {_total = 0, _details = ""}
    , _chessErrorVal = ChessEval {_total = 0, _details = ""}
    , _chessPos = posFromGridB g } []

nodeFromGridW :: V.Vector Char -> ChessNode
nodeFromGridW g = rootLabel $ treeFromGridW g

nodeFromGridB :: V.Vector Char -> ChessNode
nodeFromGridB g = rootLabel $ treeFromGridB g

posFromGridW :: V.Vector Char -> ChessPos
posFromGridW g = ChessPos {_cpGrid = g, _cpColor = White, _cpFin = NotFinal}

posFromGridB :: V.Vector Char -> ChessPos
posFromGridB g = ChessPos {_cpGrid = g, _cpColor = Black, _cpFin = NotFinal}

noMove :: ChessMv
noMove = ChessMv {isExchange = False, _startIdx = -1, _endIdx = -1, _removedIdx = -1}

---------------------------------------------------------------------------------------------------
-- Test board positions
{-
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
-}

---------------------------------------------------------------------------------------------------
board01 :: V.Vector Char
board01 = V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'K',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'P',  'P',  ' ',  ' ',  ' ',  'R',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'Q',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'q',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  'p',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   p   -   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   q   -   -   -   -   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   Q   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   -   -   R   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
_boardTemplate :: V.Vector Int
_boardTemplate = V.fromList [ 99,  99,  99,  99,  99,  99,  99,  99,  99,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  99,  99,  99,  99,  99,  99,  99,  99,  99 ]

                                          -- [71,47,55,51,87,75,82,25,44,84,21,42]
                                          --  the extra elements are:   [47, 87, 25, 21]
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}
