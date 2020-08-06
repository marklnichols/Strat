{-# LANGUAGE MultiParamTypeClasses #-}
module ChessTest (chessTest) where

import Chess
import qualified Data.Set as S
import Data.Tree
import Strat.StratTree.TreeNode
import Test.Hspec
import qualified Data.Vector.Unboxed as V

chessTest :: SpecWith ()
chessTest = do
    describe "getPieceLocs" $
        it "Gets the list of indexes of all chess pieces of a given color from the board" $ do
            getPieceLocs (nodeFromGridW board01) `shouldMatchList`
              [12, 13, 21, 22 ,25, 26, 33, 43, 45, 46, 53, 57]
            getPieceLocs (nodeFromGridB board01) `shouldMatchList`
              [61, 62, 65, 66, 67, 77, 78, 83, 86, 87]
    describe "possibleKingMoves" $
        it "Gets the possible moves for a king" $ do
            let (empties, enemies, friendlies) = possibleKingMoves board01 12
            empties `shouldMatchList` [11, 23]
            enemies `shouldMatchList` []
            friendlies `shouldMatchList` [13, 21, 22]
            let (empties2, enemies2, friendlies2) = possibleKingMoves board01 87
            empties2 `shouldMatchList` [76, 88]
            enemies2 `shouldMatchList` []
            friendlies2 `shouldMatchList` [77, 78, 86]
    describe "allowableQueenMoves" $
        it "Gets the allowable moves for a queen" $ do
            let (empties, enemies, friendlies) = allowableQueenMoves board01 46
            empties `shouldMatchList`
                [47,48 -- left/right
                ,36,56 -- up/down
                ,24,35 -- LL/UR
                ,28,37,55,64,73,82] -- LR, UL
            enemies `shouldMatchList` [66]
            friendlies `shouldMatchList` [13, 26, 45, 57]
            let (empties2, enemies2, friendlies2) = allowableQueenMoves board01 62
            empties2 `shouldMatchList`
                [63, 64 -- L/R
                ,32,42,52,72,82 -- U/D
                ,51,73,84 -- LL/UR
                ,71 ] -- LR/UL
            enemies2 `shouldMatchList` [22, 53]
            friendlies2 `shouldMatchList` [61, 65]
    describe "allowableRookMoves" $
        it "Gets the allowable moves for a rook" $ do
            let (empties, enemies, friendlies) = allowableRookMoves board01 13
            empties `shouldMatchList` [14,15,16,17,18 -- L/R
                                      ,23] -- U/D
            enemies `shouldMatchList` []
            friendlies `shouldMatchList` [12, 33]
            let (empties2, enemies2, friendlies2) = allowableRookMoves board01 83
            empties2 `shouldMatchList` [81,82,84,85 -- L/R
                                       ,73,63] -- U/D
            enemies2 `shouldMatchList` [53]
            friendlies2 `shouldMatchList` [86]
    describe "allowableBishopMoves" $
        it "Gets the allowable moves for a bishop" $ do
            let (empties, enemies, friendlies) = allowableBishopMoves board01 43
            empties `shouldMatchList` [32,54 -- LL/UR
                                      ,34,52] -- LR/UL
            enemies `shouldMatchList` [61, 65]
            friendlies `shouldMatchList` [21, 25]
            let (empties2, enemies2, friendlies2) = allowableBishopMoves board01 65
            empties2 `shouldMatchList` [54,76 -- LL/UR
                                       ,38,47,56,74] -- LR/UL
            enemies2 `shouldMatchList` [43]
            friendlies2 `shouldMatchList` [83, 87]
    describe "allowableKnightMoves" $
        it "Gets the allowable moves for a knight" $ do
            let (empties, enemies, friendlies) = allowableKnightMoves board01 45
            empties `shouldMatchList` [24,37,64]
            enemies `shouldMatchList` [66]
            friendlies `shouldMatchList` [26, 33, 53, 57]
            let (empties2, enemies2, friendlies2) = allowableKnightMoves board01 53
            empties2 `shouldMatchList` [32,34,41,72,74]
            enemies2 `shouldMatchList` [61,65]
            friendlies2 `shouldMatchList` [45]
    describe "allowablePawnMoves" $
        it "Gets the allowable (non capturing) moves for a pawn" $ do
            allowablePawnNonCaptures board01 22 `shouldMatchList` [32, 42]
            allowablePawnNonCaptures board01 33 `shouldMatchList` []
            allowablePawnNonCaptures board01 61 `shouldMatchList` [51]
    describe "allowablePawnCaptures" $
        it "Gets the allowable capturing moves for a pawn" $ do
            let (empties, enemies, friendlies) = allowablePawnCaptures board01 57
            empties `shouldMatchList` [68]
            enemies `shouldMatchList` [66]
            friendlies `shouldMatchList` []
            let (empties2, enemies2, friendlies2) = allowablePawnCaptures board01 78
            empties2 `shouldMatchList` []
            enemies2 `shouldMatchList` []
            friendlies2 `shouldMatchList` [67]
    describe "allowableEnPassant" $
        it "Gets the allowable enPassant capturing moves for a pawn" $
            allowableEnPassant board01 78 `shouldMatchList` [57]
    describe "calcDefended" $
       it "Creates a set w/all locations that are defended by the opposing color's pieces" $
           S.toList (calcDefended board01 White) `shouldMatchList`
              S.toList (S.fromList                 -- remove dupes (which are useful for debugging)
              [ 11, 13, 21, 22, 23                 -- K
              , 45, 47, 48                         -- Q (horizontal)
              , 26, 36, 56, 66                     -- Q (vertical)
              , 28, 37, 55, 64, 73, 82             -- Q (diagonal UL/LR)
              , 13, 24, 35, 57                     -- Q (diagonal LL/UR)
              , 12, 14, 15, 16, 17, 18, 23, 33     -- R1
              , 25, 27, 28, 16, 36, 46             -- R2
              , 14, 36, 47, 58, 16, 34, 43         -- B1
              , 21, 32, 54, 65, 25, 34, 52, 61     -- B2
              , 53, 64, 66, 57, 37, 26, 24, 33      -- N1 (@45)
              , 61, 72, 74, 65, 45, 34, 32, 41     -- N2 (@53)
              , 32, 31, 33, 42, 44, 66, 68         -- Pawns
              ])

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
                             '+',  ' ',  'K',  'R',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'P',  'P',  ' ',  ' ',  'B',  'R',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'B',  ' ',  'N',  'Q',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  'P',  ' ',  '+',
                             '+',  'p',  'q',  ' ',  ' ',  'b',  'p',  'p',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'b',  'p',  '+',
                             '+',  ' ',  ' ',  'r',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   r   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   b   p          7| (50)  71   72   73   74   75   76   77   78  (79)
p   q   -   -   b   p   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   N   -   -   -   P   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   B   -   N   Q   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   -   B   R   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   R   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

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
