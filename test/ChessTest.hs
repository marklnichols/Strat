{-# LANGUAGE MultiParamTypeClasses #-}
module ChessTest (chessTest) where

import Chess
import Data.Tree
import StratTree.TreeNode
import Test.Hspec
import qualified Data.Vector.Unboxed as V

chessTest :: SpecWith ()
chessTest = do
    describe "getPieceLocs" $
        it "Gets the list of indexes of all chess pieces of a given color from the board" $ do
            getPieceLocs (nodeFromGridW board01) `shouldMatchList` [12, 21, 22, 26, 33, 46]
            getPieceLocs (nodeFromGridB board01) `shouldMatchList` [62, 67, 76, 78, 86, 87]
    describe "getSingleLocs" $
        it "Gets the possible moves for a king" $ do
            getSingleLocs 22 `shouldMatchList` [23,21,32,12,31,13,33,11] 
            getSingleLocs 81 `shouldMatchList` [71, 72, 82]

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------
treeFromGridW :: V.Vector Int -> Tree ChessNode
treeFromGridW g = Node ChessNode 
    { _chessMv = noMove
    , _chessVal = ChessEval {_total = 0, _details = ""}
    , _chessErrorVal = ChessEval {_total = 0, _details = ""}
    , _chessPos = ChessPos {_grid = g, _clr = 1, _fin = NotFinal}} []

treeFromGridB :: V.Vector Int -> Tree ChessNode
treeFromGridB g = Node ChessNode 
    { _chessMv = noMove
    , _chessVal = ChessEval {_total = 0, _details = ""}
    , _chessErrorVal = ChessEval {_total = 0, _details = ""}
    , _chessPos = ChessPos {_grid = g, _clr = -1, _fin = NotFinal } } []

nodeFromGridW :: V.Vector Int -> ChessNode
nodeFromGridW g = rootLabel $ treeFromGridW g

nodeFromGridB :: V.Vector Int -> ChessNode
nodeFromGridB g = rootLabel $ treeFromGridB g

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
-}

---------------------------------------------------------------------------------------------------
board01 :: V.Vector Int
board01 = V.fromList       [ 99,  99,  99,  99,  99,  99,  99,  99,  99,  99, 
                             99,  00,  01,  00,  00,  00,  00,  00,  00,  99,
                             99,  06,  06,  00,  00,  00,  03,  00,  00,  99,
                             99,  00,  00,  06,  00,  00,  00,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  02,  00,  00,  99,
                             99,  00,  00,  00,  00,  00,  00,  00,  00,  99,
                             99,  00, -02,  00,  00,  00,  00, -06,  00,  99,
                             99,  00,  00,  00,  00,  00, -06,  00, -06,  99,
                             99,  00,  00,  00,  00,  00, -03, -01,  00,  99, 
                             99,  99,  99,  99,  99,  99,  99,  99,  99,  99 ]

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
