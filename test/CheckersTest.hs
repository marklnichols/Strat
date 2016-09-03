{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import CkParser
import Test.Hspec
import Data.Tree
import Data.Either
import qualified Data.Vector.Unboxed as V
import Control.Lens
import StratTree.TreeNode

checkersTest = do
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel getStartNode) `shouldMatchList` fmap mkSimpleCkMove [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getPossibleMoves blackFirstStartNode `shouldMatchList` fmap mkSimpleCkMove [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves

            getPossibleMoves (nodeFromGridW board01) `shouldMatchList` fmap mkSimpleCkMove    [510, 1721, 1722, 2832, 2833, 2823, 2824, 3934, 3935]
            getPossibleMoves (nodeFromGridB board01) `shouldMatchList` fmap mkSimpleCkMove    [3732, 3733, 3025, 3026, 2024, 2025, 2015, 2016, 711, 712]
            getPossibleMoves (nodeFromGridW board02) `shouldMatchList` mkMultiCkJump m02Multi : fmap mkSimpleCkJump [(0717, 12), (1624, 20), (2515, 20), (2517, 21), (2535, 30)]

            getPossibleMoves (nodeFromGridB board02) `shouldMatchList` fmap mkSimpleCkJump [(2111, 16), (3729, 33)]
            getPossibleMoves (nodeFromGridW board06) `shouldMatchList` fmap mkSimpleCkJump [(2535, 30)]

            getPossibleMoves (nodeFromGridW board07) `shouldMatchList` snd (partitionEithers (fmap
                (parseCkMove (nodeFromGridW board07)) ["A3-C1-E3", "A3-C5-E3", "A3-C5-E7-G5"]))
    describe "calcNewNode" $
        it "creates a new node from a previous position and a move" $ do
            calcNewNode (nodeFromGridW board01) (mkSimpleCkMove m1) ^. ckPosition ^. grid `shouldBe` board01_m1
            calcNewNode (nodeFromGridB board02) (mkSimpleCkJump m2) ^. ckPosition ^. grid `shouldBe` board02_m2
            calcNewNode (nodeFromGridW board03) (mkMultiCkJump m3) ^. ckPosition ^. grid `shouldBe` board03_m3
    describe "pieceCount" $
        it "Counts the number of white regular pieces minus the black regular pieces" $
            totalPieceCount board04 `shouldBe` -2
    describe "kingCount" $
        it "Counts the number of white king pieces minus the black king pieces" $
            totalKingCount board04 `shouldBe` 4
    describe "parseCkMove" $
        it "parses move input into a CkMove" $ do
            parseCkMove (nodeFromGridW board01) "A1 B2" `shouldBe` Right (mkSimpleCkMove 510)
            parseCkMove (nodeFromGridW board01) "A1-b2" `shouldBe` Right (mkSimpleCkMove 510)
            parseCkMove (nodeFromGridW board02) "A5 C7" `shouldBe` Right (mkSimpleCkJump (0717, 12))
            parseCkMove (nodeFromGridW board03) "E5 C7 A5" `shouldBe` Right (mkMultiCkJump m3)
    describe "toParserMove" $
        it "converts a CkMove to a Parser Move (for display)" $ do
            toParserMove (mkSimpleCkMove 510) `shouldBe` Just (Move [Loc 'A' 1,  Loc 'B' 2])
            toParserMove (mkSimpleCkJump (0717, 12)) `shouldBe` Just (Move [Loc 'A' 5, Loc 'C' 7])
            toParserMove (mkMultiCkJump m3) `shouldBe` Just (Move [Loc 'E' 5, Loc 'C' 7, Loc 'A' 5])
    describe "checkPromote" $
        it "promotes a piece to king if it has reached the back row" $ do
            checkPromote (nodeFromGridW board05) 01 38 `shouldBe` 2
            checkPromote (nodeFromGridB board05) (-1) 07 `shouldBe` (-2)
            checkPromote (nodeFromGridW board05) 01 25 `shouldBe` 1
    describe "checkFinal" $
        it "checks to see if the game is over" $ do
            checkFinal (nodeFromGridW board09) `shouldBe` Draw
            checkFinal (nodeFromGridB board09) `shouldBe` NotFinal
            checkFinal (nodeFromGridW board09b) `shouldBe` NotFinal
            checkFinal (nodeFromGridB board09b) `shouldBe` Draw
            checkFinal (nodeFromGridB board11) `shouldBe` WWins
            checkFinal (nodeFromGridW board12) `shouldBe` BWins
    describe "mobility" $
        it "determine the mobility of each side" $ do
            mobility (nodeFromGridW board01) `shouldBe` -1
            mobility (nodeFromGridB board03) `shouldBe`  3
    describe "homeRow'" $
        it "checks to see if the home row is full or if two no adjacent squares are occupied" $ do
            homeRow' board01 1 `shouldBe` homeRowNone
            homeRow' board01 (-1) `shouldBe` homeRowNone
            homeRow' board10 1 `shouldBe` homeRowPartial
            homeRow' board10 (-1) `shouldBe` homeRowPartial
            homeRow' board08 1 `shouldBe` homeRowFull
            homeRow' board08 (-1) `shouldBe` homeRowFull
    describe "piecePrgress" $
        it "calculates the sum of values corresponding to how far non-king piece has progressed down the board" $ do
            pieceProgress (V.fromList [15, 33]) 1 `shouldBe` 3
            pieceProgress (V.fromList [15, 33]) (-1) `shouldBe` -2

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------
blackFirstStartNode :: CkNode
blackFirstStartNode = rootLabel getStartNode & ckPosition.clr .~ (-1)

treeFromGridW :: V.Vector Int -> Tree CkNode
treeFromGridW g = Node CkNode {_ckMove = mkSimpleCkMove (-1), _ckValue = 0, _ckErrorValue = 0,
    _ckPosition = CkPosition {_grid = g, _clr = 1, _fin = NotFinal}} []

treeFromGridB :: V.Vector Int -> Tree CkNode
treeFromGridB g = Node CkNode {_ckMove = mkSimpleCkMove (-1), _ckValue = 0, _ckErrorValue = 0,
    _ckPosition = CkPosition {_grid = g, _clr = -1, _fin = NotFinal}} []

nodeFromGridW :: V.Vector Int -> CkNode
nodeFromGridW g = rootLabel $ treeFromGridW g

nodeFromGridB :: V.Vector Int -> CkNode
nodeFromGridB g = rootLabel $ treeFromGridB g

positionFromGridW :: V.Vector Int -> CkPosition
positionFromGridW g = nodeFromGridW g ^. ckPosition

positionFromGridB :: V.Vector Int -> CkPosition
positionFromGridB g = nodeFromGridB g ^. ckPosition

mkSimpleCkMove :: Int -> CkMove
mkSimpleCkMove i = CkMove {_isJump = False, _startIdx = i `div` 100, _endIdx = i `mod` 100, _middleIdxs = [], _removedIdxs = []}

mkSimpleCkJump :: (Int, Int) -> CkMove
mkSimpleCkJump (mv, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = [], _removedIdxs = [removed]}

mkMultiCkJump :: (Int, [Int], [Int]) -> CkMove
mkMultiCkJump (mv, middle, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = middle, _removedIdxs = removed}

---------------------------------------------------------------------------------------------------
-- Test Reader environments
---------------------------------------------------------------------------------------------------
envDepth6 = Env {_depth =6, _errorDepth = 3, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

---------------------------------------------------------------------------------------------------
-- Test board positions
---------------------------------------------------------------------------------------------------
board01 :: V.Vector Int
board01 = V.fromList [99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -02, 00, 00,
                      00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]

{-                                   --  (41) (42) (43) (44) (45)
                -1   00   02   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   00    --     19   20   21   22
              00   00   00   01      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13
              01   00   -2   00      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}
m1 = 1722
board01_m1 :: V.Vector Int  --board01 with move m1 applied
board01_m1 = V.fromList [99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, -02, 00, 01,
                         00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]

{-                                   --  (41) (42) (43) (44) (45)
                -1   00   02   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   01    --     19   20   21   22
              00   00   00   00      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13
              01   00   -2   00      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}

m02Multi = (1634, [26], [21, 30])     --- 16 -> 26 -> 34 jumping 21 and 30
board02 :: V.Vector Int
board02 = V.fromList [99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
                      00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]

{-                                   --  (41) (42) (43) (44) (45)
                -1   00   02   00    --     37   38   39   40
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13
              01   00   01   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}

m2 = (3729, 33)
board02_m2 :: V.Vector Int --board02 with move m2 applied
board02_m2 = V.fromList [99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
                         00, 00, 02, 00, 99, 02, -1, -1, 00, 01, 00, 00, 00, 99, 00, 00, 02, 00, 99, 99, 99, 99, 99]

{-                                   --  (41) (42) (43) (44) (45)
                00   00   02   00    --     37   38   39   40
              01  00   00   00      --   32   33   34   35    (36)
                02   -1   -1   00    --     28   29   30   31
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13
              01   00   01   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}


board03 :: V.Vector Int
board03 = V.fromList [99, 99, 99, 99, 99, 01, 00, 00, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
                      00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
{-                                   --  (41) (42) (43) (44) (45)
                -1   00   02   00    --     37   38   39   40
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13
              01   00   00   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}
m3 = (2507, [17], [21, 12])     --- 25 -> 17 -> 7 jumping 21 and 12
board03_m3 :: V.Vector Int
board03_m3 = V.fromList [99, 99, 99, 99, 99, 01, 00, 02, 01, 99, 00, 00, 00, 00, 00, 00, 01, 00, 99, 00, -02, 00, 00,
                         00, 00, 00, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
{-                                   --  (41) (42) (43) (44) (45)
                -1   00   02   00    --     37   38   39   40
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   00    --     19   20   21   22
              00   00   01   00      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13
              01   00   02   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)
--}

board04 :: V.Vector Int
board04 = V.fromList [99, 99, 99, 99, 99, 00, 01, 00, 02, 99, 00, -1, 00, 00, -1, 00, 00, 00, 99, 02, 00, 00, 00,
                      00, 00, 00, 00, 99, 00, -2, 00, 00, 00, 02, 00, -1, 99, 02, 00, 00, 02, 99, 99, 99, 99, 99]
board04_pc = -2
board04_kc = 1

board05 :: V.Vector Int
board05 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, -1, 00, 00, 00, 00, 00, 00, 99, 00, 00, 01, 00,
                      00, -1, 00, 00, 99, 00, 00, 00, 00, 00, 01, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]

{--                                  --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   01   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   -1   00   00      --   23   24   25   26      (27)
                00   00   01   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   -1   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
--}


board06 :: V.Vector Int
board06 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, -1, 00, 00, 99, 00, 00, 00, 00,
                      00, 00, 01, 00, 99, 00, 01, -1, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   01   -1   00    --     28   29   30   31
              00   00   01   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   -1   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}


board07 :: V.Vector Int
board07 = V.fromList [99, 99, 99, 99, 99, 00, 01, 00, 00, 99, -1, -1, 00, 00, 00, 00, 00, 00, 99, -1, -1, -1, 00,
                      00, 00, 00, 00, 99, 00, 00, -1, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
multi-jumps: A3-C1-E3, A3-C5-E3, A3-C5-E7-G5
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   -1   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                -1   -1   -1   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                -1   -1   00   00    --     10   11   12   13
              00   01   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

board08 :: V.Vector Int
board08 =  V.fromList [99, 99, 99, 99, 99, 01, 01, 01, 01, 99, 01, 01, 00, 01, -1, 00, 01, 00, 99, 00, 00, 00, 00,
                       -1, 00, -1, 00, 99, 01, 00, 00, -1, 00, 00, 00, -1, 99, -1, -1, -1, -1, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                -1   -1   -1   -1    --     37   38   39   40
              00   00   00   -1      --   32   33   34   35      (36)
                01   00   00   -1    --     28   29   30   31
              -1   00   -1   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              -1   00   01   00      --   14   15   16   17      (18)
                01   01   00   01    --     10   11   12   13
              01   01   01   01      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
--}


boardDebug2 :: V.Vector Int
boardDebug2 = V.fromList [99, 99, 99, 99, 99, 00, -1, 00, 00, 99, 00, 00, 00, 00, 01, 00, 00, 00, 99, 00, 00, 00, 00,
                          00, 00, 00, -1, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 01, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   01   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00  -1      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              01   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   -1   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

board09 :: V.Vector Int
board09 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00,
                      00, 00, 00, 00, 99, 00, 00, 00, 00, 01, 00, 00, 00, 99, -1, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                -1   00   00   00    --     37   38   39   40
              01   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

board09b :: V.Vector Int
board09b =  V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 01, 99, 00, 00, 00, -2, 00, 00, 00, 01, 99, 00, 00, 01, 00,
                        00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   01   00    --     19   20   21   22
              00   00   00   01      --   14   15   16   17      (18)
                00   00   00   -2    --     10   11   12   13
              00   00   00   01      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

board10 :: V.Vector Int
board10 =  V.fromList [99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 01, 01, 00, 01, -1, 00, 01, 00, 99, 00, 00, 00, 00,
                       -1, 00, -1, 00, 99, 01, 00, 00, -1, 00, 00, 00, -1, 99, -1, 00, -1, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                -1   00   -1   00    --     37   38   39   40
              00   00   00   -1      --   32   33   34   35      (36)
                01   00   00   -1    --     28   29   30   31
              -1   00   -1   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              -1   00   01   00      --   14   15   16   17      (18)
                01   01   00   01    --     10   11   12   13
              01   00   01   01      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
--}

board11 :: V.Vector Int
board11 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, 00, 00, 00,
                      00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 02, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   02   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   00   01      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

board12 :: V.Vector Int
board12 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00,
                      00, -1, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   -1   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

boardXtoWin :: V.Vector Int
boardXtoWin = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00,
                          -2, 02, 02, 00, 99, 00, 00, 00, 00, 00, 00, -2, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   -2   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              -2   02   02   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

blunderBoard1 :: V.Vector Int
blunderBoard1 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 01, 00, 00, 00, 00, 00, 01, 00, 99, 00, 00, 00, 00,
                            00, 00, 00, 00, 99, 00, 00, -2, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
-- black to move, if f6-e5 checking obvious mistake of E3-D4 or E3-F4
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   -2   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   01   00      --   14   15   16   17      (18)
                01   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}


blunderBoard2 :: V.Vector Int
blunderBoard2 = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, -2, 00, 99, 02, 00, 00, 00,
                            00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 02, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
 -- c5-d4 allows possible blunder d2-e1 or d2-c1
{--
                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   02   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                02   00   00   00    --     19   20   21   22
              00   00   -2   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

{--
board0n :: V.Vector Int
board0n = V.fromList [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00,
                      00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]

                                     --  (41) (42) (43) (44) (45)
                00   00   00   00    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

{-- To run from a given position within ghci:

    loop (Node (nodeFromGridW boardDebug1) []) 1  -- white to move next
    loop (Node (nodeFromGridB boardDebug1) []) 2  -- black to move next

    loop (Node (nodeFromGridB boardXtoWin) []) 2 -- black to move, at level 6 should see all white wins
    loop (Node (nodeFromGridB blunderBoard1) []) 2
    loop (Node (nodeFromGridB blunderBoard2) []) 2 -- black to move, at levle 6 should see blunder move
--}
