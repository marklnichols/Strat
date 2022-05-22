{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest (checkersTest) where

import Checkers
import Parser8By8
import Control.Lens
import Data.Either
import Data.Tree
import Strat.StratTree.TreeNode
import Test.Hspec
import qualified CheckersJson as J
import qualified Data.Vector.Unboxed as V


-- dummy value of TreeLocation, used for most tests
tl0 :: TreeLocation
tl0 = TreeLocation {tlDepth = 0}

checkersTest :: SpecWith ()
checkersTest = do
    describe "getAllowedMoves" $
        it "Gets the list of allowed moves for a given color from a given position." $ do
            getAllowedMoves (rootLabel (getStartNode "new_game")) `shouldMatchList` fmap mkSimpleCkMove [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getAllowedMoves blackFirstStartNode `shouldMatchList` fmap mkSimpleCkMove [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves

            getAllowedMoves (nodeFromGridW board01) `shouldMatchList` fmap mkSimpleCkMove    [510, 1721, 1722, 2832, 2833, 2823, 2824, 3934, 3935]
            getAllowedMoves (nodeFromGridB board01) `shouldMatchList` fmap mkSimpleCkMove    [3732, 3733, 3025, 3026, 2024, 2025, 2015, 2016, 711, 712]
            getAllowedMoves (nodeFromGridW board02) `shouldMatchList` mkMultiCkJump m02Multi : fmap mkSimpleCkJump [(0717, 12), (1624, 20), (2515, 20), (2517, 21), (2535, 30)]

            getAllowedMoves (nodeFromGridB board02) `shouldMatchList` fmap mkSimpleCkJump [(2111, 16), (3729, 33)]
            getAllowedMoves (nodeFromGridW board06) `shouldMatchList` fmap mkSimpleCkJump [(2535, 30)]
            MoveEntry <$> getAllowedMoves (nodeFromGridW board07) `shouldMatchList`
              snd (partitionEithers (fmap (parseCkMove (nodeFromGridW board07))
                                          ["C1-A3-C5", "C1-E3-C5", "C1-E3-G5-E7"]))
    describe "calcNewNode" $
        it "creates a new node from a previous position and a move" $ do
            calcNewNode (nodeFromGridW board01) (mkSimpleCkMove m1) tl0 ^. (ckPosition . grid) `shouldBe` board01_m1
            calcNewNode (nodeFromGridB board02) (mkSimpleCkJump m2) tl0 ^. (ckPosition . grid) `shouldBe` board02_m2
            calcNewNode (nodeFromGridW board03) (mkMultiCkJump m3) tl0 ^. (ckPosition . grid) `shouldBe` board03_m3
    describe "pieceCount" $
        it "Counts the number of white regular pieces minus the black regular pieces" $
            totalPieceCount (unGrid board04) `shouldBe` -2
    describe "kingCount" $
        it "Counts the number of white king pieces minus the black king pieces" $
            totalKingCount (unGrid board04) `shouldBe` 4
    describe "parseCkMove" $
        it "parses move input into a CkMove" $ do
            parseCkMove (nodeFromGridW board01) "A1 B2" `shouldBe` Right (MoveEntry (mkSimpleCkMove 510))
            parseCkMove (nodeFromGridW board01) "A1-b2" `shouldBe` Right (MoveEntry (mkSimpleCkMove 510))
            parseCkMove (nodeFromGridW board02) "E1 G3" `shouldBe` Right (MoveEntry (mkSimpleCkJump (0717, 12)))
            parseCkMove (nodeFromGridW board03) "E5 G3 E1" `shouldBe` Right (MoveEntry (mkMultiCkJump m3))
    describe "toParserMove" $
        it "converts a CkMove to a Parser Move (for display)" $ do
            toParserMove (mkSimpleCkMove 510) `shouldBe` Move [Loc 'A' 1,  Loc 'B' 2]
            toParserMove (mkSimpleCkJump (0717, 12)) `shouldBe` Move [Loc 'E' 1, Loc 'G' 3]
            toParserMove (mkMultiCkJump m3) `shouldBe` Move [Loc 'E' 5, Loc 'G' 3, Loc 'E' 1]
    describe "checkPromote" $
        it "promotes a piece to king if it has reached the back row" $ do
            checkPromote (nodeFromGridW board05) 01 38 `shouldBe` 2
            checkPromote (nodeFromGridB board05) (-1) 07 `shouldBe` (-2)
            checkPromote (nodeFromGridW board05) 01 25 `shouldBe` 1
    describe "checkFinal" $
        it "checks to see if the game is over" $ do
            checkFinal (nodeFromGridW board09) `shouldBe` BWins
            checkFinal (nodeFromGridB board09) `shouldBe` NotFinal
            checkFinal (nodeFromGridW board09b) `shouldBe` NotFinal
            checkFinal (nodeFromGridB board09b) `shouldBe` WWins
            checkFinal (nodeFromGridB board11) `shouldBe` WWins
            checkFinal (nodeFromGridW board12) `shouldBe` BWins
    describe "mobility" $
        it "determine the mobility of each side" $ do
            mobility (nodeFromGridW board01) `shouldBe` -1
            mobility (nodeFromGridB board03) `shouldBe`  7
    describe "homeRow'" $
        it "checks to see if the home row is full or if two no adjacent squares are occupied" $ do
            homeRow' (unGrid board01) 1 `shouldBe` homeRowNone
            homeRow' (unGrid board01) (-1) `shouldBe` homeRowNone
            homeRow' (unGrid board10) 1 `shouldBe` homeRowPartial
            homeRow' (unGrid board10) (-1) `shouldBe` homeRowPartial
            homeRow' (unGrid board08) 1 `shouldBe` homeRowFull
            homeRow' (unGrid board08) (-1) `shouldBe` homeRowFull
    describe "pieceProgress" $
        it "calculates the sum of values corresponding to how far non-king piece has progressed down the board" $ do
            pieceProgress [15, 33] 1 `shouldBe` 3
            pieceProgress [15, 33] (-1) `shouldBe` 2
    describe "progress" $
        it "evaluates pieceProgress of white pieces vs pieceProgress of black pieces" $ do
            progress (nodeFromGridW evalBoard01) `shouldBe` -1
            progress (nodeFromGridB evalBoard01) `shouldBe` -1
            progress (nodeFromGridW evalBoard02) `shouldBe` 0
            progress (nodeFromGridB blunderBoard0) `shouldBe` 0
    describe "closestToKing" $
        it "Finds the opposing piece closest to a given king" $ do
            closestToKing 5 [8, 21] `shouldBe` 5
            closestToKing 32 [40, 17, 13] `shouldBe` 6
    describe "kingProximity" $
        it ("evaluates white king farthest from opposing pieces vs. "
                ++ "black king farthest from opposing pieces") $ do
            kingProximity (nodeFromGridW board01) `shouldBe` 0
            kingProximity (nodeFromGridW board11) `shouldBe` 0
            kingProximity (nodeFromGridW board09b) `shouldBe` (-6)
            kingProximity (nodeFromGridW blunderBoard0) `shouldBe` 3
            kingProximity (nodeFromGridB blunderBoard0) `shouldBe` 3
    describe "jsonToCkMove" $
        it "converts a Json move into a CKMove" $ do
            J.jsonToCkMove (mkSimpleJsonMove m1) `shouldBe` Just (mkSimpleCkMove m1)
            J.jsonToCkMove (mkSimpleJsonJump m2) `shouldBe` Just (mkSimpleCkJump m2)
            J.jsonToCkMove (mkMultiJsonJump m3) `shouldBe` Just (mkMultiCkJump m3)

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------
blackFirstStartNode :: CkNode
blackFirstStartNode = rootLabel (getStartNode "new_game") & ckPosition.clr .~ (-1)

treeFromGridW :: CkGrid -> Tree CkNode
treeFromGridW g = Node CkNode
    { _ckTreeLoc = TreeLocation {tlDepth  = 0}
    , _ckMove = mkSimpleCkMove (-1)
    , _ckValue = CkEval {_total = 0, _details = ""}
    , _ckErrorValue = CkEval {_total = 0, _details = ""}
    , _ckPosition = CkPosition {_grid = g, _clr = 1, _fin = NotFinal}
    , _ckIsEvaluated = False } []

treeFromGridB :: CkGrid -> Tree CkNode
treeFromGridB g = Node CkNode
    { _ckTreeLoc = TreeLocation {tlDepth  = 0}
    , _ckMove = mkSimpleCkMove (-1)
    , _ckValue = CkEval {_total = 0, _details = ""}
    , _ckErrorValue = CkEval {_total = 0, _details = ""}
    , _ckPosition = CkPosition {_grid = g, _clr = -1, _fin = NotFinal}
    , _ckIsEvaluated = False } []

nodeFromGridW :: CkGrid -> CkNode
nodeFromGridW g = rootLabel $ treeFromGridW g

nodeFromGridB :: CkGrid -> CkNode
nodeFromGridB g = rootLabel $ treeFromGridB g

mkSimpleCkMove :: Int -> CkMove
mkSimpleCkMove i = CkMove {_isJump = False, _startIdx = i `div` 100, _endIdx = i `mod` 100, _middleIdxs = [], _removedIdxs = []}

mkSimpleJsonMove :: Int -> String
mkSimpleJsonMove x = J.jsonFromCkMove (mkSimpleCkMove x)

mkSimpleCkJump :: (Int, Int) -> CkMove
mkSimpleCkJump (mv, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = [], _removedIdxs = [removed]}

mkSimpleJsonJump :: (Int, Int) -> String
mkSimpleJsonJump (mv, removed) = J.jsonFromCkMove (mkSimpleCkJump (mv, removed))

mkMultiCkJump :: (Int, [Int], [Int]) -> CkMove
mkMultiCkJump (mv, middle, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = middle, _removedIdxs = removed}

mkMultiJsonJump :: (Int, [Int], [Int]) -> String
mkMultiJsonJump (mv, middle, removed) = J.jsonFromCkMove (mkMultiCkJump (mv, middle, removed))

---------------------------------------------------------------------------------------------------
-- Test board positions
---------------------------------------------------------------------------------------------------
board01 :: CkGrid
board01 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -02, 00, 00
  , 00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]

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
m1 :: Int
m1 = 1722

board01_m1 :: CkGrid  --board01 with move m1 applied
board01_m1 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, -02, 00, 01
  , 00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
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
m02Multi :: (Int, [Int], [Int])
m02Multi = (1634, [26], [21, 30])     --- 16 -> 26 -> 34 jumping 21 and 30

board02 :: CkGrid
board02 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00
  , 00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
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
m2 :: (Int, Int)
m2 = (3729, 33)

board02_m2 :: CkGrid --board02 with move m2 applied
board02_m2 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00
  , 00, 00, 02, 00, 99, 02, -1, -1, 00, 01, 00, 00, 00, 99, 00, 00, 02, 00, 99, 99, 99, 99, 99]
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

board03 :: CkGrid
board03 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, 00, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00
  , 00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
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
m3 :: (Int, [Int], [Int])
m3 = (2507, [17], [21, 12])     --- 25 -> 17 -> 7 jumping 21 and 12

board03_m3 :: CkGrid
board03_m3 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, 02, 01, 99, 00, 00, 00, 00, 00, 00, 01, 00, 99, 00, -02, 00, 00
  , 00, 00, 00, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
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

board04 :: CkGrid
board04 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 01, 00, 02, 99, 00, -1, 00, 00, -1, 00, 00, 00, 99, 02, 00, 00, 00
  , 00, 00, 00, 00, 99, 00, -2, 00, 00, 00, 02, 00, -1, 99, 02, 00, 00, 02, 99, 99, 99, 99, 99]

board05 :: CkGrid
board05 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, -1, 00, 00, 00, 00, 00, 00, 99, 00, 00, 01, 00
  , 00, -1, 00, 00, 99, 00, 00, 00, 00, 00, 01, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]

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

board06 :: CkGrid
board06 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, -1, 00, 00, 99, 00, 00, 00, 00
  , 00, 00, 01, 00, 99, 00, 01, -1, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
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

board07 :: CkGrid
board07 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 01, 00, 00, 99, -1, -1, 00, 00, 00, 00, 00, 00, 99, -1, -1, -1, 00
  , 00, 00, 00, 00, 99, 00, 00, -1, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
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

board08 :: CkGrid
board08 =  CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 01, 01, 01, 99, 01, 01, 00, 01, -1, 00, 01, 00, 99, 00, 00, 00, 00
  , -1, 00, -1, 00, 99, 01, 00, 00, -1, 00, 00, 00, -1, 99, -1, -1, -1, -1, 99, 99, 99, 99, 99]
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

board09 :: CkGrid
board09 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00
  , 00, 00, 00, 00, 99, 00, 00, 00, 00, 01, 00, 00, 00, 99, -1, 00, 00, 00, 99, 99, 99, 99, 99]
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

board09b :: CkGrid
board09b =  CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 01, 99, 00, 00, 00, -2, 00, 00, 00, 01, 99, 00, 00, 01, 00
  , 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
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

board10 :: CkGrid
board10 =  CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 01, 01, 00, 01, -1, 00, 01, 00, 99, 00, 00, 00, 00
  , -1, 00, -1, 00, 99, 01, 00, 00, -1, 00, 00, 00, -1, 99, -1, 00, -1, 00, 99, 99, 99, 99, 99]
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

board11 :: CkGrid
board11 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, 00, 00, 00
  , 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 02, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
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

board12 :: CkGrid
board12 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00
  , 00, -1, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
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

blunderBoard0 :: CkGrid
blunderBoard0 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, -2, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, -2, 02
  , 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 02, 00, 00, 00, 99, 99, 99, 99, 99]
{--
                                     --  (41) (42) (43) (44) (45)
                02   00   00   00    --     37   38   39   40
              00  00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31
              00   00   00   00      --   23   24   25   26      (27)
                00   00   -2   02    --     19   20   21   22
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13
              -2   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

evalBoard01 :: CkGrid
evalBoard01 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 00, 01, 00, 01, 99, 01, 00, 01, 01, 01, 01, 01, 01, 99, 01, -1, 01, 00
  , -1, -1, 00, -1, 99, -1, -1, 00, -1, 00, 00, 00, 00, 99, -1, -1, -1, -1, 99, 99, 99, 99, 99]
{-
                                     --  (41) (42) (43) (44) (45)
                -1   -1   -1   -1    --     37   38   39   40
              00   00   00   00      --   32   33   34   35      (36)
                -1   -1   00   -1    --     28   29   30   31
              -1   -1   00   -1      --   23   24   25   26      (27)
                01   -1   01   00    --     19   20   21   22
              01   01   01   01      --   14   15   16   17      (18)
                01   00   01   01    --     10   11   12   13
              00   01   00   01      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

evalBoard02 :: CkGrid
evalBoard02 = CkGrid $ V.fromList
  [ 99, 99, 99, 99, 99, 01, 01, 01, 01, 99, 01, 00, 01, 01, 01, 00, 01, 01, 99, 00, -1, 00, 00
  , 01, -1, 00, 00, 99, -1, -1, 00, -1, -1, 00, 00, -1, 99, -1, -1, -1, -1, 99, 99, 99, 99, 99]
{-
                                     --  (41) (42) (43) (44) (45)
                -1   -1   -1   -1    --     37   38   39   40
              -1  00   00   -1      --   32   33   34   35      (36)
                -1   -1   00   -1    --     28   29   30   31
              01  -1   00   00      --   23   24   25   26      (27)
                00   -1   00   00    --     19   20   21   22
              01   00   01   01      --   14   15   16   17      (18)
                01   00   01   01    --     10   11   12   13
              01   01   01   01      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
-}

{--
H     o     o     o     o
G  o     -     -     o
F     o     o     -     o
E  x     o     -     -
D     -     o     -     -
C  x     -     x     x
B     x     -     x     x
A  x     x     x     x

   1  2  3  4  5  6  7  8
Current position score: Total 1 made up of mat<0> mob<-1> home<0> prog<2>
--}

{--
board0n :: CkGrid
board0n = CkGrid $ V.fromList
[ 99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00
, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]

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
