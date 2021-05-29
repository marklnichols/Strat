{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module ChessTest (chessTest) where

import Test.Hspec
import Data.List
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Chess
import GameRunner
import Strat.Helpers
import Strat.StratTree.TreeNode
-- import Strat.ZipTree

chessTest :: SpecWith ()
chessTest = do
    describe "locsForColor" $
        it "Gets the list of indexes of all chess pieces of a given color from the board" $ do
            let (wLocs, bLocs) = locsForColor board01
            wLocs `shouldMatchList`
              [12, 13, 21, 22 ,25, 26, 33, 43, 45, 46, 53, 57]
            bLocs `shouldMatchList`
              [61, 62, 65, 66, 67, 77, 78, 83, 86, 87]
    describe "allowableKingMoves" $
        it "Gets the possible moves for a king" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableKingMoves
                      ( posFromGrid board01 White (12, 87) (False, False)
                        (board01HasMovedW, board01HasMovedB) ) 12
            empties `shouldMatchList` [11, 23]
            enemies `shouldMatchList` []
            let (empties2, enemies2) = pairToIndexes
                  $ allowableKingMoves
                      ( posFromGrid board01 Black (12, 87) (False, False) (board01HasMovedW, board01HasMovedB) ) 87
            empties2 `shouldMatchList` [76, 88]
            enemies2 `shouldMatchList` []
    describe "castleMoves" $ do
        it "Gets the possible castling moves" $do
          let pos = _chessPos castlingNode
              moves = castleMoves pos
              hasCastlingMove mvs =
                  case find (\case
                                StdMove{} -> False
                                CastlingMove{} -> True) mvs of
                  Just _ -> True
                  Nothing -> False

          hasCastlingMove moves `shouldBe` False


    describe "allowableQueenMoves" $
        it "Gets the allowable moves for a queen" $ do
            let (empties, enemies) =  pairToIndexes
                  $ allowableQueenMoves board01 46
            empties `shouldMatchList`
                [47,48 -- left/right
                ,36,56 -- up/down
                ,24,35 -- LL/UR
                ,28,37,55,64,73,82] -- LR, UL
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableQueenMoves board01 62
            empties2 `shouldMatchList`
                [63, 64 -- L/R
                ,32,42,52,72,82 -- U/D
                ,51,73,84 -- LL/UR
                ,71 ] -- LR/UL
            enemies2 `shouldMatchList` [22, 53]
    describe "allowableRookMoves" $
        it "Gets the allowable moves for a rook" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableRookMoves board01 13
            empties `shouldMatchList` [14,15,16,17,18 -- L/R
                                      ,23] -- U/D
            enemies `shouldMatchList` []
            let (empties2, enemies2) = pairToIndexes
                  $ allowableRookMoves board01 83
            empties2 `shouldMatchList` [81,82,84,85 -- L/R
                                       ,73,63] -- U/D
            enemies2 `shouldMatchList` [53]
    describe "allowableBishopMoves" $
        it "Gets the allowable moves for a bishop" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableBishopMoves board01 43
            empties `shouldMatchList` [32,54 -- LL/UR
                                      ,34,52] -- LR/UL
            enemies `shouldMatchList` [61, 65]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableBishopMoves board01 65
            empties2 `shouldMatchList` [54,76 -- LL/UR
                                       ,38,47,56,74] -- LR/UL
            enemies2 `shouldMatchList` [43]
    describe "allowableKnightMoves" $
        it "Gets the allowable moves for a knight" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableKnightMoves board01 45
            empties `shouldMatchList` [24,37,64]
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableKnightMoves board01 53
            empties2 `shouldMatchList` [32,34,41,72,74]
            enemies2 `shouldMatchList` [61,65]
    describe "allowablePawnNonCaptures" $
        it "Gets the allowable (non capturing) moves for a pawn" $ do
            let f m = (_startIdx m, _endIdx m)
            f <$> allowablePawnNonCaptures board01 22 `shouldMatchList` [(22,32),(22,42)]
            f <$> allowablePawnNonCaptures board01 33 `shouldMatchList` []
            f <$> allowablePawnNonCaptures board01 61 `shouldMatchList` [(61, 51)]
            f <$> allowablePawnNonCaptures startingBoard 25 `shouldMatchList` [(25, 35), (25, 45)]
    describe "allowablePawnCaptures" $
        it "Gets the allowable capturing moves for a pawn" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowablePawnCaptures board01 57
            empties `shouldMatchList` [68]
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowablePawnCaptures board01 78
            empties2 `shouldMatchList` []
            enemies2 `shouldMatchList` []
    describe "allowableEnPassant" $
        it "Gets the allowable enPassant capturing moves for a pawn" $
            _endIdx <$> allowableEnPassant board01 78 `shouldMatchList` [57]
    describe "calcMoveListGrid" $
        it "gets all possible moves from a grid, for a given color" $ do
            let f m = (_startIdx m, _endIdx m)
            let moves = calcMoveLists (posFromGrid board02 White (15, 85) (False, False)
                                       (board02HasMovedW, board02HasMovedB) )
            let emptyAndEnemy = _cmEmpty moves ++ _cmEnemy moves
            f <$> emptyAndEnemy `shouldMatchList`
               [ (11,12), (13,24), (13,35), (13,46), (13,57), (13,68), (14,24), (14,25), (14,36)
               , (14,47), (14,58), (15,24), (15,25), (16,25), (17,25), (17,36), (17,38), (21,31)
               , (21,41), (22,32), (22,42), (34,44), (34,45), (26,36), (26,46), (27,37), (27,47)
               , (28,38), (28,48), (33,12), (33,41), (33,52), (33,54), (33,45), (33,25) ]
    describe "checkCastling" $
        it ("Checks and possibly updates the castling state and the set of unmoved pieces tracked"
           ++ "for castling purposes") $ do
            case checkCastling White board03 board03HasMovedW board03Move of
                Just (_moved, castling) -> castling `shouldBe` KingSideOnlyAvailable
                _ -> error "expecting Just"

            case checkCastling Black board03 board03HasMovedB board03Move2 of
                Just (_moved, castling) -> castling `shouldBe` QueenSideOnlyAvailable
                _ -> error "expecting Just"

            case checkCastling White board03 board03HasMovedW board03Move3 of
                Just (_moved, castling) -> castling `shouldBe` Unavailable
                _ -> error "expecting Just"

            case checkCastling White board03 board03HasMovedW board03Move4 of
                Just (_moved, castling) -> castling `shouldBe` Castled
                _ -> error "expecting Just"

    describe "countMaterial" $
      it "Calculates a score for the position based on the pieces on the board for each side" $
          countMaterial board01 `shouldBe` 6

    describe "calcDevelopment" $
      it ("Calculates a score for the position based on the development of the minor pieces "
          ++ "(aka knight, bishop) for each side") $
          calcDevelopment (posFromGrid board03 White (15, 85) (False, False)
                           (board03HasMovedW, board03HasMovedB)) `shouldBe` 1
    describe "inCheck" $
      it "Determines if the King at a given loc is in check from any enemy pieces" $ do
          inCheck board03 White 15 `shouldBe` False
          inCheck board04 Black 85 `shouldBe` False
          inCheck board04 White 45 `shouldBe` True

    describe "moveChecksOpponent" $
      it "Determines if a move results in the opposing King being in check" $ do
          let mv = StdMove { _isExchange = True, _startIdx = 55, _endIdx = 53, _stdNote = "" }
          moveChecksOpponent discoveredCheckNode mv `shouldBe` True

    describe "findMove" $
      it ("find's a subtree element corresponding to a particular move from the current position"
         ++ " (this test: determine an opening move is correctly found in the starting position)") $ do
          -- startingBoard :: V.Vector Char
          let t = getStartNode "newgame"
          let newTree = expandTree t 2 2
          let mv = StdMove { _isExchange = False, _startIdx = 25, _endIdx = 45, _stdNote = "" }
          let t' = findMove newTree mv
          (t /= t') `shouldBe` True

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------
posFromGrid :: Vector Char -> Color -> (Int, Int) -> (Bool, Bool) -> (HasMoved, HasMoved) -> ChessPos
posFromGrid g c (kingLocW, kingLocB) (inCheckW, inCheckB) (hasMovedW, hasMovedB) = ChessPos
  { _cpGrid = g, _cpColor = c
  , _cpHasMoved = (hasMovedW, hasMovedB)
  , _cpKingLoc = (kingLocW, kingLocB)
  , _cpInCheck = (inCheckW, inCheckB)
  , _cpFin = NotFinal }

---------------------------------------------------------------------------------------------------
-- Test board positions
{-
    Piece representation as integers:

    75 = 0x4b = 'K' = white King     107 = 0x6b = 'k' = black King
    81 = 0x51 = 'Q' = white Queen    113 = 0x71 = 'q' = black Queen
    82 = 0x52 = 'R' = white Rook     114 = 0x72 = 'r' = black Rook
    78 = 0x4e = 'N' = white Knight   110 = 0x6e = 'n' = black Knight
    66 = 0x42 = 'B' = white Bishop   098 = 0x62 = 'b' = black Bishop
    80 = 0x50 = 'P' = white Pawn     112 = 0x70 = 'p' = black Pawn

    32 = 0x20 = ' ' = Empty Squre
    00 = 0x00 = n/a = offBoard
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
board01HasMovedW :: HasMoved
board01HasMovedW = HasMoved
  { _unMovedDev = S.fromList []
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling =  S.fromList []
  , _castlingState = Castled
  , _unMovedQueen = False }

board01HasMovedB :: HasMoved
board01HasMovedB = HasMoved
  { _unMovedDev = S.fromList []
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling =  S.fromList []
  , _castlingState = Castled
  , _unMovedQueen = False }

----------------------------------------------------------------------------------------------------
board02 :: V.Vector Char
board02 = V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'q',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  ' ',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   -   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   q   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   p   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   P   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}
board02HasMovedW :: HasMoved
board02HasMovedW = HasMoved
  { _unMovedDev = S.fromList [wKB, wQB, wQN]
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling = S.fromList [wK, wKR, wQR]
  , _castlingState = BothAvailable
  , _unMovedQueen = False }

board02HasMovedB :: HasMoved
board02HasMovedB = HasMoved
  { _unMovedDev = S.fromList [bKN, bKB, bQB, bQN]
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling =  S.fromList [bK, bKR, bQR]
  , _castlingState = BothAvailable
  , _unMovedQueen = False }

----------------------------------------------------------------------------------------------------
board03 :: V.Vector Char
board03 = V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  ' ',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'Q',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  'B',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  'q',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  ' ',  'k',  'b',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   -   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   q   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   p   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   Q   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   -   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}
board03HasMovedW :: HasMoved
board03HasMovedW = HasMoved
  { _unMovedDev = S.fromList [wKB, wKN]
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling = S.fromList [wK, wKR, wQR]
  , _castlingState = BothAvailable
  , _unMovedQueen = False }

board03HasMovedB :: HasMoved
board03HasMovedB = HasMoved
  { _unMovedDev = S.fromList [bKB, bQB, bQN]
  , _unMovedCenterPawns = S.fromList []
  , _unMovedCastling =  S.fromList [bK, bKR, bQR]
  , _castlingState = BothAvailable
  , _unMovedQueen = False }

board03Move :: ChessMove
board03Move = StdMove { _isExchange = False, _startIdx = 11, _endIdx = 12, _stdNote = ""}

board03Move2 :: ChessMove
board03Move2 = StdMove { _isExchange = False, _startIdx = 88, _endIdx = 87, _stdNote = "" }

board03Move3 :: ChessMove
board03Move3 = StdMove { _isExchange = False, _startIdx = 15, _endIdx = 24, _stdNote = "" }

board03Move4 :: ChessMove
board03Move4 = CastlingMove { _castle = QueenSide, _kingStartIdx = 15, _kingEndIdx = 13
                           , _rookStartIdx = 11, _rookEndIdx = 15, _castleNote = "O-O-O"}

----------------------------------------------------------------------------------------------------
board04 :: V.Vector Char
board04 = V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'K',  'B',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'b',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   k   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   b   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   K   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}



----------------------------------------------------------------------------------------------------
_boardTemplate :: V.Vector Char
_boardTemplate = V.fromList [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

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
