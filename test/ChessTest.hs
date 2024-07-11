{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ChessTest (chessTest) where

import Test.Hspec
import Control.Monad.Reader
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Tree (rootLabel, Tree)
import Data.Tuple.Extra (fst3)
import qualified Data.Vector.Unboxed as V

import Chess
import Strat.Helpers
import Strat.StratIO
import Strat.StratTree.TreeNode
import qualified Strat.ZipTree as Z
import System.Random hiding (next)
import Text.Printf

--TODO: look into the preSort (lack of) performance problems -- disabled for now
testEnv :: Z.ZipTreeEnv
testEnv = Z.ZipTreeEnv
        { verbose = False
        , enablePruning = True
        , enablePruneTracing = False
        , singleThreaded = True
        , enableCmpTracing = False
        , enableRandom = False
        , maxRandomChange = 10.0
        , enablePreSort = False
        , moveTraceStr = T.pack ""
        , maxDepth = 5
        , maxCritDepth = 5
        , aiPlaysWhite = True
        , aiPlaysBlack = True
        }

chessTest :: SpecWith ()
chessTest = do

    describe "invertGrid" $
      it "Inverts the rows of the grid" $ do
        invertGrid board01 `shouldBe` invertedBoard01

    describe "locsForColor" $
        it "Gets the list of indexes of all chess pieces of a given color from the board" $ do
            let (wLocs, bLocs) = locsForColor (posFromGrid board01 White (12, 87) False)
            fst3 <$> wLocs `shouldMatchList`
              [12, 13, 21, 22 ,25, 26, 33, 43, 45, 46, 53, 57]
            fst3 <$> bLocs `shouldMatchList`
              [61, 62, 65, 66, 67, 77, 78, 83, 86, 87]
    describe "allowableKingMoves" $
        it "Gets the possible moves for a king" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableKingMoves
                      ( mkTestPos board01 White (testStateCastled White) (12, 87) False)
                      (12, 'K', White)
            empties `shouldMatchList` [11, 23]
            enemies `shouldMatchList` []
            let (empties2, enemies2) = pairToIndexes
                  $ allowableKingMoves
                      ( mkTestPos board01 Black (testStateCastled Black) (12, 87) False )
                      (87, 'k', Black)
            empties2 `shouldMatchList` [76, 88]
            enemies2 `shouldMatchList` []
    describe "castleMoves" $ do
        it "Gets the possible castling moves" $ do
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
                  $ allowableQueenMoves board01 (46, 'Q', White)
            empties `shouldMatchList`
                [47,48 -- left/right
                ,36,56 -- up/down
                ,24,35 -- LL/UR
                ,28,37,55,64,73,82] -- LR, UL
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableQueenMoves board01 (62, 'q', Black)
            empties2 `shouldMatchList`
                [63, 64 -- L/R
                ,32,42,52,72,82 -- U/D
                ,51,73,84 -- LL/UR
                ,71 ] -- LR/UL
            enemies2 `shouldMatchList` [22, 53]
    describe "allowableRookMoves" $
        it "Gets the allowable moves for a rook" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableRookMoves board01 (13, 'R', White)
            empties `shouldMatchList` [14,15,16,17,18 -- L/R
                                      ,23] -- U/D
            enemies `shouldMatchList` []
            let (empties2, enemies2) = pairToIndexes
                  $ allowableRookMoves board01 (83, 'r', Black)
            empties2 `shouldMatchList` [81,82,84,85 -- L/R
                                       ,73,63] -- U/D
            enemies2 `shouldMatchList` [53]
    describe "allowableBishopMoves" $
        it "Gets the allowable moves for a bishop" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableBishopMoves board01 (43, 'B', White)
            empties `shouldMatchList` [32,54 -- LL/UR
                                      ,34,52] -- LR/UL
            enemies `shouldMatchList` [61, 65]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableBishopMoves board01 (65, 'b', Black)
            empties2 `shouldMatchList` [54,76 -- LL/UR
                                       ,38,47,56,74] -- LR/UL
            enemies2 `shouldMatchList` [43]
    describe "allowableKnightMoves" $
        it "Gets the allowable moves for a knight" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowableKnightMoves board01 (45, 'N', White)
            empties `shouldMatchList` [24,37,64]
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowableKnightMoves board01 (53, 'N', White)
            empties2 `shouldMatchList` [32,34,41,72,74]
            enemies2 `shouldMatchList` [61,65]
    describe "allowablePawnNonCaptures" $
        it "Gets the allowable (non capturing) moves for a pawn" $ do
            let f m = (_startIdx m, _endIdx m)
            f <$> allowablePawnNonCaptures board01 (22, 'P', White) `shouldMatchList` [(22,32),(22,42)]
            f <$> allowablePawnNonCaptures board01 (33, 'P', White) `shouldMatchList` []
            f <$> allowablePawnNonCaptures board01 (61, 'p', Black) `shouldMatchList` [(61, 51)]
            f <$> allowablePawnNonCaptures startingBoard (25, 'P', White) `shouldMatchList` [(25, 35), (25, 45)]
    describe "allowablePawnCaptures" $
        it "Gets the allowable capturing moves for a pawn" $ do
            let (empties, enemies) = pairToIndexes
                  $ allowablePawnCaptures board01 (57, 'P', White)
            empties `shouldMatchList` [68]
            enemies `shouldMatchList` [66]
            let (empties2, enemies2) = pairToIndexes
                  $ allowablePawnCaptures board01 (78, 'p', Black)
            empties2 `shouldMatchList` []
            enemies2 `shouldMatchList` []
    describe "allowableEnPassant" $
        it "Gets the allowable enPassant capturing moves for a pawn" $ do
            _epEndIdx <$> allowableEnPassant (posFromGridEnPassant enPassantBoard01
                                           White (Just 68) (12, 87) False)
                                           (57, 'P', White) `shouldMatchList` [68]
            _epEndIdx <$> allowableEnPassant (posFromGridEnPassant enPassantBoard01
                                           Black (Just 32) (12, 87) False)
                                           (41, 'p', Black) `shouldMatchList` [32]
            -- no enpassant in state:
            _epEndIdx <$> allowableEnPassant (posFromGridEnPassant enPassantBoard01
                                           White Nothing (12, 87) False)
                                           (57, 'P', White) `shouldMatchList` []
    describe "calcMoveListGrid" $
        it "gets all possible moves from a grid, for a given color" $ do
            let f m = (_startIdx m, _endIdx m)
            let moves = calcMoveLists (posFromGrid board02 White (15, 85) False
                                        )
            let emptyAndEnemy = _cmEmpty moves ++ _cmEnemy moves
            f <$> emptyAndEnemy `shouldMatchList`
               [ (11,12), (13,24), (13,35), (13,46), (13,57), (13,68), (14,24), (14,25), (14,36)
               , (14,47), (14,58), (15,24), (15,25), (16,25), (17,25), (17,36), (17,38), (21,31)
               , (21,41), (22,32), (22,42), (34,44), (34,45), (26,36), (26,46), (27,37), (27,47)
               , (28,38), (28,48), (33,12), (33,41), (33,52), (33,54), (33,45), (33,25) ]

    describe "castlingStatus" $
        it "Checks  the castling state of each side" $ do
            fst (castlingStatus (posFromGrid board03a White (15, 85) False))
                `shouldBe` KingSideOnlyAvailable
            snd (castlingStatus (posFromGrid board03a Black (15, 85) False))
                `shouldBe` QueenSideOnlyAvailable
            fst (castlingStatus (posFromGrid board03b White (24, 87) False))
                `shouldBe` Unavailable
            snd (castlingStatus (posFromGrid board03b Black (24, 87) False))
                `shouldBe` Castled
            fst (castlingStatus (posFromGrid board03c White (15, 85) False))
                `shouldBe` BothAvailable
            snd (castlingStatus (posFromGrid board03c Black (15, 85) False))
                `shouldBe` BothAvailable

    describe "castlingAvailable" $
        it "Checks castling availability for one player" $ do
            castlingAvailable (mkTestPos board03a White (testStateKSOnly White) (15, 85) False) White
               `shouldBe` Unavailable
            castlingAvailable (mkTestPos board03a Black (testStateQSOnly Black) (15, 85) False) Black
                `shouldBe` Unavailable
            castlingAvailable (mkTestPos board03b White (testStateUnavail White) (24, 87) False) White
                `shouldBe` Unavailable
            castlingAvailable (mkTestPos board03b Black (testStateCastled Black) (24, 87) False) Black
                `shouldBe` Castled
            castlingAvailable (mkTestPos board03c White (testStateQSOnly White) (15, 85) False) White
                `shouldBe` QueenSideOnlyAvailable
            castlingAvailable (mkTestPos board03c Black (testState Black) (15, 85) False) Black
                `shouldBe` KingSideOnlyAvailable

    describe "countMaterial" $
      it "Calculates a score for the position based on the pieces on the board for each side" $
          countMaterial board01 `shouldBe` 6

    describe "calcDevelopment" $
      it ("Calculates a score for the position based on the development of the minor pieces "
          ++ "(aka knight, bishop) for each side") $
          calcDevelopment (posFromGrid board03a White (15, 85) False
                           ) `shouldBe` 1

    describe "calcCenterPawnScore" $
      it "Calculates a score for the two center pawns of each side" $ do
        calcCenterPawnScore [24, 45] [64, 75] `shouldBe` 6.0
        calcCenterPawnScore [34, 35] [54, 55] `shouldBe` -12.0

    describe "calcKnightPawnScore" $
      it "Calculates a score tor the two knight pawns of each side" $
        calcKnightPawnScore board09 [32, 37] [52, 57] `shouldBe` 5

    describe "calcRookPawnScore (pawns on R3)" $
      it "Calculates a score for the two rook pawns of each side, focusing on the R3 squares" $
        calcRookPawnScore board10R3 [31, 38] [61, 78] `shouldBe` -1

    describe "calcRookPawnScore (pawns on R4)" $
      it "Calculates a score for the two rook pawns of each side, focusing on the R4 squares" $
        calcRookPawnScore board10R4 [41, 28] [51, 58] `shouldBe` 1

    describe "bishopPawnScore" $
      it "Calculates a score for the two bishop pawns of each side" $ do
        calcBishopPawnScore [23, 26] [63, 56] `shouldBe` 3.0
        calcBishopPawnScore [33, 36] [53, 56] `shouldBe` 2.0

    describe "calcPawnPositionScore" $
      it ("Calculates a score for the position based on pawn positioning"
          ++ " for each side") $ do
          calcPawnPositionScore (posFromGrid board05 White (15, 85) False) `shouldBe` 15
          calcPawnPositionScore (posFromGrid board06 White (15, 85) False) `shouldBe` (-15)
          calcPawnPositionScore (posFromGrid pQ4pQ4Board White (15, 85) False) `shouldBe` 0
          calcPawnPositionScore (posFromGrid pQ4pQ4Board Black (15, 85) False) `shouldBe` 0

    describe "dirLocsCount" $
      it ("Counts the number of moves available for a given piece, in a"
         ++ " given position, in a given direction") $ do
        dirLocsCount board08 (81, 'r', Black) right `shouldBe` 1
        dirLocsCount board08 (83, 'b', Black) diagDL `shouldBe` 1
        dirLocsCount board08 (13, 'B', White) diagUR `shouldBe` 5
        dirLocsCount board08 (14, 'Q', White) up `shouldBe` 2

    describe "dirLocsSingleCount" $
      it ("Is similar to dirLocsCount, but for only a single move in a given"
         ++ " direction -- i.e. used for king and knignt and only returns 0 or 1") $ do
         dirLocsSingleCount board08 (15, 'K' ,White ) up `shouldBe` 1
         dirLocsSingleCount board08 (15, 'K' ,White ) diagUL `shouldBe` 1
         dirLocsSingleCount board08 (15, 'K' ,White ) right `shouldBe` 0
         dirLocsSingleCount board08 (85, 'k' ,Black ) down `shouldBe` 0
         dirLocsSingleCount board08 (61, 'n' ,Black ) knightUR `shouldBe` 1
         dirLocsSingleCount board08 (66, 'n' ,Black ) knightDL `shouldBe` 1

    describe "queenMobility" $
      it ("Calculates the number of moves available for a queen given"
          ++ "a location on the board") $ do
         queenMobility (posFromGrid board08 White (15, 85) False) (14, 'Q', White) `shouldBe` 6
         queenMobility (posFromGrid board08 White (15, 85) False) (84, 'q', Black) `shouldBe` 0

    describe "rookMobility" $
      it ("Calculates the number of moves available for a rook given"
          ++ "a location on the board") $ do
         rookMobility board08 (12, 'R', White) `shouldBe` 1
         rookMobility board08 (18, 'R', White) `shouldBe` 0
         rookMobility board08 (81, 'r', Black) `shouldBe` 1
         rookMobility board08 (88, 'r', Black) `shouldBe` 1

    describe "calcMobility" $
      it ("Calculates a score for the position based on the number of moves "
          ++ "available to each side") $
          calcMobility (posFromGrid board08 White (15, 85) False
                           ) `shouldBe` 14  -- (25W - 11b = 14)

    describe "connectedRooks" $
      it "Determine if a side has two connected rooks" $ do
          connectedRooks (posFromGrid board07 White (12, 87) False) White `shouldBe` False
          connectedRooks (posFromGrid board07 White (12, 87) False) Black `shouldBe` False
          connectedRooks (posFromGrid board01 White (12, 87) False) White `shouldBe` False
          connectedRooks (posFromGrid board01 White (12, 87) False) Black `shouldBe` True
          connectedRooks (posFromGrid board11 White (15, 82) False) White `shouldBe` True
          connectedRooks (posFromGrid board11 White (15, 82) False) Black `shouldBe` False
          connectedRooks (mkTestPos board12 White (testStateUnavail White) (12, 82) False)
              White `shouldBe` False
          connectedRooks (mkTestPos board12 White (testStateUnavail White) (12, 82) False)
              Black `shouldBe` True

    describe "rookFileStatus" $
      it "Determines whether a rook's file is Open, HalfOpen, or NotOpen" $ do
          rookFileStatus (mkTestPos board13 White (testStateUnavail White) (13, 87) False)
              14 White `shouldBe` Open
          rookFileStatus (mkTestPos board13 White (testStateUnavail White) (13, 87) False)
              18 White `shouldBe` NotOpen
          rookFileStatus (mkTestPos board13 White (testStateUnavail White) (13, 87) False)
              82 Black `shouldBe` HalfOpen
          rookFileStatus (mkTestPos board13 White (testStateUnavail White) (13, 87) False)
              86 Black `shouldBe` Open

    describe "isKingInCheckFull" $
      it "Determines if the King at a given loc is in check from any enemy pieces" $ do
          isKingInCheckFull board03a White 15 `shouldBe` False
          isKingInCheckFull board04 Black 85 `shouldBe` False
          isKingInCheckFull board04 White 45 `shouldBe` True
          isKingInCheckFull board07 Black 88 `shouldBe` True

    describe "applyDiscoveredCheckTest" $
       it "is a helper helper used in testing discoverCheckDirs" $ do
            applyDiscoveredCheckTest down 10 0 `shouldBe` True

    describe "discoveredCheckDirs" $
      it "finds possible discovered check directions for a given move" $ do
        let (t1, _) = discoveredCheckTree2w
        let g1 = unGrid $ _cpGrid $ _chessPos $ rootLabel t1
        let bKingLoc1 = 84

        -- discovered check B6-C4
        let m1 = StdMove { _exchange = Nothing, _startIdx = 62, _endIdx = 43, _stdNote = "" }
        let res = discoveredCheckDirs g1 bKingLoc1 m1
        length res `shouldBe` 1

        putStrLn $ "dir applied to 11:" ++ show (head res 11) -- 22
        applyDiscoveredCheckTest (head res) 11 0 `shouldBe` True

        -- discovered check EP capture D5xE6
        let m2 = EnPassantMove { _epStartIdx =54, _epEndIdx = 65 , _epRemoveIdx = 55, _epNote = "" }
        let pairs2 = discoveredCheckDirs g1 bKingLoc1 m2
        length pairs2 `shouldBe` 1
        applyDiscoveredCheckTest (head pairs2) 10 0 `shouldBe` True

        let wKingLoc1 = 15
        -- king exposed by G1-H3
        let m3 = StdMove { _exchange = Nothing, _startIdx = 17, _endIdx = 38, _stdNote = "" }
        let pairs3 = discoveredCheckDirs g1 wKingLoc1 m3
        length pairs3 `shouldBe` 1
        applyDiscoveredCheckTest (head pairs3) 0 1 `shouldBe` True

        let (t2, _) = discoveredCheckTree2b
        let g2 = unGrid $ _cpGrid $ _chessPos $ rootLabel t2
        let bKingLoc2 = 84
        let wKingLoc2 = 12

        -- discovered check AND king exposed via EP capture D4xE3
        let m4 = EnPassantMove { _epStartIdx =44, _epEndIdx = 35 , _epRemoveIdx = 45, _epNote = "" }
        let pairs4 = discoveredCheckDirs g2 bKingLoc2 m4
        length pairs4 `shouldBe` 1
        applyDiscoveredCheckTest (head pairs4) 10 0 `shouldBe` True

        -- same move, same board but vs other king
        let pairs4b = discoveredCheckDirs g2 wKingLoc2 m4
        length pairs4b `shouldBe` 1
        applyDiscoveredCheckTest (head pairs4b) 0 11 `shouldBe` True

    describe "dirAttackMeLoc" $ do
      it "finds the first enemy piece in a given direction that can attack me back from that direction" $ do
        let (t, _) = attackMeTestTree
        let g = _cpGrid $ _chessPos $ rootLabel t
        dirAttackMeLoc g (44, ' ', Black) UpDir `shouldBe` Nothing
        dirAttackMeLoc g (44, ' ', Black) DownDir `shouldBe` Nothing
        dirAttackMeLoc g (44, ' ', Black) LeftDir `shouldBe` Just 41
        dirAttackMeLoc g (44, ' ', Black) RightDir `shouldBe` Nothing
        dirAttackMeLoc g (44, ' ', Black) DiagULDir `shouldBe` Just 71
        dirAttackMeLoc g (44, ' ', Black) DiagURDir `shouldBe` Nothing
        dirAttackMeLoc g (44, ' ', Black) DiagDLDir `shouldBe` Nothing
        dirAttackMeLoc g (44, ' ', Black) DiagDRDir `shouldBe` Nothing

    describe "moveIsDiscoveredCheck" $ do
      it "determines if a move results in a discovered check" $ do
        let (t, _) = discoveredCheckTree
        let g = _cpGrid $ _chessPos $ rootLabel t
        let mv1 = StdMove {_exchange = Just 'b', _startIdx = 55, _endIdx = 53, _stdNote = ""}
        let movingColor1 = White
        let kingLoc1 = 64
        moveIsDiscoveredCheck g mv1 movingColor1 kingLoc1 `shouldBe` True

        let mv1b = StdMove {_exchange = Nothing, _startIdx = 37, _endIdx = 28, _stdNote = ""}
        moveIsDiscoveredCheck g mv1b movingColor1 kingLoc1 `shouldBe` False

        let mv2 = StdMove {_exchange = Just 'b', _startIdx = 64, _endIdx = 53, _stdNote = ""}
        let movingColor2 = Black
        let kingLoc2 = 14
        moveIsDiscoveredCheck g mv2 movingColor2 kingLoc2 `shouldBe` True

    describe "moveIsDirectCheck" $
      it "determines if a move results in a direct check" $ do
        let (t1, _) = directCheckTree
        let g1 = _cpGrid $ _chessPos $ rootLabel t1
        let mv1 = CastlingMove { _castle = KingSide, _kingStartIdx = 15, _kingEndIdx = 17
                 , _rookStartIdx = 18, _rookEndIdx = 16, _castleNote = "" }
        let movingColor1 = White
        moveIsDirectCheck g1 mv1 movingColor1 `shouldBe` True

        let mv1b = StdMove {_exchange = Just 'n', _startIdx = 35, _endIdx = 53, _stdNote = ""}
        moveIsDirectCheck g1 mv1b movingColor1 `shouldBe` True

        let mv2 = StdMove {_exchange = Nothing, _startIdx = 53, _endIdx = 34, _stdNote = ""}
        let movingColor2 = Black
        moveIsDirectCheck g1 mv2 movingColor2 `shouldBe` True

        let mv2b = StdMove {_exchange = Nothing, _startIdx = 53, _endIdx = 45, _stdNote = ""}
        moveIsDirectCheck g1 mv2b movingColor2 `shouldBe` False

    describe "moveIsCheck" $
      it "determines if a move results in a check" $ do
        let (t1, _) = directCheckTree
        let pos1 = _chessPos $ rootLabel t1
        let mv1 = CastlingMove { _castle = KingSide, _kingStartIdx = 15, _kingEndIdx = 17
                 , _rookStartIdx = 18, _rookEndIdx = 16, _castleNote = "" }
        moveIsCheck pos1 mv1 `shouldBe` True

        let mv1b = StdMove {_exchange = Just 'n', _startIdx = 35, _endIdx = 53, _stdNote = ""}
        moveIsCheck pos1 mv1b `shouldBe` True

        let mv1c = CastlingMove { _castle = QueenSide, _kingStartIdx = 15, _kingEndIdx = 13
                 , _rookStartIdx = 11, _rookEndIdx = 14 , _castleNote = "" }
        moveIsCheck pos1 mv1c `shouldBe` False

        let n2 = discoveredCheckNode
        let pos2 = _chessPos n2
        let mv2 = StdMove {_exchange = Just 'b', _startIdx = 55, _endIdx = 53, _stdNote = ""}
        moveIsCheck pos2 mv2 `shouldBe` True

        let (t3, _) = discoveredCheckTree
        let pos3 = _chessPos $ rootLabel t3
        let mv3 = StdMove {_exchange = Just 'b', _startIdx = 55, _endIdx = 53, _stdNote = ""}
        moveIsCheck pos3 mv3 `shouldBe` True

        let mv3b = StdMove {_exchange = Nothing, _startIdx = 37, _endIdx = 28, _stdNote = ""}
        moveIsCheck pos3 mv3b `shouldBe` False

    describe "moveExposesKingDirect" $
      it "checks for a King moving directly into check" $ do
        let (t1, _) = exposedKingDirectTree
        let g1 = _cpGrid $ _chessPos $ rootLabel $ t1
        let m1 = StdMove {_exchange = Nothing, _startIdx = 63, _endIdx = 72, _stdNote = ""}
        let movingSideColor = White
        moveExposesKingDirect g1 m1 movingSideColor `shouldBe` True

        let m1b = StdMove {_exchange = Nothing, _startIdx = 63, _endIdx = 62, _stdNote = ""}
        moveExposesKingDirect g1 m1b movingSideColor `shouldBe` True

    describe "moveExposesKingDiscovered" $
      it "checks if the moving side's king is exposed by a discovered check" $ do
        let (t1, _) = exposedKingDiscoveredTree
        let g1 = _cpGrid $ _chessPos $ rootLabel t1
        let m1 = StdMove {_exchange = Nothing, _startIdx = 54, _endIdx = 55, _stdNote = ""}
        let movingSideColor = White
        let movingSideKingLoc = 45
        moveExposesKingDiscovered g1 m1 movingSideColor movingSideKingLoc `shouldBe` True

    describe "moveExposesKing" $
      it "Determines if a move results in the opposing King being in check" $ do
        let (t, _) = getStartNode "newgame"
        let pos = _chessPos $ rootLabel t
        let mv1 = StdMove { _exchange = Nothing, _startIdx = 25, _endIdx = 45, _stdNote = "" }
        moveExposesKing pos mv1 `shouldBe` False

        let (t2, _) = exposedKingDiscoveredTree
        let pos2 = _chessPos $ rootLabel t2
        let mv2 = StdMove {_exchange = Nothing, _startIdx = 54, _endIdx = 55, _stdNote = ""}
        moveExposesKing pos2 mv2 `shouldBe` True

    describe "findMove" $
      it ("find's a subtree element corresponding to a particular move from the current position"
         ++ " (this test: determine an opening move is correctly found in the starting position)") $ do
          let (t, _) = getStartNode "newgame"
          newTree <- runReaderT (expandToSingleThreaded t 2 2) testEnv
          let mv = StdMove { _exchange = Nothing, _startIdx = 25, _endIdx = 45, _stdNote = "" }
          case findMove newTree mv of
            Right t' -> (t /= t') `shouldBe` True
            Left s -> error s

    describe "checkFinal'" $
      it "determines if the board is in a 'final' position, i.e. checkmate or draw" $ do
          checkFinal' isFinalTestPos `shouldBe` BWins
          checkFinal' (posFromGrid board07 Black (68, 88) True) `shouldBe` WWins
          checkFinal' (posFromGrid board07b White (18,38) True) `shouldBe` BWins
          checkFinal' pos07c `shouldBe` Draw
          checkFinal' (posFromGrid board07d White (11, 23) True) `shouldBe` BWins
          checkFinal' pos14 `shouldBe` BWins

    describe "negaMax" $
      it "finds the best move from the tree of possible moves" $ do
        r2_2 <- matchStdMove mateInTwo02TestData
        r2_2 `shouldBe` True
        r2_3b <- matchStdMove mateInTwo03bTestData
        r2_3b `shouldBe` True
        r2_3 <- matchStdMove mateInTwo03TestData
        r2_3 `shouldBe` True
        r3_1 <- matchStdMove mateInThree01TestData
        r3_1 `shouldBe` True
        r3_2 <- matchStdMove mateInThree02TestData
        r3_2 `shouldBe` True
        p1 <- matchStdMove promotion01TestData
        p1 `shouldBe` True
        c1 <- matchStdMove critBug01TestData
        c1 `shouldBe` False
        c2 <- matchStdMove critBug01TestDataB
        c2 `shouldBe` False
        r2_1 <- matchStdMove mateInTwo01TestData
        r2_1 `shouldBe` True

    describe "checkPromote" $
      it "checks for pawn promotion" $ do
          checkPromote 'P' 82 `shouldBe` 'Q'
          checkPromote 'p' 12 `shouldBe` 'q'
    describe "Z.mateInCompare" $
      it "handles the comparison of two moves containing one or two 'mate in N moves'" $ do
        let no = Z.MateIn Nothing
        let whiteThree = Z.MateIn (Just (3, Z.Pos))
        let blackThree = Z.MateIn (Just (3, Z.Neg))
        let whiteTwo = Z.MateIn (Just (2, Z.Pos))
        let blackTwo = Z.MateIn (Just (2, Z.Neg))

        Z.mateInCompare whiteThree no `shouldBe` False
        Z.mateInCompare no whiteThree `shouldBe` True

        Z.mateInCompare no blackThree `shouldBe` False
        Z.mateInCompare blackThree no `shouldBe` True

        Z.mateInCompare whiteTwo whiteThree `shouldBe` False
        Z.mateInCompare blackTwo blackThree `shouldBe` True

        Z.mateInCompare whiteTwo blackTwo `shouldBe` False
        Z.mateInCompare blackThree whiteTwo `shouldBe` True
    describe "toFen" $
      it "describes the position in FEN (Forsyth-Edwards Notation)" $
        let pos = _chessPos endgameNode01
        in toFen pos `shouldBe` "r1k1r3/1pp2pp1/6q1/p7/5Q2/P5P1/1P3P1P/2KR3R w - - 0 20"
    describe "fromFen followed by toFen" $
      it "results in the original FEN string" $ do
        let fenStr = "3r4/8/3k4/2b1R3/8/6B1/8/3K4 w - - 0 0"
        let result =
              case fromFen fenStr of
                Left err -> err
                Right t -> (toFen . _chessPos . rootLabel . fst) t
        result `shouldBe` fenStr
    describe "parseChessEntry" $
      it "parses a move entered as text" $ do
        let n = castlingNode
        isRight (parseChessEntry n "F2 F3") `shouldBe` True
        isRight (parseChessEntry n "F2-F3") `shouldBe` True
        isRight (parseChessEntry n "f2-f3") `shouldBe` True
        isRight (parseChessEntry n "F2F3") `shouldBe` True
        isRight (parseChessEntry n "F2.F3") `shouldBe` True
        isRight (parseChessEntry n "F2,F3") `shouldBe` True
        isRight (parseChessEntry n "F2/F3") `shouldBe` True
        isRight (parseChessEntry n "F2|F3") `shouldBe` True
        isRight (parseChessEntry n "E1xE7") `shouldBe` True

        isRight (parseChessEntry n "E1xE") `shouldBe` False
        isRight (parseChessEntry n "2 F3") `shouldBe` False
        isRight (parseChessEntry n "") `shouldBe` False
        isRight (parseChessEntry n "\n") `shouldBe` False

    describe "findDirFromKing" $
      it "finds a rectangular or diagonal direction from the king to a a piece if it exists" $ do

        let d1 = findDirFromKing 45 75
        case d1 of
            Nothing -> putStrLn "d1 == Nothing"
            Just d -> putStrLn ("dir d1 applied to 10:" ++ show (d 10))
        applyDirTest d1 0 10 `shouldBe` True

        let d2 = findDirFromKing 45 25
        case d2 of
            Nothing -> putStrLn "d2 == Nothing"
            Just d -> putStrLn $ "dir d2 applied to 10:" ++ show (d 10)
        applyDirTest d2 10 0 `shouldBe` True

        let d3 = findDirFromKing 45 41
        applyDirTest d3 10 9 `shouldBe` True
        let d4 = findDirFromKing 45 47
        applyDirTest d4 10 11 `shouldBe` True
        let d5 = findDirFromKing 45 78
        applyDirTest d5 0 11 `shouldBe` True
        let d6 = findDirFromKing 45 54
        applyDirTest d6 1 10 `shouldBe` True
        let d7 = findDirFromKing 45 18
        applyDirTest d7 10 1 `shouldBe` True
        let d8 = findDirFromKing 45 23
        applyDirTest d8 11 0 `shouldBe` True
        isJust (findDirFromKing 45 64) `shouldBe` False
        isJust (findDirFromKing 45 45) `shouldBe` False


---------------------------------------------------------------------------------------------------
-- Test helper functions / datatypes
---------------------------------------------------------------------------------------------------
applyDirTest :: Maybe Dir -> Int -> Int -> Bool
applyDirTest dir param expected =
    isJust dir &&
    fromJust dir param == expected

applyDiscoveredCheckTest :: Dir -> Int -> Int -> Bool
applyDiscoveredCheckTest dir testParam expectedDirResult =
         dir testParam == expectedDirResult

matchStdMove :: StdMoveTestData -> IO Bool
matchStdMove StdMoveTestData{..} = do
    let (board, _) = getStartNode smtdBoardName
    let f :: (Z.HasZipTreeEnv r) => Z.ZipTreeM r (Z.NegaResult ChessNode)
        f = do
            tree <- Z.expandTo board 1 smtdDepth smtdCritDepth
            Z.negaMax tree (Nothing :: Maybe StdGen)
    result <- runReaderT f testEnv
    let theBest = Z.picked result
    let mvNode = head $ Z.nmMovePath theBest -- nmMovePath is never empty
    let mv = _chessMv mvNode
    case mv of
        StdMove {..} -> do
            let start = _startIdx
            let end = _endIdx
            putStrLn $ printf "ChestTest::matchStdMove - start:%d, end:%d" start end
            liftIO $ return $ start == smtdStartIdx && end == smtdEndIdx
        CastlingMove {} -> liftIO $ return False

posFromGrid :: ChessGrid -> Color -> (Int, Int) -> Bool -> ChessPos
posFromGrid g c (kingLocW, kingLocB) inCheck =
 mkTestPos g c  (testState c) (kingLocW, kingLocB) inCheck

posFromGridEnPassant :: ChessGrid -> Color -> Maybe Int -> (Int, Int) -> Bool -> ChessPos
posFromGridEnPassant g c ep (kingLocW, kingLocB) inCheck =
 mkTestPos g c (epTestState c ep) (kingLocW, kingLocB) inCheck

mkTestPos :: ChessGrid -> Color -> ChessPosState -> (Int, Int) -> Bool -> ChessPos
mkTestPos g c cpState (kingLocW, kingLocB) inCheck =
  let (wLocs, bLocs) = calcLocsForColor g
  in ChessPos
    { _cpGrid = g
    , _cpKingLoc = (kingLocW, kingLocB)
    , _cpInCheck = inCheck
    , _cpWhitePieceLocs = wLocs
    , _cpBlackPieceLocs = bLocs
    , _cpFin = NotFinal
    , _cpState = cpState {_cpsColorToMove = c}}

-- generic test state (Both K and Q side castiling avail for both sides)
testState :: Color -> ChessPosState
testState clr = ChessPosState
    { _cpsColorToMove = clr
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = 0
    , _cpsMoveNumber = 10
    , _cpsCastling = (BothAvailable, BothAvailable)
    , _cpsEnPassant = Nothing
    }

-- test state - King Side only castling both sides
testStateKSOnly :: Color -> ChessPosState
testStateKSOnly clr = (testState clr)
  { _cpsCastling = (KingSideOnlyAvailable, KingSideOnlyAvailable) }

-- test state - Queen Side only castling both sides
testStateQSOnly :: Color -> ChessPosState
testStateQSOnly clr = (testState clr)
  { _cpsCastling = (QueenSideOnlyAvailable, QueenSideOnlyAvailable) }

-- test state - castling unavailable both sides

testStateUnavail :: Color -> ChessPosState
testStateUnavail clr = (testState clr)
  { _cpsCastling = (Unavailable, Unavailable) }

-- test state - castled both sides
testStateCastled :: Color -> ChessPosState
testStateCastled clr = (testState clr)
  { _cpsCastling = (Castled, Castled) }

-- test state used for enpassant tests
epTestState :: Color -> Maybe Int -> ChessPosState
epTestState clr ep = ChessPosState
    { _cpsColorToMove = clr
    , _cpsLastMove = Nothing
    , _cpsHalfMovesForDraw = 0
    , _cpsMoveNumber = 10
    , _cpsCastling = (Castled, Castled)
    , _cpsEnPassant = ep
    }

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
board01 :: ChessGrid
board01 = ChessGrid $ V.fromList
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


enPassantBoard01 :: ChessGrid
enPassantBoard01 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'K',  'R',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  'B',  'R',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'P',  'B',  ' ',  'N',  'Q',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  'P',  'p',  '+',
                             '+',  ' ',  'q',  ' ',  ' ',  'b',  'p',  'p',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'b',  ' ',  '+',
                             '+',  ' ',  ' ',  'r',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   r   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   b   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   q   -   -   b   p   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   N   -   -   -   P   p          5| (50)  51   52   53   54   55   56   57   58  (59)
p   P   B   -   N   Q   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   -   -   B   R   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   R   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H

To test enpassant:
with White to move, sen enPassant state to 68 (for piece to move at 57)
with Black to move, set enPassant state to 32 (for piece to move at 41)
-}


---------------------------------------------------------------------------------------------------
invertedBoard01 :: ChessGrid
invertedBoard01 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  'r',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'b',  'p',  '+',
                             '+',  'p',  'q',  ' ',  ' ',  'b',  'p',  'p',  ' ',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  'P',  ' ',  '+',
                             '+',  ' ',  ' ',  'B',  ' ',  'N',  'Q',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'P',  'P',  ' ',  ' ',  'B',  'R',  ' ',  ' ',  '+',
                             '+',  ' ',  'K',  'R',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   K   R   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)
P   P   -   -   B   R   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   P   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   B   -   N   Q   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   P   -          5| (50)  51   52   53   54   55   56   57   58  (59)
p   q   -   -   b   p   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   b   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   r   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
board02 :: ChessGrid
board02 = ChessGrid $ V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

----------------------------------------------------------------------------------------------------
board03a :: ChessGrid
board03a = ChessGrid $ V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'R',  ' ',  ' ',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'Q',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  'B',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  'q',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  ' ',  'k',  'b',  'r',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   -   k   b   r   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   q   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   p   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   Q   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
_   R   -   -   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

board03b :: ChessGrid
board03b = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  ' ',  ' ',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  'K',  'Q',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  'B',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  'q',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   q   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   p   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   K   Q   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   -   -   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

board03c :: ChessGrid
board03c = ChessGrid $ V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  ' ',  ' ',  ' ',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'Q',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  'B',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'n',  ' ',  'p',  'n',  'p',  ' ',  '+',
                             '+',  'p',  'p',  'p',  'b',  'q',  'p',  'b',  'p',  '+',
                             '+',  'r',  ' ',  ' ',  'q',  'k',  ' ',  ' ',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   q   k   -   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   b   q   p   b   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   n   -   p   n   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   B   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   Q   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   -   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
board04 :: ChessGrid
board04 = ChessGrid $ V.fromList       [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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

board05 :: ChessGrid
board05 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  ' ',  'P',  ' ',  ' ',  'P',  ' ',  'P',  '+',
                             '+',  ' ',  'P',  ' ',  ' ',  'P',  ' ',  'P',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  'p',  '+',
                             '+',  ' ',  'p',  ' ',  'p',  'p',  ' ',  'p',  ' ',  '+',
                             '+',  'r',  'n',  'b',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   -   p   p   -   p   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   p   -   -   -   -   p          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   -   -   p   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   P   -   -   P   -   P   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   P   -   -   P   -   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board06 :: ChessGrid
board06 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  ' ',  'P',  ' ',  'P',  'P',  ' ',  'P',  ' ',  '+',
                             '+',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  'P',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  ' ',  'p',  ' ',  ' ',  'p',  ' ',  '+',
                             '+',  'p',  ' ',  'p',  ' ',  ' ',  'p',  ' ',  'p',  '+',
                             '+',  'r',  'n',  'b',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   p   -   -   p   -   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   -   p   -   -   p   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   p   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
P   -   -   -   -   P   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   P   -   -   -   -   P          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   -   P   P   -   P   -          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

pQ4pQ4Board :: ChessGrid
pQ4pQ4Board = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  'P',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   p   -   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   p   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   P   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}



----------------------------------------------------------------------------------------------------
-- Boards to check checkmate, draw states
----------------------------------------------------------------------------------------------------
board07 :: ChessGrid
board07 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'K',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'R',  ' ',  ' ',  'k',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   R   -   -   k          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   K          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
(WWins)
-}

board07b :: ChessGrid
board07b = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'r',  ' ',  ' ',  'K',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'k',  '+',
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
-   -   -   -   -   -   -   k          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   r   -   -   K          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
(BWins)
-}

pos07c :: ChessPos
pos07c =
    let (t, _) = board07cTree
    in _chessPos $ rootLabel t

board07cTree :: (Tree ChessNode, ChessPosState)
board07cTree =
    case getNodeFromFen "8/8/8/8/8/8/4r3/5k1K w - - 0 1" of
      Left err -> error "board07cTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   r   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   k   -   K          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
(if White's turn, Draw)
-}

board07d:: ChessGrid
board07d = ChessGrid $ V.fromList
                              [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                                '+',  'K',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  'k',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  'b',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                                '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (70)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (60)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   b   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   p   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   k   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
K   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
(BWins)
-}

board08 :: ChessGrid
board08 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'R',  'B',  'Q',  'K',  'B',  'N',  'R',  '+',
                             '+',  'P',  'P',  'P',  ' ',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'P',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'n',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  'p',  '-',  'p',  'p',  'p',  'p',  'p',  'p',  '+',
                             '+',  'r',  '-',  'b',  'q',  'k',  'b',  '-',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   -   b   q   k   b   -   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   p   p   p   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
n   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   p   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   P   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   N   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   P   -   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   R   B   Q   K   B   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board09 :: ChessGrid
board09 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  'K',  ' ',  'N',  'R',  '+',
                             '+',  'P',  ' ',  'P',  ' ',  'B',  'P',  ' ',  'P',  '+',
                             '+',  ' ',  'P',  ' ',  'P',  ' ',  ' ',  'P',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  ' ',  ' ',  'p',  ' ',  'p',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  'p',  'b',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  'p',  ' ',  ' ',  'p',  ' ',  'p',  '+',
                             '+',  'r',  'n',  ' ',  'q',  'k',  'b',  'n',  'r',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   -   q   k   b   n   r          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   p   -   -   p   -   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   p   b   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   p   -   -   p   -   p   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   P   -   P   -   -   P   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   P   -   B   P   -   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   K   -   N   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board10R3 :: ChessGrid
board10R3 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  ' ',  'R',  'K',  ' ',  '+',
                             '+',  ' ',  'P',  'P',  'P',  ' ',  'P',  'P',  ' ',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  ' ',  'N',  ' ',  'P',  '+',
                             '+',  ' ',  ' ',  'B',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  'b',  ' ',  'p',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  'p',  'p',  ' ',  'p',  'p',  'p',  '+',
                             '+',  'r',  'n',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   p   p   -   p   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
p   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   b   -   p   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   B   -   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   N   -   P          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   P   P   -   P   P              2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   -   R   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board10R4 :: ChessGrid
board10R4 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  'R',  'N',  'B',  'Q',  ' ',  'R',  'K',  ' ',  '+',
                             '+',  ' ',  'P',  'P',  'P',  ' ',  'P',  'P',  'P',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'N',  ' ',  ' ',  '+',
                             '+',  'P',  ' ',  'B',  ' ',  'P',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  'b',  ' ',  'p',  ' ',  ' ',  'p',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'n',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  'p',  'p',  ' ',  'p',  'p',  ' ',  '+',
                             '+',  'r',  'n',  'b',  'q',  ' ',  'r',  'k',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   n   b   q   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   p   p   -   p   p   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   n   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   b   -   p   -   -   p          5| (50)  51   52   53   54   55   56   57   58  (59)
P   -   B   -   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   N   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   P   P   -   P   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   N   B   Q   -   R   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board11 :: ChessGrid
board11 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  'R',  ' ',  'K',  ' ',  '+',
                              '+',  ' ',  'P',  ' ',  'B',  ' ',  ' ',  'P',  'P',  '+',
                              '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  'p',  ' ',  ' ',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'p',  '+',
                              '+',  ' ',  'p',  ' ',  ' ',  ' ',  'b',  'p',  ' ',  '+',
                              '+',  'r',  'k',  ' ',  ' ',  'r',  ' ',  ' ',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

r   k   -   -   r   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   p   -   -   -   b   p   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   p          6| (50)  61   62   63   64   65   66   67   68  (69)
p   -   -   -   R   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   -   B   -   -   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   R   -   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

board12 :: ChessGrid
board12 = ChessGrid $ V.fromList
                           [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                             '+',  ' ',  'K',  ' ',  ' ',  'R',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  'R',  ' ',  ' ',  '+',
                             '+',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'p',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  'p',  ' ',  ' ',  'r',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  ' ',  'k',  ' ',  'r',  ' ',  ' ',  ' ',  ' ',  '+',
                             '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   k   -   r   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   -   -   r   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   p   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   P   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
P   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   R   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   -   -   R   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-}

board13 :: ChessGrid
board13 = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
                              '+',  ' ',  ' ',  'K',  'R',  ' ',  ' ',  ' ',  'R',  '+',
                              '+',  ' ',  'P',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  'B',  ' ',  'N',  ' ',  ' ',  ' ',  ' ',  'P',  '+',
                              '+',  'b',  ' ',  'n',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  '+',
                              '+',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  'p',  '+',
                              '+',  ' ',  'r',  ' ',  ' ',  ' ',  'r',  'k',  ' ',  '+',
                              '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+' ]

{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   r   -   -   -   r   k   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   b   -   n   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   B   -   N   -   -   -   P          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   P   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   K   R   -   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
Desired rookFileStatus:
  R @ 14, Open
  R @ 18, NotOpen
  r @ 82, HalfOpen
  r @ 86, Open
-}

pos14 :: ChessPos
pos14 =
    let (t, _) = board14Tree
    in _chessPos $ rootLabel t

board14Tree :: (Tree ChessNode, ChessPosState)
board14Tree =
    case getNodeFromFen "8/8/8/8/8/1p6/rP6/K1k5 w - - 0 1" of
      Left err -> error "board14Tree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   p   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
r   P   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
K   -   k   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
(White's turn, checkmate)
-}

discoveredCheckTree2w :: (Tree ChessNode, ChessPosState)
discoveredCheckTree2w =
    case getNodeFromFen "3k4/8/1N6/B2Pp3/8/8/8/3RK1Nr w - e6 0 1" of
      Left err -> error "discoveredCheckTree2w returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   -   k   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   N   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
B   -   -   P   p   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   R   K   -   N   r          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
White to move
Black has just played D7-D5, so enpassant is available
discovered check moves:
  B6-wherever
  D5-E6 (EP)
discovered 'self-check':
  G1-whatever
-}

directCheckTree :: (Tree ChessNode, ChessPosState)
directCheckTree =
    case getNodeFromFen "r1r2k2/pp4pp/8/2n5/8/4B3/PP4PP/R3K2R w QK - 39 20" of
      Left err -> error "directCheckTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
r   -   r   -   -   k   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
p   p   -   -   -   -   p   p          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   n   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   B   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   P   -   -   -   -   P   P          2| (20)  21   22   23   24   25   26   27   28  (29)
R   -   -   -   K   -   -   R          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-- Moves to verify:
-- (W)  E1-G1 (king-side castle) check
-- (W)  E3xC5 check
-- (b)  C5-D3 check
-}

exposedKingDirectTree :: (Tree ChessNode, ChessPosState)
exposedKingDirectTree =
    case getNodeFromFen "2k5/8/2K5/q7/2Q5/8/8/8 w - - 0 1" of
      Left err -> error "exposedKingDirectTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   k   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   K   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
q   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   Q   -   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-- Moves to verify:
-- C6-B7 illegal, king exposed
-- C6-B6 illegal, king exposed
-}

exposedKingDiscoveredTree :: (Tree ChessNode, ChessPosState)
exposedKingDiscoveredTree =
    case getNodeFromFen "b3k3/8/8/3R4/4K3/8/8/8 w - - 0 1" of
      Left err -> error "exposedKingDiscoveredTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
b   -   -   -   k   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   -   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   R   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   -   K   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
-- Moves to verify:
-- D5-E5 illegal, king exposed
--
-}

discoveredCheckTree2b :: (Tree ChessNode, ChessPosState)
discoveredCheckTree2b =
    case getNodeFromFen "3k4/8/6b1/8/3pP3/8/3R4/1K6 w - e3 0 1" of
      Left err -> error "discoveredCheckTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   -   k   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   -   b   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   -   p   P   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   B          3| (30)  31   32   33   34   35   36   37   38  (39)
-   -   -   R   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   K   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Black to move
White has just played E2-E4, so enpassant is available
discovered check moves:
  D4xE3 (EP)
discovered 'self-check':
  D4xE3 (EP)
-}

-- Note: this is not intended to be a legal position...
attackMeTestTree :: (Tree ChessNode, ChessPosState)
attackMeTestTree =
    case getNodeFromFen "8/B5B1/5pb1/8/R2kP2R/8/1R6/6K1 b - - 0 1" of
      Left err -> error "discoveredCheckTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)
-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
B   -   -   -   -   -   B   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   -   -   -   -   p   b   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   -   -   -   -   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
R   -   -   k   P   -   -   R          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   -   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
-   R   -   -   -   -   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   K   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                          -------------------------------------------------
                                                A    B    C    D    E    F    G    H
Black to move
White has just played E2-E4, so enpassant is available
discovered check moves:
  D4xE3 (EP)
discovered 'self-check':
  D4xE3 (EP)
-}
isFinalTestPos :: ChessPos
isFinalTestPos =
  let (t, _) = isFinalTestTree
  in _chessPos $ rootLabel t

isFinalTestTree :: (Tree ChessNode, ChessPosState)
isFinalTestTree =
    case getNodeFromFen "8/8/1k4n1/1Pq1b3/2Kp4/5r2/P4P2/8 w - - 0 1" of
      Left err -> error "isFinalTestTree returned left??" -- this shouldn't happen
      Right r -> r
{-                                        (90) (91) (92) (93) (94) (95) (96) (97) (98) (99)

-   -   -   -   -   -   -   -          8| (80)  81   82   83   84   85   86   87   88  (89)
-   -   -   -   -   -   -   -          7| (50)  71   72   73   74   75   76   77   78  (79)
-   k   -   -   -   -   n   -          6| (50)  61   62   63   64   65   66   67   68  (69)
-   P   q   -   b   -   -   -          5| (50)  51   52   53   54   55   56   57   58  (59)
-   -   K   p   -   -   -   -          4| (40)  41   42   43   44   45   46   47   48  (49)
-   -   -   -   -   r   -   -          3| (30)  31   32   33   34   35   36   37   38  (39)
P   -   -   -   -   P   -   -          2| (20)  21   22   23   24   25   26   27   28  (29)
-   -   -   -   -   -   -   -          1| (10)  11   12   13   14   15   16   17   18  (19)

                                           (-) (01) (02) (03) (04) (05) (06) (07) (08) (09)
                                           -------------------------------------------------
                                                 A    B    C    D    E    F    G    H
-}

----------------------------------------------------------------------------------------------------
_boardTemplate :: ChessGrid
_boardTemplate = ChessGrid $ V.fromList
                            [ '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',  '+',
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
