{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckersJson where
import Checkers
import qualified CkParser as P
import Data.Aeson
import GHC.Generics
import Data.Text

--------------------------------------------------
{-
data Loc = Loc {
    column :: Char,
    row :: Int
} deriving (Generic, Show)

instance ToJSON PieceLoc where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON PieceLoc
-}
instance ToJSON P.Loc where
    toEncoding = genericToEncoding defaultOptions
    
instance FromJSON P.Loc

--------------------------------------------------
{--
data Move = Move {
    isJump :: Bool, start :: Loc , end :: Loc, middle :: [Loc], removed :: [Loc]
} deriving (Generic, Show)

instance ToJSON Move where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Move
-}
instance ToJSON P.Move where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON P.Move

--------------------------------------------------
data LegalMoves = LegalMoves {moves :: [P.Move]} deriving (Generic, Show)

instance ToJSON LegalMoves where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LegalMoves

--------------------------------------------------
data Board = Board {
    pieces :: [P.Loc] } deriving (Generic, Show)

instance ToJSON Board where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Board

--------------------------------------------------
{-
 parseCkMove (nodeFromGridW board01) "A1 B2" `shouldBe` Right (mkSimpleCkMove 510)
            parseCkMove (nodeFromGridW board01) "A1-b2" `shouldBe` Right (mkSimpleCkMove 510)
            parseCkMove (nodeFromGridW board02) "A5 C7" `shouldBe` Right (mkSimpleCkJump (0717, 12))
            parseCkMove (nodeFromGridW board03) "E5 C7 A5" `shouldBe` Right (mkMultiCkJump m3)
    describe "toParserMove" $
        it "converts a CkMove to a Parser Move (for display)" $ do
            toParserMove (mkSimpleCkMove 510) `shouldBe` Just (Move [Loc 'A' 1,  Loc 'B' 2])
            toParserMove (mkSimpleCkJump (0717, 12)) `shouldBe` Just (Move [Loc 'A' 5, Loc 'C' 7])
            toParserMove (mkMultiCkJump m3) `shouldBe` Just (Move [Loc 'E' 5, Loc 'C' 7, Loc 'A' 5])
-}
{-

l1 = P.Loc 'A' 1
l2 = P.Loc 'B' 5

encode l1
encode l2

m1 = P.Move [P.Loc 'A' 1,  P.Loc 'B' 2]
m2 = P.Move [P.Loc 'E' 5, P.Loc 'C' 7, P.Loc 'A' 5]

encode m1
encode m2

b1 = Board {pieces = [l1, l2]}
encode b1



p1 = PieceLoc {column = 'A', row = 1, pieceType = 1, pieceColor = 1}
p2 = PieceLoc {column = 'B', row = 2, pieceType = 3, pieceColor = 1}
p3 = PieceLoc {column = 'C', row = 3, pieceType = 5, pieceColor = 1}
p4 = PieceLoc {column = 'D', row = 4, pieceType = 9, pieceColor = (-1)}

m1 = Move {pieceLocs = [p1, p2]}
m2 = Move {pieceLocs = [p3, p4]}

legal = LegalMoves { moves = [m1, m2]}

board = Board {pieces = [p1, p2, p3, p4]}

j1 = encode p1
j2 = encode p2
j3 = encode m1
j4 = encode legal
j5 = encode board
-}
