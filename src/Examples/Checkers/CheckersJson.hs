{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckersJson where
import qualified CkParser as P
import Data.Aeson
import GHC.Generics

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
-}
