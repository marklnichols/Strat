{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}


module FenTest (fenTest) where

import Data.Text (Text)
import Test.Hspec
import FenParser

rows :: [Text]
rows = [ "r1k1r3"
       , "1pp2pp1"
       , "6q1"
       , "p7"
       , "5Q2"
       , "P5P1"
       , "1P3P1P"
       , "2KR3R"]

fenTest :: SpecWith ()
fenTest = do
  describe "parseFen" $ do
    it "returns the correct parsing result" $ do
      parseFen "r1k1r3/1pp2pp1/6q1/p7/5Q2/P5P1/1P3P1P/2KR3R w - - 5 30" `shouldBe` Right fenData01
      parseFen "r1k1r3/1pp2pp1/6q1/p7/5Q2/P5P1/1P3P1P/2KR3R b KQk e3 17 40" `shouldBe` Right fenData02

fenData01 :: FenData
fenData01 = FenData
    { rowData = rows
    , nextColor = 'w'
    , castling = (Nothing, Nothing)
    , enPassant = Nothing
    , halfMovesForDraw = 5
    , moveNumber = 30
    }

fenData02 :: FenData
fenData02 = FenData
    { rowData = rows
    , nextColor = 'b'
    , castling = (Just "KQ", Just "k")
    , enPassant = Just "e3"
    , halfMovesForDraw = 17
    , moveNumber = 40
    }
