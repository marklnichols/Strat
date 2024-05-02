{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

module FenParser
  ( parseFen
  , pFen
  , pRowData
  , FenData (..)
  )
  where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
-- import Text.Megaparsec.Debug

type Parser = Parsec Void Text

data FenData = FenData
    { rowData :: [Text] -- e.g., ["r1k1r3", "1pp2pp1", "6q1", "p7", ""5Q2", "P5P1", "1P3P1P", "2KR3R"]
    , nextColor :: Char -- e.g., 'w'
    , castling :: (Maybe Text, Maybe Text) -- e.g. (Just "KQ", Just "q")
    , enPassant :: Maybe Text -- e.g., Just "e4"
    , halfMovesForDraw :: Int -- e.g., 20
    , moveNumber :: Int -- e.g., 10
    }
  deriving (Show, Eq)

parseFen :: String -> Either String FenData
parseFen str =
  case parse (pFen <* eof) "" (T.pack str) of
    Right a -> Right a
    Left bundle -> Left (errorBundlePretty bundle)

pFen :: Parser FenData
pFen = do
  rows <- pRowData
  clr <- pColor
  castlng <- pCastling
  enPassnt <- pEnPassant
  halfMoveCount <- pHalfMoveCount
  moveNum <- pMoveNumber
  return FenData
    { rowData = rows
    , nextColor = clr
    , castling = castlng
    , enPassant = enPassnt
    , halfMovesForDraw = halfMoveCount
    , moveNumber = moveNum
    }

pRowData :: Parser [Text]
pRowData = do
    row8 <- pRow "row8" '/'
    row7 <- pRow "row7" '/'
    row6 <- pRow "row6" '/'
    row5 <- pRow "row5" '/'
    row4 <- pRow "row4" '/'
    row3 <- pRow "row3" '/'
    row2 <- pRow "row2" '/'
    row1 <- pRow "row1" ' '
    return [ row8, row7, row6, row5, row4, row3, row2, row1 ]

pRow :: String -> Char -> Parser Text
pRow rowName delim = do
  row <- takeWhileP (Just rowName) (/= delim)
  void (char delim)
  return row

pColor :: Parser Char
pColor = do
    c <- oneOf ['w', 'b']
    void (char ' ')
    return c

----------------------------------------------------------------------------------------------------
-- Parse the castling info
-- parses, for example:
-- "KQkq" --> (Just "KQ", Just "kq")
-- "Kq"   --> (Just "K", Just "q")
-- "kq"   --> (None, Just "kq")
-- "K"    --> (Just "K", None)
-- "-"    --> (None, None)
----------------------------------------------------------------------------------------------------
pCastling :: Parser (Maybe Text, Maybe Text)
pCastling = do
    x <- optional $ takeWhile1P (Just "white pieces castling")
           (\c -> (c == 'K') || (c == 'Q'))
    y <- optional $ takeWhile1P (Just "black pieces castling")
          (\c -> (c == 'k') || (c == 'q'))
    skipMany "-"   -- there could be a '-' to remove
    void (char ' ') -- plus a trailing space
    return (x, y)

----------------------------------------------------------------------------------------------------
-- Parse the enpassant info
-- parses, for example:
-- "a4" --> Just ("a4")
-- "-"  --> Nothing
----------------------------------------------------------------------------------------------------
pEnPassant :: Parser (Maybe Text)
pEnPassant = do
    dashMay <- optional (single '-' :: Parser Char)
    case dashMay of
      Just c -> do
        void (char ' ')
        return Nothing
      Nothing -> do
          result <- pAlgebraic
          void (char ' ')
          return $ Just result
    where
      pAlgebraic :: Parser Text
      pAlgebraic = do
        let letters = ['a'..'h'] ++ ['A'..'H'] -- letters :: [Char]
        c <- oneOf letters -- cMay :: Char
        d <- oneOf ['1'..'8']
        return $ T.pack [c, d]

pHalfMoveCount :: Parser Int
pHalfMoveCount = do
  result <- pNumber
  void (char ' ')
  return result

pMoveNumber :: Parser Int
pMoveNumber = pNumber

pNumber :: Parser Int
pNumber = do
    digitList <- many $ oneOf ['0'..'9']
    return $ read @Int $ digitList
