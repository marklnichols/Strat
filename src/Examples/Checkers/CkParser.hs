{-# LANGUAGE DeriveGeneric #-}

module CkParser
    ( Move (..)
    , Loc (..)
    , run
    ) where

import Data.Aeson
import Data.Char
import GHC.Generics
import Text.Parsec
import Text.Parsec.String

data Loc = Loc Char Int
    deriving (Eq, Generic)

instance Show Loc where
    show (Loc c i) = c : show i

instance ToJSON Loc where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Loc

newtype Move =  Move [Loc]
   deriving (Eq, Generic)

instance Show Move where
    show (Move xs) = init $ concatMap (\x -> show x ++ "-") xs

instance ToJSON Move where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Move

loc :: Parser Loc
loc = do
    c <- oneOf "abcdefghABCDEFGH"
    d <- oneOf "12345678"
    return $ Loc c (digitToInt d)

spacers :: Parser ()
spacers = skipMany (space <|> oneOf "-.,/|")

move :: Parser Move
move = do
    xs <- sepBy1 loc spacers
    return $ Move xs

run :: String -> Either String Move
run s = case runParser move () "" s of
    Left err    -> Left $ show err
    Right xs    -> Right xs
