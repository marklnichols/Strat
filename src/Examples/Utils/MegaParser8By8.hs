{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module MegaParser8By8
    ( Entry (..)
    , Loc (..)
    , run
    ) where

import Data.Aeson
import Data.Char
import Data.Void
import GHC.Generics

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Loc = Loc Char Int
    deriving (Eq, Generic)

instance Show Loc where
    show (Loc c i) = c : show i

instance ToJSON Loc where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Loc

data Entry
    = Move [Loc]
    | Cmd String
  deriving (Eq, Generic)

instance Show Entry where
  show (Move xs) = init $ concatMap (\x -> show x ++ "-") xs
  show (Cmd s) = "Command: " ++ s

loc :: Parser Loc
loc = do
    c <- oneOf "abcdefghABCDEFGH"
    d <- oneOf "12345678"
    return $ Loc c (digitToInt d)

spacers :: Parser ()
spacers = skipMany (oneOf "-.,/|x ")

move :: Parser Entry
move = do
    xs <- sepBy1 loc spacers
    return $ Move xs

cmd :: Parser Entry
cmd = do
    _ <- oneOf ":"
    b <- letterChar
    c <- many alphaNumChar
    return $ Cmd (b:c)

entry :: Parser Entry
entry = move <|> cmd

run :: String -> Either String Entry
run s = case runParser entry "" s of
    Left err    -> Left $ show err
    Right xs    -> Right xs
