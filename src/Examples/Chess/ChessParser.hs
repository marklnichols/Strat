module ChessParser (Move (..), Loc (..), run) where

import Text.Parsec
import Text.Parsec.String
import Data.Char

data Loc
    = Loc Char Int
    deriving Eq

instance Show Loc where
    show (Loc c i) = c : show i

data Move =  Move [Loc]
   deriving Eq

instance Show Move where
    show (Move xs) = init $ concatMap (\x -> show x ++ "-") xs

loc :: Parser Loc
loc = undefined

move :: Parser Move
move = undefined

run :: String -> Either String Move
run s = case runParser move () "" s of
    Left err    -> Left $ show err
    Right xs    -> Right xs
