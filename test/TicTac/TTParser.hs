module TicTac.TTParser (run) where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Error

move :: Parser Int
move = do
    d <- oneOf "123456789"
    return $ digitToInt d

run :: String -> Either String Int
run s = case runParser move () "" s of
    Left err -> Left $ concatMap messageString (errorMessages err)
    Right x  -> Right x
