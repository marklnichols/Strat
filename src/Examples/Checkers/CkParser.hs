module CkParser (Move (..), Loc (..), run) where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Error
import Data.Char  
 
data Loc 
    = Loc Char Int
    deriving Show
   
data Move =  Move [Loc]
   deriving Show
 
loc :: Parser Loc
loc = do
    c <- oneOf "abcdefghABCDEFGH"
    --optional $ char '-'
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
    Left err    -> Left $ concatMap messageString (errorMessages err)
    Right xs    -> Right xs
 

