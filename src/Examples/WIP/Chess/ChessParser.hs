{-# LANGUAGE DeriveGeneric #-}

module ChessParser 
    ( Move(..)    
    ) where
import GHC.Generics

--the following is just a temp placeholder-----
data Loc 
    = Loc Char Int
    deriving (Eq, Generic)
   
instance Show Loc where
    show (Loc c i) = c : show i    
   
data Move =  Move [Loc]
   deriving (Eq, Generic)
 
instance Show Move where
    show (Move xs) = init $ concatMap (\x -> show x ++ "-") xs
-----------------------------------------------