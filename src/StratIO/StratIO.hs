module StratIO.StratIO (resolveRandom) where

import System.Random

resolveRandom :: [Int] -> Maybe (IO Int)
resolveRandom [] = Nothing
resolveRandom xs = Just $ getStdRandom $ randomR (1, length xs)


 

