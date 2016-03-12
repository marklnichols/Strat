module StratIO.StratIO (resolveRandom) where

import System.Random
import System.IO

resolveRandom :: [Int] -> IO (Maybe Int)
resolveRandom [] = return Nothing
resolveRandom xs = do
    r <- getStdRandom $ randomR (1, length xs)
    let n = xs !! (r-1)
    putStrLn ("selecting: " ++ show n ++ " from: " ++ show xs)
    return (Just n) 


 

