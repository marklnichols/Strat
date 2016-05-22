module StratIO.StratIO (resolveRandom) where

import StratTree.TreeNode
import System.Random
import System.IO

resolveRandom :: Move m => Maybe [MoveScore m] -> IO (Maybe m)
resolveRandom Nothing = return Nothing
resolveRandom (Just []) = return Nothing
resolveRandom (Just xs) = do
    --let ints = fmap (\x -> _move x) xs
    let ms = fmap _move xs
    r <- getStdRandom $ randomR (1, length ms)
    let n = ms !! (r-1)
    --putStrLn ("selecting: " ++ show n ++ " from: " ++ show xs)
    return (Just n) 
 