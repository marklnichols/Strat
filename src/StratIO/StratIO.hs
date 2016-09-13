module StratIO.StratIO (resolveRandom) where

import StratTree.TreeNode
import System.Random
--import System.IO

resolveRandom :: (Move m, Eval e) => [MoveScore m e] -> IO (Maybe m)
resolveRandom [] = return Nothing
resolveRandom xs = do
    let ms = fmap _move xs
    r <- getStdRandom $ randomR (1, length ms)
    let n = ms !! (r-1)
    --putStrLn ("selecting: " ++ show n ++ " from: " ++ show xs)
    return (Just n)
