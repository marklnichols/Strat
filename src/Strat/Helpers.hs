{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Strat.Helpers
    ( isLegal
    , makeChildren
    , findMove
    ) where

import Data.Foldable
import Data.Tree
import Strat.StratTree.TreeNode
-- import Debug.Trace
-- import Text.Printf

isLegal :: TreeNode t m => Tree t -> m -> [m] -> Bool
isLegal t mv exclusions =
  mv `notElem` exclusions &&
  mv `elem` possibleMoves (rootLabel t)

findMove :: forall t m. (TreeNode t m, Move m) => Tree t -> m -> Either String (Tree t)
findMove tree mv =
    let parentDepth = tlDepth $ treeLoc $ rootLabel tree
    in case find (\x ->
                     let n = rootLabel x
                     in tlDepth (treeLoc n) == parentDepth + 1
                        && mv == getMove (rootLabel x))
                 (subForest tree) of
        Just !t' -> Right t'
        Nothing ->
            let f x acc =
                    let dpthStr = show $ tlDepth $ treeLoc $ rootLabel x
                        moveStr = show $ getMove $ rootLabel x
                    in "|<" ++ moveStr ++ " (depth: " ++ dpthStr ++ ")>|, " ++ acc
                str = foldr f "" (subForest tree)
            in Left ("findMoved failed for move: " ++ show mv
                     ++ "(depth: " ++ show (parentDepth + 1) ++ ")"
                     ++ "(treesize: " ++ show (treeSize tree) ++ ")\n" ++ str) -- this should not happen

makeChildren :: TreeNode n m => n -> [Tree n]
makeChildren n =
    let tloc = treeLoc n
        parentDepth = tlDepth tloc
        tl = TreeLocation (parentDepth + 1)
        mvs = possibleMoves n
        ns = map (\m -> (newNode n m tl)) mvs
        ts = map (\x -> Node x []) ns
    in ts

--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
                in (sum levelTotals, levelTotals)
