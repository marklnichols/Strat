{-# LANGUAGE BangPatterns #-}

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

isLegal :: TreeNode t m => Tree t -> m -> Bool
isLegal t mv = mv `elem` possibleMoves (rootLabel t)

findMove :: TreeNode t m => Tree t -> m -> Tree t
findMove t mv =
    case find (\ x -> mv == getMove (rootLabel x)) (subForest t) of
        Nothing -> error ("findMoved failed for move: " ++ show mv
                         ++ "(treesize: " ++ show (treeSize t) ++ ")") -- this should not happen
        Just !t' -> t'

makeChildren :: TreeNode n m => n -> [Tree n]
makeChildren n =
    let ns = map (newNode n) (possibleMoves n)
        ts = map (\x -> Node x []) ns
    in ts

--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
                in (sum levelTotals, levelTotals)
