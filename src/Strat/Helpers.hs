module Strat.Helpers
    ( isLegal
    , makeChildren
    , findMove
    ) where

-- import Data.Maybe
import Data.Foldable
import Data.Tree
import Strat.StratTree.TreeNode
-- import Debug.Trace

isLegal :: TreeNode t m => Tree t -> m -> Bool
isLegal t mv = mv `elem` possibleMoves (rootLabel t)

findMove :: TreeNode t m => Tree t -> m -> Tree t
findMove t mv =
    case find (\ x -> mv == getMove (rootLabel x)) (subForest t) of
        Nothing -> error ("findMoved failed for move: " ++ show mv) -- this should not happen
        Just t' -> t'
  -- fromMaybe t (find (\ x -> mv == getMove (rootLabel x)) (subForest t))

makeChildren :: TreeNode n m => n -> [Tree n]
makeChildren n =
    let ns = map (newNode n) (possibleMoves n)
        ts = map (\x -> Node x []) ns
    in ts
