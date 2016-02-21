module StratTree.StratTree ( best, best', expandTree, processMove) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------

--best' :: tree -> depth -> color -> ([best path])
best' :: TreeNode t => Tree t -> Int -> Int -> [Int]
best' tree depth color = fst $ best tree depth color

best :: TreeNode t => Tree t -> Int -> Int -> ([Int], Int)
best tree depth color = 
    let (path, move) = findBest tree depth color
    in (tail path, move)
  
--process a chosen move - prune the tree down so the selected move is the new head 
-- if there are no child moves at all, create a tree with just the single position corresponding to the move  
--processMove :: tree -> move -> tree
processMove :: PositionNode n => Tree n -> Int -> Tree n
processMove tree move = case subForest tree of 
    [] -> Node (newNode (rootLabel tree) move) []
    xs -> pruneToChild tree move  

--expandTree :: tree -> depth -> tree
expandTree :: PositionNode n => Tree n -> Int -> Tree n
expandTree tree maxDepth = visitTree tree maxDepth visitor  
    
--findBest :: tree -> depth -> color -> ([best mv path], findBest value)
findBest :: TreeNode t => Tree t -> Int -> Int -> ([Int], Int)
findBest (Node n []) depth color = ([getMove n], color * getValue n)
findBest (Node n xs) 0 color = ([getMove n], color * getValue n)
findBest (Node n xs) depth color = 
    let f = bestFold depth color
    in
        let (bestMvs, bestVal) = foldl f ([], minBound) xs
        in (getMove n : bestMvs, bestVal)

bestFold :: TreeNode t => Int -> Int -> ([Int], Int) -> Tree t -> ([Int], Int)
bestFold depth color (rMvs, rVal) t = 
    let (mvs, v) = findBest t (depth - 1) (-color)
        (newMvs, newVal) = (mvs, -v)
    in
        if rVal < newVal then (newMvs, newVal) else (rMvs, rVal)

        
        
        