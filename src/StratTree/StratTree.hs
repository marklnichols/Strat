module StratTree.StratTree ( best, best') where

import StratTree.Internal.Trees
import StratTree.TreeNode
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
--best' :: tree -> depth -> color -> ([best path])
best' :: TreeNode t => Tree t -> Int -> Int -> [Int]
best' tree depth color = fst $ best tree depth color

--best :: tree -> depth -> color -> ([best mv path], best value)
best :: TreeNode t => Tree t -> Int -> Int -> ([Int], Int)
best tree depth color = 
    let (path, move) = findBest tree depth color
    in (tail path, move)
    
--expandTree :: tree -> depth -> moveFirstColor -> tree
expandTree :: TreeNode t => Tree t -> Int -> Int -> Tree t
expandTree tree depth fstMoveColor = tree   --nop for now

        -- scan down with zipper until no children or (depth = 0 && !exchanging)
        -- call possibleMoves
        
    
    
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
