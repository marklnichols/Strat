module StratTree.StratTree ( best, best', expandTree, processMove) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Tuple.Select

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
--best' :: tree -> depth -> color -> ([best path])
best' :: TreeNode t => Tree t -> Int -> Int -> [Int]
best' tree depth color = sel1 $ best tree depth color


--best :: tree -> depth -> color -> ([best path], [equiv random choices], best score))
best :: TreeNode t => Tree t -> Int -> Int -> ([Int], [Int], Int)
best tree depth color = 
    let (path, rChoices, bestScore) = findBest tree depth color
        path' = tail path   --without the tree's starting "move"
        randChoices = head path' : rChoices   --current best move is one of the random choices not in the list
        moves = resolveRandom path' randChoices
    in (moves, randChoices, bestScore) 

    
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
    
---------------------------------------------------------------------------------------------------
-- non-exported functions
---------------------------------------------------------------------------------------------------    
--resolve any random choices for the next move into a particular choice    
--resolveRandom :: [path of moves incl. a default choice] -> [random equivalent choices] -> [new path of moves]
resolveRandom :: [Int] -> [Int] -> [Int]    
resolveRandom moves randChoices = moves     --nop   
   
   
--findBest :: tree -> depth -> color -> ([best mv path], [equiv random choices], best score)
findBest :: TreeNode t => Tree t -> Int -> Int -> ([Int], [Int], Int)
findBest (Node n []) depth color = ([getMove n], [], color * getValue n)
findBest (Node n xs) 0 color = ([getMove n], [], color * getValue n)
findBest (Node n xs) depth color = 
    let f = bestFold depth color
    in
        let (bestMvs, randChoices,  bestVal) = foldl f ([], [], minBound) xs
        in (getMove n : bestMvs, randChoices, bestVal)

        
--bestFold :: depth -> color -> ([best move list], [equiv random choices], best score) 
bestFold :: TreeNode t => Int -> Int -> ([Int], [Int], Int) -> Tree t -> ([Int], [Int], Int)
bestFold depth color (rMvs, randChoices, rVal) t = 
    let (mvs, _, v) = findBest t (depth - 1) (-color)
        (newMvs, newVal) = (mvs, -v)
    in
        --if rVal < newVal then (newMvs, newVal) else (rMvs, rVal)
        case rVal `compare` newVal of 
            EQ -> (rMvs, head newMvs : randChoices, rVal)
            LT -> (newMvs, randChoices, newVal)
            GT ->  (rMvs, randChoices, rVal)
