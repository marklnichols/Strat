module StratTree.StratTree ( best, worstReply, expandTree, processMove) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Tuple.Select
import Safe
import Control.Monad

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
best :: TreeNode t => Tree t -> Int -> Int -> Maybe Result
best tree depth color = best' tree depth color negate

worstReply :: TreeNode t => Tree t -> Int -> Int -> Int -> Maybe Result
worstReply tree depth color move = worst (pruneToChild tree move) depth color

--process a chosen move - prune the tree down so the selected move is the new head 
--if there are no child moves at all, create a tree with just the single position corresponding to the move  
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
worst :: TreeNode t => Tree t -> Int -> Int -> Maybe Result
worst tree depth color = best' tree depth color id

best' :: TreeNode t => Tree t -> Int -> Int -> (Int -> Int) -> Maybe Result
best' tree depth color colorFlip = 
    let (path, rChoices, bestScore) = findBest tree depth color colorFlip
        pathM = tailMay path -- without the tree's starting "move"
        headM = pathM >>= headMay  
        followingM = pathM >>= tailMay 
        randChoiceM = (liftM2 (:)) headM (Just rChoices)
        --TODO: implement randchoices with different scores once scoreTolerance is added -- for now they are the same
        f bestScore xs = fmap (\x -> MoveScore x bestScore) xs
        scoresM = liftM (f bestScore) randChoiceM
    in liftM3 Result randChoiceM followingM scoresM  
 
--findBest :: tree -> depth -> color -> color flipping function -> ([best mv path], [equiv random choices], best score)
findBest :: TreeNode t => Tree t -> Int -> Int -> (Int -> Int) -> ([Int], [Int], Int)
findBest (Node n []) depth color colorFlip = ([getMove n], [], color * getValue n)
findBest (Node n xs) 0 color colorFlip = ([getMove n], [], color * getValue n)
findBest (Node n xs) depth color colorFlip = 
    let (bestMvs, randChoices,  bestVal) = foldl (bestFold depth color colorFlip) ([], [], minBound) xs
    in (getMove n : bestMvs, randChoices, bestVal)

        
--bestFold :: depth -> color -> color flipping function -> ([best move list], [equiv random choices], best score) 
bestFold :: TreeNode t => Int -> Int -> (Int -> Int) -> ([Int], [Int], Int) -> Tree t -> ([Int], [Int], Int)
bestFold depth color colorFlip (rMvs, randChoices, rVal) t = 
    let (mvs, _, v) = findBest t (depth - 1) (colorFlip color) colorFlip
        (newMvs, newVal) = (mvs, colorFlip v)
    in  case rVal `compare` newVal of 
        EQ -> (rMvs, head newMvs : randChoices, rVal)
        LT -> (newMvs, [], newVal)
        GT ->  (rMvs, randChoices, rVal)
