module StratTree.StratTree ( best, expandTree, processMove) where

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
best tree depth color = 
    let (path, rChoices, bestScore) = findBest tree depth color
        pathM = tailMay path -- without the tree's starting "move"
        headM = pathM >>= headMay  
        followingM = pathM >>= tailMay 
        randChoiceM = (liftM2 (:)) headM (Just rChoices)
       
        --TODO: implement randchoices with different scores once scoreTolerance is added -- for now they are the same
        f bestScore xs = fmap (\x -> MoveScore x bestScore) xs
        scoresM = liftM (f bestScore) randChoiceM
    in liftM3 Result randChoiceM followingM scoresM 
  
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
 
--findBest :: tree -> depth -> color -> ([best mv path], [equiv random choices], best score)
findBest :: TreeNode t => Tree t -> Int -> Int -> ([Int], [Int], Int)
findBest (Node n []) depth color = ([getMove n], [], color * getValue n)
findBest (Node n xs) 0 color = ([getMove n], [], color * getValue n)
findBest (Node n xs) depth color = 
    let (bestMvs, randChoices,  bestVal) = foldl (bestFold depth color) ([], [], minBound) xs
    --todo: for each randChoice need to build a list of moves/scores similar to what
    -- we are doing for moves now -- so that for each rand choice we have the path 
    -- containing both moves and scores -- we can get rid of a "main" move path
    --i.e. [[rndchoice1MvPath], [rndChoice2MvPath], etc.]]
    --findBest can now just return :: [[MoveScore]]
    --and we can probably get rid of the Result type
    in (getMove n : bestMvs, randChoices, bestVal)

        
--bestFold :: depth -> color -> ([best move list], [equiv random choices], best score) 
bestFold :: TreeNode t => Int -> Int -> ([Int], [Int], Int) -> Tree t -> ([Int], [Int], Int)
bestFold depth color (rMvs, randChoices, rVal) t = 
    --todo: treat randChoices the same way as mvs -- using rXYZ and newXYZ
    --in the LT case below, the previous randChoices need to be disgarded...
    let (mvs, _, v) = findBest t (depth - 1) (-color)
        (newMvs, newVal) = (mvs, -v)
    in  case rVal `compare` newVal of 
    --TODO: EQ -> rMvs, newMvs : randChoices_xxs - adding list to [[]]
        EQ -> (rMvs, head newMvs : randChoices, rVal)
        LT -> (newMvs, randChoices, newVal)
        GT ->  (rMvs, randChoices, rVal)
