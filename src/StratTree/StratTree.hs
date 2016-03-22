module StratTree.StratTree ( best, worstReply, checkBlunders, expandTree, processMove, addEquiv, isWorse) where

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

--TODO: move to lens getters
--TODO: bad move threshold, bad move search depth, etc. come from a reader
old_checkBlunders :: TreeNode t => Tree t -> Int -> Int -> [MoveScore] -> [MoveScore]
old_checkBlunders tree depth color equivMS =
    --turn the list of equiv. MoveScores into a new list of MoveScores representing each move along with the score of 
    --the worst opponent's reply to that move 
    --TODO: incoming MoveScore should be a non-empty list, head ok here
    let equivScore = _score $ head equivMS
        convert ms = case (worstReply tree depth color (_move ms)) of -- :: MoveScore -> Maybe MoveScore
                        Nothing -> Nothing
                        --TODO: convert _moveScores to non-empty List, head ok here
                        (Just result) -> Just (MoveScore (_move ms) (_score (head (_moveScores result)))) 
        possibles = catMaybes $ fmap convert equivMS    -- :: [MoveScore] 
        worst = worstMS possibles color
    in if (equivScore - _score worst) >= 10 --todo: make this blunderThreshold from reader
           then  addEquiv worst possibles
           else  equivMS
           
checkBlunders :: TreeNode t => Tree t -> Int -> Int -> [MoveScore] -> [MoveScore]
checkBlunders tree depth color equivMS =
    --turn the list of equiv. MoveScores into a new list of MoveScores representing each move along with the score of 
    --the worst opponent's reply to that move 
    --TODO: incoming MoveScore should be a non-empty list, head ok here
    let equivScore = _score $ head equivMS
        possibles = possibleBlunders tree depth color equivMS -- :: [MoveScore] 
        worst = worstMS possibles color
    in if isWorse (_score worst) equivScore 10 color --todo: make this blunderThreshold from reader
           then  addEquiv worst possibles
           else  equivMS           
 
--"worse" here is better wrt color used, since color is from tree level above 
isWorse :: Int -> Int -> Int -> Int -> Bool
isWorse scoreToCheck compareTo margin color
    | (abs (scoreToCheck - compareTo)) < margin    = False
    | (color * scoreToCheck) > (color * compareTo) = True
    | otherwise                                    = False 
    
  
possibleBlunders :: TreeNode t => Tree t -> Int -> Int -> [MoveScore] -> [MoveScore]  
possibleBlunders tree depth color equivMS = 
    catMaybes $ fmap convert equivMS where 
        convert ms = case (worstReply tree depth color (_move ms)) of -- :: MoveScore -> Maybe MoveScore
                           Nothing -> Nothing
                           --TODO: convert _moveScores to non-empty List, head ok here
                           (Just result) -> Just (MoveScore (_move ms) (_score (head (_moveScores result))))

--TODO: param should be non-empty list    
worstMS :: [MoveScore] -> Int -> MoveScore
worstMS (x : []) color = x
worstMS (x : xs) color = foldr f x xs
    --where f ms worst = if _score ms < _score worst then ms else worst 
    where f ms worst = if (_score ms) * color > (_score worst) * color then ms else worst
    
addEquiv :: MoveScore -> [MoveScore] -> [MoveScore]
addEquiv target possibles = 
    foldr f [] possibles where 
        f x xs = if abs (_score x - _score target) == 0 then x:xs else xs    --TODO: add threshold for eqiv. here  
    
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

--TODO: make the components of Result non-empty lists...
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
