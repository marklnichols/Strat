module StratTree.StratTree ( best, worstReply, checkBlunders, expandTree, processMove, addEquiv, isWorse) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Maybe
import Safe
import Control.Monad
import Control.Monad.Reader
import Control.Lens

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
best :: TreeNode t => Tree t -> Int -> Reader Env (Maybe Result)
best tree color = do 
    depth <- asks _depth
    return (best' tree depth color negate)

--TODO: move to lens getters
checkBlunders :: TreeNode t => Tree t -> Int -> [MoveScore] -> Reader Env (Maybe [MoveScore])
checkBlunders tree color [] = return Nothing
checkBlunders tree color equivMS = do
    depth <- asks _errorDepth
    threshold <- asks _errorEquivThreshold
    --turn the list of equiv. MoveScores into a new list of MoveScores representing each move along with the score of 
    --the worst opponent's reply to that move 
    let equivScore = _score $ head equivMS
        possibles = possibleBlunders tree depth color equivMS -- :: [MoveScore] 
        badMovesM = worstMS possibles color >>= (\worst -> if isWorse (worst ^. score) equivScore threshold color 
                                                      then Just (addEquiv worst possibles) 
                                                      else Just equivMS)
    return badMovesM


--expandTree :: tree -> depth -> tree
expandTree :: PositionNode n => Tree n -> Reader Env (Tree n)
expandTree tree = do 
    depth <- asks _depth
    return $ visitTree tree depth visitor     
    
--process a chosen move - prune the tree down so the selected move is the new head 
--if there are no child moves at all, create a tree with just the single position corresponding to the move  
--processMove :: tree -> move -> tree
processMove :: PositionNode n => Tree n -> Int -> Tree n
processMove tree move = case subForest tree of 
    [] -> Node (newNode (rootLabel tree) move) []
    xs -> pruneToChild tree move  
 
      
---------------------------------------------------------------------------------------------------
-- non-exported functions
---------------------------------------------------------------------------------------------------    
    
--"worse" here is better wrt color used, since color is from tree level above 
isWorse :: Int -> Int -> Int -> Int -> Bool
isWorse  scoreToCheck compareTo margin color 
    | abs (scoreToCheck - compareTo) < margin    = False
    | (color * scoreToCheck) > (color * compareTo) = True
    | otherwise                                    = False 
    
possibleBlunders :: TreeNode t => Tree t -> Int -> Int -> [MoveScore] -> [MoveScore]  
possibleBlunders tree depth color equivMS = catMaybes $ fmap convert equivMS where 
    convert ms = let result = worstReply tree depth color (_move ms) -- :: Maybe Result
                 in result >>= (\r -> case _moveScores r of 
                                          [] -> Nothing 
                                          (x:xs) ->  Just (MoveScore (_move ms) (_score x)))                                          

worstMS :: [MoveScore] -> Int -> Maybe MoveScore
worstMS [] color = Nothing
worstMS (x : xs) color = Just $ foldr f x xs
    where f ms worst = if _score ms * color > _score worst * color then ms else worst
    
addEquiv :: MoveScore -> [MoveScore] -> [MoveScore]
addEquiv target = 
    foldr f [] where 
        f x xs = if abs (_score x - _score target) == 0 then x:xs else xs    --TODO: add threshold for eqiv. here  
    
worstReply :: TreeNode t => Tree t -> Int -> Int -> Int -> Maybe Result
worstReply tree depth color move = worst (pruneToChild tree move) depth color

worst :: TreeNode t => Tree t -> Int -> Int -> Maybe Result
worst tree depth color = best' tree depth color id

best' :: TreeNode t => Tree t -> Int -> Int -> (Int -> Int) -> Maybe Result
best' tree depth color colorFlip = 
    let (path, rChoices, bestScore) = findBest tree depth color colorFlip
        pathM = tailMay path -- without the tree's starting "move"
        headM = pathM >>= headMay  
        followingM = pathM >>= tailMay 
        randChoiceM = liftM2 (:) headM (Just rChoices)
        --TODO: implement randchoices with different scores once scoreTolerance is added -- for now they are the same
        f bestScore = fmap (`MoveScore` bestScore)
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
