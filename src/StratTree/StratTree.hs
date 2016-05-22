module StratTree.StratTree ( best, worstReply, checkBlunders, expandTree, processMove, addEquiv, isWorse, isLegal) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Maybe
import Safe
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Debug.Trace

----------------------------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
best :: TreeNode t m => Tree t -> Int -> Reader Env (Maybe (Result m))
best tree color = do 
    depth <- asks _depth
    return (best' tree depth color negate getValue)

--TODO: move to lens getters
checkBlunders :: TreeNode t m => Tree t -> Int -> [MoveScore m] -> Reader Env (Maybe [MoveScore m])
checkBlunders tree color [] = return Nothing
checkBlunders tree color [ms] = return $ Just [ms]
checkBlunders tree color equivMS = do
    depth <- asks _errorDepth
    threshold <- asks _errorEquivThreshold
    --turn the list of equiv. MoveScores into a new list of MoveScores representing each move along with the score of 
    --the worst opponent's reply to that move 
    let equivScore = _score $ head equivMS
    let possibles = possibleBlunders tree depth color equivMS -- :: [MoveScore m] 
    return $ worstMS possibles color >>= (\worst -> if isWorse (worst ^. score) equivScore threshold color 
                                                    then Just (addEquiv worst possibles) 
                                                    else Just equivMS)

--expandTree :: tree -> depth -> tree
expandTree :: PositionNode n m => Tree n -> Reader Env (Tree n)
expandTree tree = do 
    depth <- asks _depth
    return $ visitTree tree depth visitor     
    
--process a chosen move - prune the tree down so the selected move is the new head 
--if there are no child moves at all, create a tree with just the single position corresponding to the move  
--processMove :: tree -> move -> tree
processMove :: PositionNode n m => Tree n -> m -> Tree n
processMove tree move = case subForest tree of 
    [] -> Node (newNode (rootLabel tree) move) []
    xs -> pruneToChild tree move  

isLegal :: PositionNode n m => Tree n -> m -> Bool
isLegal tree move = move `elem` possibleMoves (rootLabel tree)
 
---------------------------------------------------------------------------------------------------
-- non-exported functions
---------------------------------------------------------------------------------------------------    
    
--"worse" here is better wrt color used, since color is from tree level above 
isWorse :: Int -> Int -> Int -> Int -> Bool
isWorse  scoreToCheck compareTo margin color 
    | abs (scoreToCheck - compareTo) < margin    = False
    | (color * scoreToCheck) > (color * compareTo) = True
    | otherwise                                    = False 
    
possibleBlunders :: TreeNode t m => Tree t -> Int -> Int -> [MoveScore m] -> [MoveScore m]  
possibleBlunders tree depth color equivMS = catMaybes $ fmap convert equivMS where 
    convert ms = let result = worstReply tree depth color (_move ms) -- :: Maybe Result
                 in result >>= (\r -> case _moveScores r of 
                                          [] -> Nothing
                                          (x:xs) ->  Just (mkMoveScore (_move ms) (_score x)))                                          

worstMS :: [MoveScore m] -> Int -> Maybe (MoveScore m)
worstMS [] color = Nothing
worstMS (x : xs) color = Just $ foldr f x xs
    where f ms worst = if _score ms * color > _score worst * color then ms else worst
    
addEquiv :: MoveScore m -> [MoveScore m] -> [MoveScore m]
addEquiv target = 
    foldr f [] where 
        f x xs = if abs (_score x - _score target) == 0 then x:xs else xs    --TODO: add threshold for eqiv. here  
    
worstReply :: TreeNode t m => Tree t -> Int -> Int -> m -> Maybe (Result m)
worstReply tree depth color move = worst (pruneToChild tree move) depth color

worst :: TreeNode t m => Tree t -> Int -> Int -> Maybe (Result m)
worst tree depth color = best' tree depth color id getErrorValue

best' :: TreeNode t m => Tree t -> Int -> Int -> (Int -> Int) -> (t -> Int) -> Maybe (Result m)
best' tree depth color colorFlip getMoveValue = 
    let (path, rChoices, bestScore) = down tree depth color colorFlip getMoveValue
        pathM = tailMay path -- without the tree's starting "move"
        headM = pathM >>= headMay  
        followingM = pathM >>= tailMay 
        randChoiceM = liftM2 (:) headM (Just rChoices)
        --TODO: implement randchoices with different scores once scoreTolerance is added -- for now they are the same
        f x = fmap (`mkMoveScore` x)
        scoresM = fmap (f bestScore) randChoiceM
    in liftM3 Result randChoiceM followingM scoresM  
 
--down :: tree -> depth -> color -> color flipping function -> getValue/getErrorValue funct -> ([best mv path], [equiv random choices], best score)
down :: TreeNode t m => Tree t -> Int -> Int -> (Int -> Int) -> (t -> Int) -> ([m], [m], Int)
down (Node n []) depth color colorFlip getMoveValue = ([getMove n], [], color * getMoveValue n)
down (Node n xs) 0 color colorFlip getMoveValue = ([getMove n], [], color * getMoveValue n)
down (Node n xs) depth color colorFlip getMoveValue = 
    let (bestMvs, randChoices,  bestVal) = foldl (across depth color colorFlip getMoveValue) ([], [], minBound) xs
        --in trace ("Best move added: " ++ show (getMoveValue n) ++ " (color is " ++ show color) 
        --    (getMove n : bestMvs, randChoices, bestVal)
    in (getMove n : bestMvs, randChoices, bestVal)

--across :: depth -> color -> color flipping function -> getValue/getErrorValue funct -> ([best move list], [equiv random choices], best score) 
across :: TreeNode t m => Int -> Int -> (Int -> Int) -> (t -> Int) -> ([m], [m], Int) -> Tree t -> ([m], [m], Int)
across depth color colorFlip getMoveValue (rMvs, randChoices, rVal) t = 
    let (mvs, _, v) = down t (depth - 1) (colorFlip color) colorFlip getMoveValue
        (newMvs, newVal) = (mvs, colorFlip v)
    in  case rVal `compare` newVal of 
        EQ -> (rMvs, head newMvs : randChoices, rVal)
        LT -> (newMvs, [], newVal)
        GT ->  (rMvs, randChoices, rVal)
{-- 
 in  trace ("Color is " ++ show (colorFlip color) ++ ", comparing " ++ show rVal ++
                " to " ++ show newVal ++ " results in... ") 
        (case rVal `compare` newVal of 
            EQ -> trace "EQ - adding to equivalent list" (rMvs, head newMvs : randChoices, rVal)
            LT -> trace ("LT - new candidate (" ++ show newVal ++ ")") (newMvs, [], newVal)
            GT ->  trace "GT - ignoring" (rMvs, randChoices, rVal))
--}            
            