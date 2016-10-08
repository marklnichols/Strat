module StratTree.StratTree ( best, worstReply, checkBlunders, expandTree, processMove, addEquiv, isWorse, isLegal, possibleBlunders, worst, best') where

import StratTree.Internal.Trees
import StratTree.TreeNode

import Data.Tree
import Data.Maybe
import Safe
import Control.Monad
import Control.Monad.Reader
import Control.Lens
--import Debug.Trace
import Data.Tree.Zipper

----------------------------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
best :: (Move m, Eval e, TreeNode t m e) => Tree t -> Int -> Reader Env (Maybe (Result m e))
best tree color = do
    depth <- asks _depth
    return (best' tree depth color flipColor getValue)

--TODO: move to lens getters
checkBlunders :: TreeNode t m e => Tree t -> Int -> [MoveScore m e] -> Reader Env (Maybe [MoveScore m e])
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

expandTree :: PositionNode n m e => Tree n -> Reader Env (Tree n)
expandTree tree = do
    depth <- asks _depth
    return $ visitTree tree depth visitor

--process a chosen move - prune the tree down so the selected move is the new head
--if there are no child moves at all, create a tree with just the single position corresponding to the move
processMove :: PositionNode n m e => Tree n -> m -> Tree n
processMove tree move = case subForest tree of
    [] -> Node (newNode (rootLabel tree) move) []
    xs -> pruneToChild tree move

isLegal :: PositionNode n m e => Tree n -> m -> Bool
isLegal tree move = move `elem` possibleMoves (rootLabel tree)

---------------------------------------------------------------------------------------------------
-- non-exported functions
---------------------------------------------------------------------------------------------------
--"worse" here is better wrt color used, since color is from tree level above
isWorse :: Eval e => e -> e -> Int -> Int -> Bool
isWorse  scoreToCheck compareTo margin color
    | abs (getInt scoreToCheck - getInt compareTo) < margin      = False
    | (color * getInt scoreToCheck) > (color * getInt compareTo) = True
    | otherwise = False

possibleBlunders :: TreeNode t m e => Tree t -> Int -> Int -> [MoveScore m e] -> [MoveScore m e]
possibleBlunders tree depth color equivMS = catMaybes $ fmap convert equivMS where
    convert ms = let result = worstReply tree depth color (_move ms) -- :: Maybe Result
                 in result >>= (\r -> case _moveScores r of
                                          [] -> Nothing
                                          (x:xs) ->  Just (mkMoveScore (_move ms) (_score x)))

worstMS :: Eval e => [MoveScore m e] -> Int -> Maybe (MoveScore m e)
worstMS [] color = Nothing
worstMS (x : xs) color = Just $ foldr f x xs
    where f ms worst = if getInt (_score ms) * color > getInt (_score worst) * color then ms else worst

addEquiv :: Eval e => MoveScore m e -> [MoveScore m e] -> [MoveScore m e]
addEquiv target =
    foldr f [] where
        f x xs = if abs (getInt (_score x) - getInt (_score target)) == 0 then x:xs else xs    --TODO: add threshold for eqiv. here

worstReply :: TreeNode t m e => Tree t -> Int -> Int -> m -> Maybe (Result m e)
worstReply tree depth color move = worst (pruneToChild tree move) depth color

worst :: TreeNode t m e => Tree t -> Int -> Int -> Maybe (Result m e)
worst tree depth color = best' tree depth color keepColor getErrorValue

best' :: (Eval e, TreeNode t m e) => Tree t -> Int -> Int -> (e -> e) -> (t -> e) -> Maybe (Result m e)
best' tree depth color colorFlip getValue =
    let (path, rChoices, flippedScore) = down tree depth color colorFlip getValue

        --black score has been flipped to positive for the comparisons, flip it back to negative:
        bestScore = setInt flippedScore (color * getInt flippedScore)

        pathM = tailMay path -- without the tree's starting "move"
        headM = pathM >>= headMay
        followingM = pathM >>= tailMay
        randChoiceM = liftM2 (:) headM (Just rChoices)
        --TODO: implement randchoices with different scores once scoreTolerance is added -- for now they are the same
        f x = fmap (`mkMoveScore` x)
        scoresM = fmap (f bestScore) randChoiceM
    in liftM3 Result randChoiceM followingM scoresM

--down :: tree -> depth -> color -> color flipping function -> getValue/getErrorValue funct
--        -> ([best mv path], [equiv random choices], best score)
down :: (Eval e, Move m, TreeNode t m e) => Tree t -> Int -> Int -> (e -> e) -> (t -> e) -> ([m], [m], e)
down (Node n []) depth color colorFlip getValue = ([getMove n], [], setInt (getValue n) (color * getInt (getValue n)))
down (Node n xs) 0 color colorFlip getValue = ([getMove n], [], setInt (getValue n) (color * getInt (getValue n)))
down (Node n xs) depth color colorFlip getValue =
    let (bestMvs, randChoices,  bestVal) =
            foldl (across depth color colorFlip getValue) ([], [], fromInt minBound) xs
        --in trace ("Best move added: " ++ show (getValue n) ++ " (color is " ++ show color)
        --    (getMove n : bestMvs, randChoices, bestVal)
    in (getMove n : bestMvs, randChoices, bestVal)

--across :: depth -> color -> color flipping function -> getValue/getErrorValue funct -> ([best move list], [equiv random choices], best score)
across :: (Eval e, Move m, TreeNode t m e) => Int -> Int -> (e -> e) -> (t -> e) -> ([m], [m], e) -> Tree t -> ([m], [m], e)
across depth color colorFlip getValue (rMvs, randChoices, rVal) t =
    let (mvs, _, v) = down t (depth - 1) (negate color) colorFlip getValue
        (newMvs, newVal) = (mvs, colorFlip v)
    in  case rVal `compare` newVal of
        EQ -> (rMvs, head newMvs : randChoices, rVal)
        LT -> (newMvs, [], newVal)
        GT ->  (rMvs, randChoices, rVal)

--updateTree 'visit' function - if not a final position and no children -- create and add children moves
visitor :: PositionNode n m e => TreePos Full n -> Int -> Int -> TreePos Full n
visitor tPos depth max
    | final (label tPos) /= NotFinal = tPos
    | depth == max                   = tPos
    | not (hasChildren tPos)         = modifyTree addBranches tPos
    | otherwise                      = tPos

--add branches at the bottom of the tree for a new depth-level of moves
--TODO: evaluate the new positions and set the values (set the move # too) and the "final" flag
addBranches :: PositionNode n m e => Tree n -> Tree n
addBranches tree =  let n = rootLabel tree
                        ns = map (newNode n) (possibleMoves n)
                        ts = map (\n -> Node n []) ns
                    in Node (rootLabel tree) ts
{--
 in  trace ("Color is " ++ show (colorFlip color) ++ ", comparing " ++ show rVal ++
                " to " ++ show newVal ++ " results in... ")
        (case rVal `compare` newVal of
            EQ -> trace "EQ - adding to equivalent list" (rMvs, head newMvs : randChoices, rVal)
            LT -> trace ("LT - new candidate (" ++ show newVal ++ ")") (newMvs, [], newVal)
            GT ->  trace "GT - ignoring" (rMvs, randChoices, rVal))
--}
