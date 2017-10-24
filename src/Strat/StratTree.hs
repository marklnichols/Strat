module Strat.StratTree 
    ( addEquiv
    , best
    , best'
    , checkBlunders
    , expandTree
    , isLegal
    , isWorse 
    , possibleBlunders
    , processMove
    , worst
    , worstReply
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper
import Safe
import Strat.StratTree.TreeNode
import Strat.StratTree.Trees

best :: (TreeNode t m e) => Tree t -> Int -> RST (Maybe (Result m e))
best t colr = do
    depth <- asks _depth
    return (best' t depth colr flipColor getValue)

--TODO: Convert to lens getters
checkBlunders :: TreeNode t m e => Tree t -> Int -> [MoveScore m e] -> RST (Maybe [MoveScore m e])
checkBlunders _ _ [] = return Nothing
checkBlunders _ _ [ms] = return $ Just [ms]
checkBlunders t colr equivMS = do
    depth <- asks _errorDepth
    threshold <- asks _errorEquivThreshold
    --turn the list of equiv. MoveScores into a new list of MoveScores representing each move along 
    -- with the score of the worst opponent's reply to that move
    let equivScore = _score $ head equivMS
    let possibles = possibleBlunders t depth colr equivMS -- :: [MoveScore m]
    return $ worstMS possibles colr >>= (\wrst -> if isWorse (wrst ^. score) equivScore threshold colr
                                                    then Just (addEquiv wrst possibles)
                                                    else Just equivMS)

expandTree :: PositionNode n m e => Tree n -> RST (Tree n)
expandTree t = do
    depth <- asks _depth
    return $ visitTree t depth visitor

--process a chosen move - prune the tree down so the selected move is the new head
--if there are no child moves at all, create a tree with just the single position corresponding to the move
processMove :: PositionNode n m e => Tree n -> m -> Tree n
processMove t mv = case subForest t of
    [] -> Node (newNode (rootLabel t) mv) []
    _ -> pruneToChild t mv

isLegal :: PositionNode n m e => Tree n -> m -> Bool
isLegal t mv = mv `elem` possibleMoves (rootLabel t)

--"worse" here is better wrt color used, since color is from tree level above
isWorse :: Eval e => e -> e -> Int -> Int -> Bool
isWorse  scoreToCheck compareTo margin colr
    | abs (getInt scoreToCheck - getInt compareTo) < margin    = False
    | (colr * getInt scoreToCheck) > (colr * getInt compareTo) = True
    | otherwise = False

possibleBlunders :: TreeNode t m e => Tree t -> Int -> Int -> [MoveScore m e] -> [MoveScore m e]
possibleBlunders t depth colr equivMS = catMaybes $ fmap convert equivMS where
    convert ms = 
        let result = worstReply t depth colr (_move ms) -- :: Maybe Result
        in result >>= (\r -> case _moveScores r of
            [] -> Nothing
            (x:_) ->  Just (mkMoveScore (_move ms) (_score x)))

worstMS :: Eval e => [MoveScore m e] -> Int -> Maybe (MoveScore m e)
worstMS [] _ = Nothing
worstMS (x : xs) colr = Just $ foldr f x xs
    where f ms wrst = if getInt (_score ms) * colr > getInt (_score wrst) * colr then ms else wrst

addEquiv :: Eval e => MoveScore m e -> [MoveScore m e] -> [MoveScore m e]
addEquiv target =
    foldr f [] where
        f x xs = if abs (getInt (_score x) - getInt (_score target)) == 0 then x:xs else xs    --TODO: add threshold for eqiv. here

worstReply :: TreeNode t m e => Tree t -> Int -> Int -> m -> Maybe (Result m e)
worstReply t depth colr mv = worst (pruneToChild t mv) depth colr

worst :: TreeNode t m e => Tree t -> Int -> Int -> Maybe (Result m e)
worst t depth colr = best' t depth colr keepColor getErrorValue

best' :: TreeNode t m e => Tree t -> Int -> Int -> (e -> e) -> (t -> e) -> Maybe (Result m e)
best' t depth colr colorFlip getVal =
    let (path, rChoices, flippedScore) = down t depth colr colorFlip getVal

        --black score has been flipped to positive for the comparisons, flip it back to negative:
        bestScore = setInt flippedScore (colr * getInt flippedScore)

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
down :: TreeNode t m e => Tree t -> Int -> Int -> (e -> e) -> (t -> e) -> ([m], [m], e)
down (Node n []) _ colr _ getVal = ([getMove n], [], setInt (getVal n) (colr * getInt (getVal n)))
down (Node n _) 0 colr _ getVal = ([getMove n], [], setInt (getVal n) (colr * getInt (getValue n)))
down (Node n xs) depth colr colorFlip getVal =
    let (bestMvs, randChoices,  bestVal) =
            foldl (across depth colr colorFlip getVal) ([], [], fromInt minBound) xs
        --in trace ("Best move added: " ++ show (getValue n) ++ " (color is " ++ show color)
        --    (getMove n : bestMvs, randChoices, bestVal)
    in (getMove n : bestMvs, randChoices, bestVal)

--across :: depth -> color -> color flipping function -> getValue/getErrorValue funct -> ([best move list], [equiv random choices], best score)
across :: (Eval e, TreeNode t m e) => Int -> Int -> (e -> e) -> (t -> e) -> ([m], [m], e) -> Tree t -> ([m], [m], e)
across depth colr colorFlip getVal (rMvs, randChoices, rVal) t =
    let (mvs, _, v) = down t (depth - 1) (negate colr) colorFlip getVal
        (newMvs, newVal) = (mvs, colorFlip v)
    in  case rVal `compare` newVal of
        EQ -> (rMvs, head newMvs : randChoices, rVal)
        LT -> (newMvs, [], newVal)
        GT ->  (rMvs, randChoices, rVal)

--updateTree 'visit' function - if not a final position and no children -- create and add children moves
visitor :: PositionNode n m e => TreePos Full n -> Int -> Int -> TreePos Full n
visitor tPos depth maxi
    | final (label tPos) /= NotFinal = tPos
    | depth == maxi                  = tPos
    | not (hasChildren tPos)         = modifyTree addBranches tPos
    | otherwise                      = tPos

--add branches at the bottom of the tree for a new depth-level of moves
--TODO: evaluate the new positions and set the values (set the move # too) and the "final" flag
addBranches :: PositionNode n m e => Tree n -> Tree n
addBranches t =  let n = rootLabel t
                     ns = map (newNode n) (possibleMoves n)
                     ts = map (\x -> Node x []) ns
                 in Node (rootLabel t) ts
{--
 in  trace ("Color is " ++ show (colorFlip color) ++ ", comparing " ++ show rVal ++
                " to " ++ show newVal ++ " results in... ")
        (case rVal `compare` newVal of
            EQ -> trace "EQ - adding to equivalent list" (rMvs, head newMvs : randChoices, rVal)
            LT -> trace ("LT - new candidate (" ++ show newVal ++ ")") (newMvs, [], newVal)
            GT ->  trace "GT - ignoring" (rMvs, randChoices, rVal))
--}
