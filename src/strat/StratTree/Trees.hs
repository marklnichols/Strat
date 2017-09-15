{-# LANGUAGE AllowAmbiguousTypes #-}
module StratTree.Trees 
    ( childByMove
    , descendPath
    , getChildren
    , getSiblings
    , pruneChildrenExcept
    , pruneToChild
    , treeSize
    , visitTree
    ) where

import Data.Tree
import Data.Tree.Zipper
import Data.List hiding (delete)
import Data.Maybe
import StratTree.TreeNode

--get a list of child nodes
getChildren :: TreePos Full a  -> [TreePos Full a]
getChildren t = reverse $ loop (firstChild t) [] where
    loop :: Maybe (TreePos Full a) -> [TreePos Full a] -> [TreePos Full a]
    loop Nothing xs = xs
    loop (Just t') xs = loop (next t') (t' : xs)

--get a list of sibling nodes
getSiblings :: TreePos Full a  -> [TreePos Full a]
getSiblings t = case parent t of
    Nothing -> [t]
    Just t' -> getChildren t'

--follows a path of moves to the tree corresponding to the last move in the list
descendPath :: TreeNode t m e => [m] -> TreePos Full t -> Maybe (TreePos Full t)
descendPath moves startTree = foldl f (Just startTree) moves where
        f :: TreeNode t m e => Maybe (TreePos Full t) -> m -> Maybe (TreePos Full t)
        f r mv = r >>= childByMove mv

--visit all the nodes and modify the tree via the visit function
visitTree :: Tree n -> Int -> (TreePos Full n -> Int -> Int -> TreePos Full n) -> Tree n
visitTree t maxi visitFunct = toTree $ descend' (visitFunct (fromTree t) 0 maxi) 0 where
    descend' tPos depth =
        case firstChild tPos of
            Nothing    -> tPos
            Just child -> fromJust $ parent $ loop child (depth + 1) where
                loop tp dpth = let modified = descend' (visitFunct tp dpth maxi) dpth 
                               in case next modified of
                                    Nothing      -> modified
                                    Just sibling -> loop sibling dpth

--finds a child of a tree matching a given move
childByMove :: TreeNode t m e => m -> TreePos Full t -> Maybe (TreePos Full t)
childByMove mv t  = find (\x -> mv == getMove (label x)) (getChildren t)

--pruneToChild -- prune the tree down to the subtree whose root matches the given child
pruneToChild :: TreeNode t m e => Tree t -> m -> Tree t
pruneToChild t mv = fromMaybe t (find (\ x -> mv == getMove (rootLabel x)) (subForest t))

--pruneExcept -- Prune the tree of all the children except the one matching the supplied move
pruneChildrenExcept :: TreeNode t m e => Tree t -> m -> Tree t
pruneChildrenExcept t mv =
    let mPair = delOneUntilLast (firstChild $ fromTree t) mv
    in case mPair of
        (Nothing, _) -> t
        (Just tPos, False) -> pruneChildrenExcept (toTree tPos) mv
        (Just tPos, True) -> toTree tPos

delOneUntilLast :: TreeNode t m e => Maybe (TreePos Full t) -> m -> (Maybe (TreePos Full t), Bool)
delOneUntilLast Nothing _ = (Nothing, True)
delOneUntilLast (Just tp) mv
   | isOnlyChild tp             = (parent tp, True)
   | getMove (label tp) /= mv   = (parent (delete tp), False)  --not the one to keep, return done yet flag
   | otherwise                  = delOneUntilLast (next tp) mv --more than 1 child & keeping this one -- try the next

isOnlyChild :: TreePos Full t -> Bool
isOnlyChild tp = isFirst tp && isLast tp

--gets the number of elements at each level of the tree plus the total size
--for debugging / analysis only
treeSize :: Tree t -> (Int, [Int])
treeSize t = let levelTotals = fmap length (levels t)
                in (sum levelTotals, levelTotals)
