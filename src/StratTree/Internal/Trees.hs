{-# LANGUAGE AllowAmbiguousTypes #-}
module StratTree.Internal.Trees (getChildren, getSiblings, descendPath, childByMove, 
    pruneChildrenExcept, visitTree, pruneToChild) where     
 
import StratTree.TreeNode 
import Data.Tree
import Data.Tree.Zipper 
import Data.List hiding (delete)
import Data.Maybe
 
--get a list of child nodes
getChildren :: TreePos Full a  -> [TreePos Full a]
getChildren tree = reverse $ loop (firstChild tree) [] where
    loop :: Maybe (TreePos Full a) -> [TreePos Full a] -> [TreePos Full a]
    loop Nothing xs = xs
    loop (Just tree) xs = loop (next tree) (tree : xs)

--get a list of sibling nodes
getSiblings :: TreePos Full a  -> [TreePos Full a]
getSiblings tree = case parent tree of
                       Nothing -> [tree]
                       Just t  -> getChildren t

--follows a path of moves to the tree corresponding to the last move in the list
--decendPath :: [moves] -> starting tree -> maybe (last child in list)
descendPath :: TreeNode t m => [m] -> TreePos Full t -> Maybe (TreePos Full t)
descendPath moves startTree = foldl f (Just startTree) moves 
    where 
        f :: TreeNode t => Maybe (TreePos Full t) -> Int -> Maybe (TreePos Full t)
        f r move = r >>= childByMove move 
   
--visit all the nodes and modify the tree via the visit function
--visitTree :: tree -> max depth -> visit function -> new Tree
visitTree :: PositionNode n m => Tree n -> Int -> (TreePos Full n -> Int -> Int -> TreePos Full n) -> Tree n
visitTree tree max visitFunct = toTree $ descend' (visitFunct (fromTree tree) 0 max) 0 where
    descend' tPos depth =
        case firstChild tPos of
            Nothing    -> tPos
            Just child -> fromJust $ parent $ loop child (depth + 1)
        where
            loop tPos dpth = let modified = descend' (visitFunct tPos dpth max) dpth in
                                case next modified of 
                                    Nothing      -> modified
                                    Just sibling -> loop sibling dpth     
   
--finds a child of a tree matching a given move
childByMove :: TreeNode t m => m -> TreePos Full t -> Maybe (TreePos Full t)
childByMove move tree  = 
    find (\x -> move == getMove (label x)) (getChildren tree) 

--pruneToChild -- prune the tree down to the subtree whose root matches the given child
--pruneToChild :: starting tree -> move to match -> new pruned sub tree                             
pruneToChild :: TreeNode t m => Tree t -> m -> Tree t
pruneToChild tree move = fromMaybe tree (find (\ x -> move == getMove (rootLabel x)) (subForest tree))                                                                                     

--delParent :: parentTree -> childTree -> childTree
delParent :: TreeNode t m => Tree t -> Tree t ->Tree t
delParent parent child = toTree $ modifyTree (const child) $ fromTree parent
    
--pruneExcept -- Prune the tree of all the children except the one matching the supplied move
--pruneExcept :: starting tree -> move to match -> pruned tree
pruneChildrenExcept :: TreeNode t m => Tree t -> m -> Tree t
pruneChildrenExcept tree move = 
    let mPair = delOneUntilLast (firstChild $ fromTree tree) move
    in case mPair of 
        (Nothing, b) -> tree
        (Just tPos, False) -> pruneChildrenExcept (toTree tPos) move
        (Just tPos, True) -> toTree tPos 

delOneUntilLast :: TreeNode t m => Maybe (TreePos Full t) -> m -> (Maybe (TreePos Full t), Bool) 
delOneUntilLast Nothing move = (Nothing, True)
delOneUntilLast (Just tp) move
   | isOnlyChild tp                 = (parent tp, True)
   | getMove (label tp) /= move   = (parent (delete tp), False)  --not the one to keep, return done yet flag  
   | otherwise                      = delOneUntilLast (next tp) move --more than 1 child & keeping this one -- try the next
              
isOnlyChild :: TreePos Full t -> Bool
isOnlyChild tp = isFirst tp && isLast tp 



