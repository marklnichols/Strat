module StratTree.Internal.Trees (getChildren, getSiblings, descendPath, childByMove, pruneChildrenExcept) where     
 
import StratTree.TreeNode 
import Data.Tree
import Data.Tree.Zipper 
import Data.List hiding (delete)
import Data.Maybe
 
-------------------------------------------------------------
getChildren :: TreePos Full a  -> [TreePos Full a]
getChildren tree = reverse $ loop (firstChild tree) [] where
    loop :: Maybe (TreePos Full a) -> [TreePos Full a] -> [TreePos Full a]
    loop Nothing xs = xs
    loop (Just tree) xs = loop (next tree) (tree : xs)

getSiblings :: TreePos Full a  -> [TreePos Full a]
getSiblings tree = case (parent tree) of
                       Nothing -> [tree]
                       Just t  -> getChildren t

--decendPath :: [moves] -> starting tree -> maybe (last child in list)
descendPath :: TreeNode t => [Int] -> TreePos Full t -> Maybe (TreePos Full t)
descendPath moves startTree = foldl f (Just startTree) moves 
    where 
        f :: TreeNode t => Maybe (TreePos Full t) -> Int -> Maybe (TreePos Full t)
        f r move = r >>= (childByMove move) 

childByMove :: TreeNode t => Int -> TreePos Full t -> Maybe (TreePos Full t)
childByMove move tree  = 
    find ((\x -> move == (getMove $ label x))) (getChildren tree) 
    
--pruneExcept -- Prune the tree of all the children except the one matching the supplied move
--pruneExcept :: starting tree -> move to match -> pruned tree
pruneChildrenExcept :: TreeNode t => Tree t -> Int -> Tree t
pruneChildrenExcept tree move = 
    let mPair = delOneUntilLast (firstChild $ fromTree tree) move
    in case mPair of 
        (Nothing, b) -> tree
        (Just tPos, False) -> pruneChildrenExcept (toTree tPos) move
        (Just tPos, True) -> toTree tPos 

delOneUntilLast :: TreeNode t => Maybe (TreePos Full t) -> Int -> (Maybe (TreePos Full t), Bool) 
delOneUntilLast Nothing move = (Nothing, True)
delOneUntilLast (Just tp) move = 
    if isOnlyChild tp   --only one child
        then (parent tp, True) 
        else if ((getMove $ label tp) /= move)
                    then  (parent (delete tp), False)  --not the one to keep, return done yet flag                         
                    else  delOneUntilLast (next tp) move      --more than one child & keeping this one -- try the next
       
isOnlyChild :: TreePos Full t -> Bool
isOnlyChild tp = (isFirst tp) && (isLast tp) 



