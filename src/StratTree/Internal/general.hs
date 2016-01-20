module StratTree.Internal.General (visitor) where          
    
import StratTree.TreeNode 
import Data.Tree
import Data.Tree.Zipper
    
--TODO where is this used?
--updateTree 'visit' function - if at depthMax -1 and no children -- create and add children moves
visitor :: (PositionNode n) => TreePos Full n -> Int -> Int -> TreePos Full n
visitor tPos depth max
    | depth == (max-1) && (hasChildren tPos) == False   = modifyTree addBranches tPos
    | otherwise                                         = tPos
 
--modifyTree :: (Tree a -> Tree a) -> TreePos Full a -> TreePos Full a
 
--add branches at the bottom of the tree for a new depth-level of moves
addBranches :: PositionNode n => Tree n -> Tree n
addBranches tree =  let n = rootLabel tree
                        ns = map (newNode n) (possibleMoves n)
                        ts = map (\n -> Node n []) ns
                    in Node (rootLabel tree) ts