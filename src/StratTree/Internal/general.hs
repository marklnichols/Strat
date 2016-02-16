module StratTree.Internal.General (visitor) where          
    
import StratTree.TreeNode 
import Data.Tree
import Data.Tree.Zipper

{--    
--updateTree 'visit' function - if at depthMax -1 and no children -- create and add children moves
visitor :: (PositionNode n) => TreePos Full n -> Int -> Int -> TreePos Full n
visitor tPos depth max
    | (final $ label tPos) == True                      = tPos
    | depth == (max-1) && (hasChildren tPos) == False   = modifyTree addBranches tPos
    | otherwise                                         = tPos
--} 
--updateTree 'visit' function - if not a final position and no children -- create and add children moves
visitor :: (PositionNode n) => TreePos Full n -> Int -> Int -> TreePos Full n
visitor tPos depth max
    | (final $ label tPos) == True  = tPos
    | depth == max                  = tPos
    | (hasChildren tPos) == False   = modifyTree addBranches tPos
    | otherwise                     = tPos
 
--add branches at the bottom of the tree for a new depth-level of moves
--TODO: evaluate the new positions and set the values (set the move # too) and the "final" flag
addBranches :: PositionNode n => Tree n -> Tree n
addBranches tree =  let n = rootLabel tree
                        ns = map (newNode n) (possibleMoves n)
                        ts = map (\n -> Node n []) ns
                    in Node (rootLabel tree) ts