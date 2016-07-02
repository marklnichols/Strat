import Data.Tree
import Data.Tree.Zipper
import Data.Maybe

miniTree = Node 0 [Node 1 [Node 11 [], Node 12 []], Node 2 [Node 21 [], Node 22 []], Node 3 [Node 31 [], Node 32[]]]
284newBranch = Node 4 [] 

main = do
    let r = descend miniTree 4 visitor
    print r
    
descend :: Tree Int -> Int -> (TreePos Full Int -> Int -> TreePos Full Int) -> Tree Int 
descend tree max visitFunct = toTree $ descend' (fromTree tree) 0 where
    descend' tPos depth = 
        case firstChild tPos of
            Nothing    -> tPos
            Just child -> fromJust $ parent $ loop child (depth + 1)
        where
            loop tPos dpth = let modified = descend' (visitFunct tPos dpth) dpth in
                                case (next modified) of 
                                    Nothing      -> modified
                                    Just sibling -> loop sibling dpth 
  
visitor :: TreePos Full Int -> Int -> TreePos Full Int
visitor tPos depth
    | depth == 2    = modifyTree addBranch tPos
    | otherwise     = tPos
    
addBranch :: Tree Int -> Tree Int
addBranch tree = Node (rootLabel tree) [newBranch]

-- let subTPos = fromJust $ firstChild $ fromTree miniTree
-- let newTree = toTree modifyTree addBranch subTPos0
    
    

{--    
-- simple descend and add
descend :: TreeNode t => Maybe (TreeNode Full t) -> Int -> Int -> Maybe (TreeNode Full t)
descend Nothing depth max = Nothing
descend Just tPos depth max = loop firstChild tPos depth+1 max

loop Nothing depth max = Nothing
loop Just tp depth max = loop next ? (descend tp) 

-- descend all branches adding new moves for at supplied depth
addAtDepth :: startTree -> new depth max -> modified Tree 
addAtDepth TreeNode t => TreePos Full t -> Int ->  TreePos Full t
addAtDepth tPos max = addAtDepth` tPos 0 max         
        
-- descend all branches adding new moves for at supplied depth
addAtDepth' :: startTree -> current scan depth -> new depth max -> modified Tree 
addAtDepth' TreeNode t => TreePos Full t -> Int -> Int ->  TreePos Full t
addAtDepth' tPos depth max  
    | checkGoBack tPos depth max    = tPos
    | checkDescend tPos depth max   = TBD DESCEND
    | START HERE 
    
    {--
    | checkDepth (getLabel tPos) depth == True  = next sibling or getParent? $  to tPos $ lens update $ modify tPos
    | otherwise loop firstChild tPos where
        
        loop :: TreeNode t => Maybe (StreePos Full t) -> xyz
        loop Nothing = 
        loop Just tp = descendToDepth tp ??
    --}

checkGoBack tPos depth max = (hasChildren == False) && (depth < max - 1)  

checkDescend tPos depth max = (hasChildren == True) && (depth < max - 1)
   
   

{-- pseudo-code: 
if < depthMax-1 and no children -- return
if < depthMax -1 and children -- descend 
    
if at depthMax-1 and children 
    if this is an exchanging node, 
            check that all children have been evaluated, evaluate any missing- back up with modified tree 
    else done already, back up

if at depthMax -1 and no children -- create and add children moves

after adding, do descnedX where only exchanging nodes are visited, and only moves that exchange are evaluated..
--}


-- assumptions about tree completion: consider when implementing halt evaluation:
-- * for d < depth-1, if no children exist then node is a final move
-- * for non-exchanging nodes, if at least one child exists, all moves have been created and evaluated.
-- * for exchanging nodes, if >= 1 child exists, all moves have been created, but possible only exchaning sub-nodes have been evaluted
   

        
--roseZipper functions to use:
        --(Maybe full) = hasChildren tPos
        --(Bool) = firstChild tPos
        --(Maybe full) next tPos
        
 --}    

 
blunderTree2 = Node TreeItem {_tiMove = IntMove 0, _tiValue = 0, _tiErrorValue = 0} [
    Node TreeItem {_tiMove = IntMove 1, _tiValue = 100, _tiErrorValue = 121} [], 
    Node TreeItem {_tiMove = IntMove 3, _tiValue = 100, _tiErrorValue = 121} [],
    Node TreeItem {_tiMove = IntMove 4, _tiValue = 100, _tiErrorValue = 121} []]
        
            --isJust (runReader (checkBlunders blunderTree2 1 
            --    [mkMoveScore (IntMove 1) 100, mkMoveScore (IntMove 4) 100,
            --     mkMoveScore (IntMove 3) 100]) testEnv4) `shouldBe` True
