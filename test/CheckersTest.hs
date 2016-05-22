{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import Test.Hspec
import Data.Tree
import Control.Lens
import Control.Lens.Setter
import StratTree.TreeNode

checkersTest =  
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel getStartNode) `shouldMatchList` fmap IntMove [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getPossibleMoves blackFirstStartNode `shouldMatchList` fmap IntMove [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves
            
            getPossibleMoves (nodeFromGridW board01) `shouldMatchList` fmap IntMove [510, 1721, 1722, 2832, 2833, 2823, 2824, 3934, 3935] 
            getPossibleMoves (nodeFromGridB board01) `shouldMatchList` fmap IntMove [3732, 3733, 3025, 3026, 2024, 2025, 2015, 2016, 711, 712] 
            getPossibleMoves (nodeFromGridW board02) `shouldMatchList` fmap IntMove [510, 711, 813, 2529, 2823, 2824, 3329, 3338, 3934, 3935,
                                                                        10717, 11624, 11626, 12515, 12517, 12535]
            getPossibleMoves (nodeFromGridB board02) `shouldMatchList` fmap IntMove [2015, 2024, 2117, 3026, 12111, 13729]

---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------            
blackFirstStartNode :: CkNode
blackFirstStartNode = rootLabel getStartNode & ckPosition.clr .~ (-1)

treeFromGridW :: [Int] -> Tree CkNode
treeFromGridW g = Node CkNode {_ckMove = IntMove (-1), _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = 1, _fin = NotFinal}} []

treeFromGridB :: [Int] -> Tree CkNode
treeFromGridB g = Node CkNode {_ckMove = IntMove (-1), _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = -1, _fin = NotFinal}} []
 
nodeFromGridW :: [Int] -> CkNode
nodeFromGridW g = rootLabel $ treeFromGridW g
 
nodeFromGridB :: [Int] -> CkNode
nodeFromGridB g = rootLabel $ treeFromGridB g 


---------------------------------------------------------------------------------------------------
-- Test board positions
---------------------------------------------------------------------------------------------------
board01 :: [Int]                   
board01 = [99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -02, 00, 00,
           00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   00    --     19   20   21   22        
              00   00   00   01      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13        
              01   00   -2   00      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}

board02 :: [Int]                   
board02 = [99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
           00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22        
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13        
              01   00   01   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}

{-- 
board0n :: [Int]                    --  (41) (42) (43) (44) (45)    
board0n = [99, 99, 99, 99, 99, 01, 00, 02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 01, 99, 00, -2, 00, 00,
           00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
                                     --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35      (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26      (27)
                00   -2   00   00    --     19   20   21   22        
              00   00   00   01      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13        
              01   00   -2   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)       
-} 