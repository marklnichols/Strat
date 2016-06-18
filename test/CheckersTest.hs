{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import Test.Hspec
import Data.Tree
import Control.Lens
import Control.Lens.Setter
import StratTree.TreeNode

checkersTest = do
    describe "getPossibleMoves" $
        it "Gets the list of possible moves for a given color from a given position." $ do
            getPossibleMoves (rootLabel getStartNode) `shouldMatchList` fmap mkSimpleCkMove [1419, 1519, 1520, 1620, 1621, 1721, 1722] --white moves
            getPossibleMoves blackFirstStartNode `shouldMatchList` fmap mkSimpleCkMove [2823, 2824, 2924, 2925, 3025, 3026, 3126] --black moves
            
            getPossibleMoves (nodeFromGridW board01) `shouldMatchList` fmap mkSimpleCkMove    [510, 1721, 1722, 2832, 2833, 2823, 2824, 3934, 3935] 
            getPossibleMoves (nodeFromGridB board01) `shouldMatchList` fmap mkSimpleCkMove    [3732, 3733, 3025, 3026, 2024, 2025, 2015, 2016, 711, 712] 
            getPossibleMoves (nodeFromGridW board02) `shouldMatchList` fmap mkSimpleCkMove    [510, 711, 813, 2529, 2823, 2824, 3329, 3338, 3934, 3935]
                                                                       ++ fmap mkSimpleCkJump [(0717, 12), (1624, 20), (1626, 21), (2515, 20), (2517, 21), (2535, 30)]
            getPossibleMoves (nodeFromGridB board02) `shouldMatchList` fmap mkSimpleCkMove    [2015, 2024, 2117, 3026]
                                                                       ++ fmap mkSimpleCkJump [(2111, 16), (3729, 33)]
    describe "calcNewNode" $
            calcNewNode (nodeFromGridW board01) (mkSimpleCkMove m1) ^. ckPosition ^. grid `shouldBe` board01_m1

            
---------------------------------------------------------------------------------------------------
-- Test helper functions
---------------------------------------------------------------------------------------------------            
blackFirstStartNode :: CkNode
blackFirstStartNode = rootLabel getStartNode & ckPosition.clr .~ (-1)

treeFromGridW :: [Int] -> Tree CkNode
treeFromGridW g = Node CkNode {_ckMove = mkSimpleCkMove (-1), _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = 1, _fin = NotFinal}} []

treeFromGridB :: [Int] -> Tree CkNode
treeFromGridB g = Node CkNode {_ckMove = mkSimpleCkMove (-1), _ckValue = 0, _ckErrorValue = 0, 
    _ckPosition = CkPosition {_grid = g, _clr = -1, _fin = NotFinal}} []
 
nodeFromGridW :: [Int] -> CkNode
nodeFromGridW g = rootLabel $ treeFromGridW g
 
nodeFromGridB :: [Int] -> CkNode
nodeFromGridB g = rootLabel $ treeFromGridB g 

mkSimpleCkMove :: Int -> CkMove
mkSimpleCkMove i = CkMove {_isJump = False, _startIdx = i `div` 100, _endIdx = i `mod` 100, _middleIdxs = [], _removedIdxs = []}

--TODO: must set the list of removed items...
mkSimpleCkJump :: (Int, Int) -> CkMove
mkSimpleCkJump (mv, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = [], _removedIdxs = [removed]}

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
m1 = 1722
board01_m1 :: [Int]  --board01 with move m1 applied                   
board01_m1 = [99, 99, 99, 99, 99, 01, 00, -02, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, -02, 00, 01,
           00, 00, 00, 00, 99, 02, 00, -1, 00, 00, 00, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
           
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   01    --     19   20   21   22        
              00   00   00   00      --   14   15   16   17    (18)
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