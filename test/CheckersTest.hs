{-# LANGUAGE MultiParamTypeClasses #-}
module CheckersTest where

import Checkers
import CkParser
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
            getPossibleMoves (nodeFromGridW board02) `shouldMatchList` fmap mkSimpleCkJump [(0717, 12), (1624, 20), (1626, 21), (2515, 20), (2517, 21), (2535, 30)]
            getPossibleMoves (nodeFromGridB board02) `shouldMatchList` fmap mkSimpleCkJump [(2111, 16), (3729, 33)]
            getPossibleMoves (nodeFromGridW board06) `shouldMatchList` fmap mkSimpleCkJump [(2535, 30)]
    describe "calcNewNode" $
        it "creates a new node from a previous position and a move" $ do 
            calcNewNode (nodeFromGridW board01) (mkSimpleCkMove m1) ^. ckPosition ^. grid `shouldBe` board01_m1
            calcNewNode (nodeFromGridB board02) (mkSimpleCkJump m2) ^. ckPosition ^. grid `shouldBe` board02_m2
            calcNewNode (nodeFromGridW board03) (mkMultiCkJump m3) ^. ckPosition ^. grid `shouldBe` board03_m3
    describe "pieceCount" $
        it "Counts the number of white regular pieces minus the black regular pieces" $ 
            pieceCount board04 `shouldBe` -2
    describe "kingCount" $
        it "Counts the number of white king pieces minus the black king pieces" $ 
            kingCount board04 `shouldBe` 4
    describe "parseCkMove" $
        it "parses move input into a CkMove" $ do
            parseCkMove (nodeFromGridW board01) "A1 B2" `shouldBe` Right (mkSimpleCkMove 510)
            parseCkMove (nodeFromGridW board01) "A1-b2" `shouldBe` Right (mkSimpleCkMove 510)             
            parseCkMove (nodeFromGridW board02) "A5 C7" `shouldBe` Right (mkSimpleCkJump (0717, 12))
            parseCkMove (nodeFromGridW board03) "E5 C7 A5" `shouldBe` Right (mkMultiCkJump m3)
    describe "toParserMove" $
        it "converts a CkMove to a Parser Move (for display)" $ do
            toParserMove (mkSimpleCkMove 510) `shouldBe` Just (Move [Loc 'A' 1,  Loc 'B' 2])
            toParserMove (mkSimpleCkJump (0717, 12)) `shouldBe` Just (Move [Loc 'A' 5, Loc 'C' 7])
            toParserMove (mkMultiCkJump m3) `shouldBe` Just (Move [Loc 'E' 5, Loc 'C' 7, Loc 'A' 5])
    describe "checkPromote" $
        it "promotes a piece to king if it has reached the back row" $ do
            checkPromote (positionFromGridW board05) 01 38 `shouldBe` 2
            checkPromote (positionFromGridB board05) (-1) 07 `shouldBe` (-2)
            checkPromote (positionFromGridW board05) 01 25 `shouldBe` 1
      
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

positionFromGridW :: [Int] -> CkPosition
positionFromGridW g = nodeFromGridW g ^. ckPosition
 
positionFromGridB :: [Int] -> CkPosition
positionFromGridB g = nodeFromGridB g ^. ckPosition 

mkSimpleCkMove :: Int -> CkMove
mkSimpleCkMove i = CkMove {_isJump = False, _startIdx = i `div` 100, _endIdx = i `mod` 100, _middleIdxs = [], _removedIdxs = []}

mkSimpleCkJump :: (Int, Int) -> CkMove
mkSimpleCkJump (mv, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = [], _removedIdxs = [removed]}

mkMultiCkJump :: (Int, [Int], [Int]) -> CkMove
mkMultiCkJump (mv, middle, removed) = CkMove {_isJump = True, _startIdx = mv `div` 100, _endIdx = mv `mod` 100, _middleIdxs = middle, _removedIdxs = removed}

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

m2 = (3729, 33) 
board02_m2 :: [Int] --board02 with move m2 applied                
board02_m2 = [99, 99, 99, 99, 99, 01, 00, 01, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
           00, 00, 02, 00, 99, 02, -1, -1, 00, 01, 00, 00, 00, 99, 00, 00, 02, 00, 99, 99, 99, 99, 99]
           
{-                                   --  (41) (42) (43) (44) (45)    
                00   00   02   00    --     37   38   39   40        
              01  00   00   00      --   32   33   34   35    (36)
                02   -1   -1   00    --     28   29   30   31        
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22        
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13        
              01   00   01   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}


board03 :: [Int]                   
board03 = [99, 99, 99, 99, 99, 01, 00, 00, 01, 99, 00, 00, -1, 00, 00, 00, 01, 00, 99, 00, -02, -1, 00,
           00, 00, 02, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   02   00      --   23   24   25   26    (27)
                00   -2   -1   00    --     19   20   21   22        
              00   00   01   00      --   14   15   16   17    (18)
                00   00   -1   00    --     10   11   12   13        
              01   00   00   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}
m3 = (2507, [17], [21, 12])     --- 25 -> 17 -> 7 jumping 21 and 12
board03_m3 :: [Int]                   
board03_m3 = [99, 99, 99, 99, 99, 01, 00, 02, 01, 99, 00, 00, 00, 00, 00, 00, 01, 00, 99, 00, -02, 00, 00,
              00, 00, 00, 00, 99, 02, 00, -1, 00, 01, 02, 00, 00, 99, -1, 00, 02, 00, 99, 99, 99, 99, 99]
{-                                   --  (41) (42) (43) (44) (45)    
                -1   00   02   00    --     37   38   39   40        
              01  02   00   00      --   32   33   34   35    (36)
                02   00   -1   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26    (27)
                00   -2   00   00    --     19   20   21   22        
              00   00   01   00      --   14   15   16   17    (18)
                00   00   00   00    --     10   11   12   13        
              01   00   02   01      --   05   06   07   08    (09)
                                     --  (00) (01) (02) (03) (04)    
--}

board04 :: [Int]                    
board04 = [99, 99, 99, 99, 99, 00, 01, 00, 02, 99, 00, -1, 00, 00, -1, 00, 00, 00, 99, 02, 00, 00, 00,
           00, 00, 00, 00, 99, 00, -2, 00, 00, 00, 02, 00, -1, 99, 02, 00, 00, 02, 99, 99, 99, 99, 99]
board04_pc = -2
board04_kc = 1
       
board05 :: [Int]                    
board05 = [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, -1, 00, 00, 00, 00, 00, 00, 99, 00, 00, 01, 00,
           00, -1, 00, 00, 99, 00, 00, 00, 00, 00, 01, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
           
{--                                  --  (41) (42) (43) (44) (45)    
                00   00   00   00    --     37   38   39   40        
              00   01   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31        
              00   -1   00   00      --   23   24   25   26      (27)
                00   00   01   00    --     19   20   21   22        
              00   00   00   00      --   14   15   16   17      (18)
                00   -1   00   00    --     10   11   12   13        
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)
--}


board06 :: [Int]                    
board06 = [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, -1, 00, 00, 99, 00, 00, 00, 00,
           00, 00, 01, 00, 99, 00, 01, -1, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
{--            
                                     --  (41) (42) (43) (44) (45)    
                00   00   00   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35      (36)
                00   01   -1   00    --     28   29   30   31        
              00   00   01   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22        
              00   -1   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13        
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)       
-} 
       
{-- 
board0n :: [Int]                    
board0n = [99, 99, 99, 99, 99, 00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00,
           00, 00, 00, 00, 99, 00, 00, 00, 00, 00, 00, 00, 00, 99, 00, 00, 00, 00, 99, 99, 99, 99, 99]
           
                                     --  (41) (42) (43) (44) (45)    
                00   00   00   00    --     37   38   39   40        
              00   00   00   00      --   32   33   34   35      (36)
                00   00   00   00    --     28   29   30   31        
              00   00   00   00      --   23   24   25   26      (27)
                00   00   00   00    --     19   20   21   22        
              00   00   00   00      --   14   15   16   17      (18)
                00   00   00   00    --     10   11   12   13        
              00   00   00   00      --   05   06   07   08      (09)
                                     --  (00) (01) (02) (03) (04)       
-} 

{--
    --To run from a given position within ghci:
    let n = nodeFromGridW board06
    let t = Node n []
    loop t 1
    
    --or 
    loop (Node (nodeFromGridW board06) []) 1
--}