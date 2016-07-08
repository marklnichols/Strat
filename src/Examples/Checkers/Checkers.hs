{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Checkers where
import qualified CkParser as Parser
import Prelude hiding (lookup)
import Control.Monad.ST
import Data.HashTable.ST.Basic
import Data.Tree
import StratTree.TreeNode hiding (Result, MoveScore)
import Data.List.Lens
import Control.Lens
import Data.List hiding (lookup, insert)
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
    
---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
data CkPosition = CkPosition {_grid :: [Int], _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''CkPosition

data CkMove = CkMove {_isJump :: Bool, _startIdx :: Int, _endIdx :: Int, _middleIdxs :: [Int], _removedIdxs :: [Int]} 
    deriving (Eq, Ord)
makeLenses ''CkMove

data CkNode = CkNode {_ckMove :: CkMove, _ckValue :: Int, _ckErrorValue :: Int, _ckPosition :: CkPosition}
makeLenses ''CkNode

--data JumpSeq = JumpSeq { _start :: Int, _middle :: [Int], _end :: Int}
--makeLenses ''JumpSeq

instance PositionNode CkNode CkMove where
    newNode = calcNewNode
    possibleMoves = getPossibleMoves
    color = view (ckPosition . clr)
    final = view (ckPosition . fin)
    showPosition = format
    parseMove = parseCkMove
    
-- todo: change this to a better input scheme    
-- return type to become Maybe CkMove
parseCkMove :: CkNode -> String -> Either String CkMove
parseCkMove n s 
    | Left err <- pMove   = Left err
    | Right x  <- pMove   = toCkMove n x
        where
        pMove = Parser.run s
        
{-- let x = (runParser Parser.move () "" s)
    in case x of     
        Left err -> x
        Right n -> Right $ buildMove (n) 
--}        

---------------------------------------------------------------------------------------------------
-- parse string input to move
---------------------------------------------------------------------------------------------------  
--strToMove :: String -> Int -> Either String IntMove    
--strToMove str color = do 
    --IntMove $ color * read str
    
 
instance TreeNode CkNode CkMove where
    getMove = _ckMove
    getValue = _ckValue
    getErrorValue = _ckErrorValue
  
-- TODO: show the removed pieces 
instance Show CkMove where
    show m = "{jmp:" ++ show (m ^. isJump) ++ " st:" ++ show (m ^. startIdx) ++
                " mid:[" ++ showList (m^.middleIdxs) ++ "]" ++
                " rem:[" ++ showList (m^.removedIdxs) ++ "]" ++
                " end:[" ++ show (m^.endIdx) ++ "]}"
                where showList [] = ""
                      showList xs = foldr (\y acc -> acc ++ show y ++ " ") "" xs
                      
                {--
                ++ case m^.middleIdxs of 
                    [] -> "" 
                    xs -> foldr (\y acc -> acc ++ show y ++ " ") "" xs
                --}
                
instance Show CkNode where
    show n = "move: " ++ show (n ^. ckMove) ++ " value: " ++ show (n ^. ckValue) ++ " errorValue: " 
             ++ show (n ^. ckErrorValue) ++ " position: " ++ show (n ^. ckPosition) 

instance Move CkMove    
    
---------------------------------------------------------------------------------------------------
-- starting position,
---------------------------------------------------------------------------------------------------
getStartNode :: Tree CkNode
getStartNode = Node 
    CkNode {_ckMove = CkMove {_isJump = False, _startIdx = -1, _endIdx = -1, _middleIdxs = [], _removedIdxs = []},
            _ckValue = 0, _ckErrorValue = 0, _ckPosition = 
            CkPosition {_grid = mkStartGrid 1, _clr = 1, _fin = NotFinal}} []
        

---------------------------------------------------------------------------------------------------
-- Grid layout - indexes 0-45
---------------------------------------------------------------------------------------------------
{-- how indexes relate to board position (indexes in parens are not displayed):
          
   (41) (42) (43) (44) (45)    
   
H|     37  38  39  40                          
G|   32  33  34  35  (36)        
F|     28  29  30  31                                
E|   23  24  25  26  (27)               
D|     19  20  21  22                              
C|   14  15  16  17  (18)              
B|     10  11  12  13                              
A|   05  06  07  08  (09)         
                                                     
   (00) (01) (02) (03) (04)    
     ---------------
     1 2 3 4 5 6 7 8
--}                             

offBoard :: [Int]
offBoard = [0, 1, 2, 3, 4, 9, 18, 27, 36, 41, 42, 43, 44, 45]

--------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Initial board position
---------------------------------------------------------------------------------------------------                                                                                    
mkStartGrid :: Int -> [Int] 
mkStartGrid bottomColor =  fmap (indexToValue bottomColor) [0..45]

indexToValue :: Int -> Int -> Int
indexToValue bottomColor idx 
    | idx < 5               = 99           -- off the edge
    | idx > 40              = 99           -- off the edge
    | idx `mod` 9 == 0      = 99           -- off the edge
    | idx > 18 && idx < 27  = 0            -- center, no initial pieces 
    | idx < 18              = bottomColor  -- player at bottom of board initial pieces
    | otherwise             = negate bottomColor -- player at top initial pieces
  
---------------------------------------------------------------------------------------------------
-- format position as a string
---------------------------------------------------------------------------------------------------
format :: CkNode -> String
format node =   let xs = node ^. ckPosition ^. grid
                in loop xs 5 "" where
                    loop :: [Int] -> Int -> String -> String
                    loop xs 41 result = result
                    loop xs n result =  let (newIdx, spaces) = case n `mod` 9 of
                                                                   0 -> (n + 1, " ") 
                                                                   5 -> (n, "")
                                        in loop xs (newIdx + 4) (result ++ rowToStr xs newIdx spaces)
                                
rowToStr :: [Int] -> Int -> String -> String
rowToStr xs i spaces = spaces ++ toXOs (xs !! i) ++ 
                                 toXOs (xs !! (i + 1)) ++ 
                                 toXOs (xs !! (i + 2)) ++ 
                                 toXOs (xs !! (i + 3)) ++ "\n"

toXOs :: Int -> String
toXOs 1 = "X "
toXOs (-1) = "O "
toXOs 0 = "- "
toXOs _ = "? "
   
---------------------------------------------------------------------------------------------------
-- Convert Parser's Move type to CkMove
---------------------------------------------------------------------------------------------------
toCkMove :: CkNode -> Parser.Move -> Either String CkMove
toCkMove node (Parser.Move xs) =     --parser guarentees at least one item in list. TODO: move to non-empty list
    let xs' = fmap locToInt xs 
    in case length xs' of 
        1 -> fromSingleLoc node (head xs')
        n -> Right $ fromMultLoc xs'

        
fromSingleLoc :: CkNode -> Int -> Either String CkMove
fromSingleLoc node x
    | nMoves + nJumps /= 1  = Left $ show x ++ " is not a valid unique move."
    | nMoves == 1           = Right $ head moves
    | otherwise             = Right $ head jumps
        where
        moves = pieceMoves node x
        jumps = pieceJumps node x
        nMoves = length moves
        nJumps = length jumps

        
fromMultLoc :: [Int] -> CkMove
fromMultLoc xs = 
    let st = head xs
        end = last xs
        mid = init $ tail xs
        rem = calcRemoved xs
        isJmp = not (null rem)
    in  CkMove { _isJump = isJmp, _startIdx = st, _endIdx = end, _middleIdxs = mid, 
                 _removedIdxs = rem }

                 
calcRemoved :: [Int] -> [Int]
calcRemoved xs = foldr f [] (zip xs (tail xs))  where
    f (prevX, x) r 
        | abs (x - prevX) `elem` [8, 10] = prevX + (x-prevX) `div` 2 : r
        | otherwise                      = r

        
locToInt :: Parser.Loc -> Int  -- parser ensures valid key
locToInt (Parser.Loc c d)  
    | d `mod` 2 == 0    = f (d-1)
    | otherwise         = f d
        where
        f x = Map.findWithDefault (-1) (toUpper c) rowIndexes + (x `div` 2)

        
rowIndexes = Map.fromList [('A', 5), ('B', 10), ('C', 14), ('D', 19), ('E', 23), ('F', 28), ('G', 23)]
 
---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: CkNode -> CkMove -> CkNode
calcNewNode node mv =
    let moved = movePiece (node ^. ckPosition) (mv ^. startIdx) (mv ^. endIdx) 
        captured = removeMultiple moved (mv ^. removedIdxs)      --remove any captured pieces
        clrFlipped = set clr (flipColor (captured ^. clr)) captured
        (score, finalSt) = evalGrid $ clrFlipped ^. grid
        errScore = errorEvalGrid $ clrFlipped ^. grid
        allSet = set fin finalSt clrFlipped
    in  CkNode mv score errScore allSet
 
removePiece :: CkPosition -> Int -> CkPosition
removePiece pos index = set (grid . ix index) 0 pos

removeMultiple :: CkPosition -> [Int] -> CkPosition
removeMultiple = foldr (flip removePiece)    
     
movePiece :: CkPosition -> Int -> Int -> CkPosition
movePiece pos from to = 
    let value = pos ^? (grid . ix from) 
        valid = value >>= validPiece pos
    in case valid of 
        Nothing -> pos
        Just x ->  let p = set (grid . ix to) x pos
                     in removePiece p from    
        
validPiece :: CkPosition -> Int -> Maybe Int
validPiece pos x = if x /= 0 && abs x < 3 then Just x else Nothing

  
--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
eval :: CkNode -> Int
eval n = fst $ evalGrid $ n ^. ckPosition ^. grid 

errorEval :: CkNode -> Int
errorEval n = errorEvalGrid $ n ^. ckPosition ^. grid
    
-- TODO: implement    
evalGrid :: [Int] ->  (Int, FinalState)
evalGrid grid = (kingCount grid * 3 + pieceCount grid, NotFinal)

-- TODO: implement
errorEvalGrid :: [Int] -> Int
errorEvalGrid grid = fst $ evalGrid grid

pieceCount :: [Int] -> Int
pieceCount grid = count grid 1

kingCount :: [Int] -> Int
kingCount grid = count grid 2

count :: [Int] -> Int -> Int
count grid p = 
    let w = length $ filter (== p) grid  
        b = length $ filter (== (-p)) grid
    in  w-b
    
---------------------------------------------------------------------------------------------------
-- get possible moves from a given position
---------------------------------------------------------------------------------------------------
getPossibleMoves :: CkNode -> [CkMove]
getPossibleMoves n = foldr f [] (getPieceLocs n) where
                        f x r = r ++ pieceMoves n x ++ pieceJumps n x

getPieceLocs :: CkNode -> [Int]
getPieceLocs node = 
    let pos = node ^. ckPosition
        c = pos ^. clr
        pairs = zip [0..45] (pos ^. grid)
    in fmap fst (filter (pMatch c) pairs)
        where pMatch color pair =
                let val = snd pair
                    av = abs val 
                in (av > 0 && av <3 && (val * color) > 0)

isKing :: Int -> Bool
isKing move = abs move > 1

---------------------------------------------------------------------------------------------------
-- calculate available (non-jump) moves
---------------------------------------------------------------------------------------------------
pieceMoves :: CkNode -> Int -> [CkMove]
pieceMoves node idx =
    let pos = node ^. ckPosition
        g = pos ^. grid
    in case g ^? ix idx of
        Nothing -> []
        Just val -> if isKing val then kingMoves g idx else forwardMoves g idx (pos ^. clr) 

forwardMoves :: [Int] -> Int -> Int -> [CkMove]
forwardMoves g idx color = 
    let newIdxs = filter f [idx + (color * 4), idx + (color * 5)] 
        f idx = case g ^? ix idx of
                        Nothing -> False
                        Just val -> val == 0
    in fmap h newIdxs where
        h newIdx = CkMove {_isJump = False, _startIdx = idx, _endIdx = newIdx, _middleIdxs = [], _removedIdxs = []}       
    
kingMoves :: [Int] -> Int -> [CkMove]
kingMoves g idx = forwardMoves g idx (-1) ++ forwardMoves g idx 1

---------------------------------------------------------------------------------------------------
-- calculate available jumps
---------------------------------------------------------------------------------------------------
pieceJumps :: CkNode -> Int -> [CkMove]
pieceJumps node idx = 
    let pos = node ^. ckPosition
        g = pos ^. grid
        color = pos ^. clr
    in case g ^? ix idx of
            Nothing -> []
            Just val -> if isKing val 
                then kingJumps g idx color 
                else forwardJumps g idx color color
    
--jumpSeqToMap :: JumpSeq -> CkMove
--jumpSeqToMap js = IntMove $ 10000 + js ^. start * 100 + js ^. end    
                        
--todo: implement multi-step jumps
forwardJumps :: [Int] -> Int -> Int -> Int -> [CkMove]
forwardJumps g idx color jumpDir = 
    let newIdxPairs = filter f [(idx + (jumpDir * 4), idx + (jumpDir * 8)), 
                                (idx + (jumpDir * 5), idx + (jumpDir * 10))] 
        f idxPair = case (g ^? ix (fst idxPair), g ^? ix (snd idxPair)) of
                        (Nothing, _) -> False
                        (_, Nothing) -> False
                        (Just jumpOver, Just landing) -> 
                            landing == 0 && jumpOver /= 0 && jumpOver /= 99 && jumpOver * color < 0 
        h pair = CkMove {_isJump = True, _startIdx = idx, _endIdx = snd pair, _middleIdxs = [], _removedIdxs = [fst pair]}   
    in fmap h newIdxPairs 

   
kingJumps :: [Int] -> Int -> Int -> [CkMove]
kingJumps g idx color = forwardJumps g idx color (-1) ++ forwardJumps g idx color 1




    


