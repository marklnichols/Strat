{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Checkers where
import qualified CkParser as Parser
import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.ST
import Data.HashTable.ST.Basic
import Data.Tree
import StratTree.TreeNode hiding (MoveScore, Result)
import Data.List.Lens
import Control.Lens
import Data.List hiding (lookup, insert)
import Data.List.Split
import Data.Char
import Data.Maybe
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

data JumpOff = JumpOff {_jmpOver :: Int, _land :: Int}
makeLenses ''JumpOff 

instance PositionNode CkNode CkMove where
    newNode = calcNewNode
    possibleMoves = getPossibleMoves
    color = view (ckPosition . clr)
    final = view (ckPosition . fin)
    showPosition = format
    parseMove = parseCkMove
    


instance TreeNode CkNode CkMove where
    getMove = _ckMove
    getValue = _ckValue
    getErrorValue = _ckErrorValue
    
instance Show CkMove where
    show move = case toParserMove move of
                    Just m -> show m
                    Nothing -> show move
        
instance Show CkNode where
    show n = "move: " ++ show (n ^. ckMove) ++ " value: " ++ show (n ^. ckValue) ++ " errorValue: " 
             ++ show (n ^. ckErrorValue) ++ " position: " ++ show (n ^. ckPosition) 

instance Move CkMove    

---------------------------------------------------------------------------------------------------
-- parse string input to move
---------------------------------------------------------------------------------------------------     
parseCkMove :: CkNode -> String -> Either String CkMove
parseCkMove n s 
    | Left err <- pMove   = Left err
    | Right x  <- pMove   = toCkMove n x
        where
        pMove = Parser.run s  
  
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
format node = loop (node^.ckPosition^.grid) 40 "" where
    loop xs 4 result = result ++ "\n" ++ colLabels
    loop xs n result = loop xs (newIdx - 4) (result ++ rowToStr xs newIdx spaces) where
        (newIdx, spaces) = case n `mod` 9 of
            0 -> (n-1, "") 
            4 -> (n, "   ")   
  
                                                            
rowToStr :: [Int] -> Int -> String -> String
rowToStr xs i spaces =  Map.findWithDefault "??" i labelMap ++ "  " ++ spaces ++ 
                            toXOs (xs !! (i-3)) ++ gap ++
                            toXOs (xs !! (i-2)) ++ gap ++
                            toXOs (xs !! (i-1)) ++ gap ++
                            toXOs (xs !!    i)  ++ "\n"

gap = "     "
                            
toXOs :: Int -> String
toXOs 1 = "x"
toXOs (-1) = "o"
toXOs (2) = "X"
toXOs (-2) = "O"
toXOs 0 = "-"
toXOs _ = "?"

labelMap = Map.fromList [(40, "H"), (35, "G"), (31, "F"), (26, "E"), (22, "D"), (17, "C"), (13, "B"), (8, "A")]
  
colLabels = "   " ++ intercalate "  " ["1", "2", "3", "4", "5", "6", "7", "8"]

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

        
rowIndexes = Map.fromList [('A', 5), ('B', 10), ('C', 14), ('D', 19), ('E', 23), ('F', 28), ('G', 32), ('H', 37)] 
 
---------------------------------------------------------------------------------------------------
-- Convert CkMove to Parser Move (for display)
---------------------------------------------------------------------------------------------------
toParserMove :: CkMove -> Maybe Parser.Move
toParserMove move = 
    let mAll = intToLoc (move^.startIdx) : fmap intToLoc (move^.middleIdxs) ++ [intToLoc (move^.endIdx)]
        locs = catMaybes mAll
    in  if length locs == length mAll
            then Just $ Parser.Move locs
            else Nothing
 
intToLoc :: Int -> Maybe Parser.Loc
intToLoc n = 
    let num = offset n + colPlus n + 1
        mChar = rowLabels ^? element (labelIdx n)
    in case mChar of 
        Nothing -> Nothing
        Just c -> Just $ Parser.Loc c num

offset :: Int -> Int
offset n =  if ((n-5) `mod` 9) `div` 5 == 0 then 0 else 1

colPlus :: Int -> Int
colPlus n = (((n-5) `mod` 9) `mod` 5) * 2

labelIdx :: Int -> Int
labelIdx n = ((n-5) `div` 9) * 2 + ((n-5) `mod` 9) `div` 5
 
rowLabels = ['A'..'H']
 
---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: CkNode -> CkMove -> CkNode
calcNewNode node mv =
    let moved = movePiece node (mv ^. startIdx) (mv ^. endIdx) 
        captured = removeMultiple moved (mv ^. removedIdxs)      --remove any captured pieces
        clrFlipped = set (ckPosition . clr) (flipColor (captured ^. ckPosition ^. clr)) captured
        
        (score, finalSt) = evalNode clrFlipped
        errScore = errorEvalNode clrFlipped
        
        finSet = set (ckPosition . fin) finalSt clrFlipped
        scoreSet = set ckValue score finSet
        errScoreSet = set ckErrorValue score scoreSet
    in  set ckMove mv errScoreSet   
 
removePiece :: CkNode -> Int -> CkNode
removePiece node index = set (ckPosition . grid . ix index) 0 node

removeMultiple :: CkNode -> [Int] -> CkNode
removeMultiple = foldr (flip removePiece)    
     
movePiece :: CkNode -> Int -> Int -> CkNode
movePiece node from to = 
    let value = node ^? (ckPosition . grid . ix from) 
        valid = value >>= validPiece node
    in case valid of 
        Nothing -> node
        Just x ->  let z = checkPromote node x to
                       p = set (ckPosition . grid . ix to) z node
                   in removePiece p from    
        
validPiece :: CkNode -> Int -> Maybe Int
validPiece node x = if x /= 0 && abs x < 3 then Just x else Nothing

checkPromote :: CkNode -> Int -> Int -> Int
checkPromote node value to
    | color > 0 && to > 36  = 2 * color
    | color < 0 && to < 9   = 2 * color
    | otherwise             = value
        where color = node^.ckPosition^.clr
     
--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
homeRowFull = 4
homeRowPartial = 2
homeRowNone = 0

kingVal = 15
pieceVal = 5
mobilityVal = 1

finalValW = 1000
vinalValB = -1000

evalNode :: CkNode -> (Int, FinalState)
evalNode n = let g =   n ^. ckPosition ^. grid
                 mat = kingCount g * kingVal + pieceCount g * pieceVal
                 mob = mobility n * mobilityVal
                 home = homeRow g 
                 final = checkFinal n
             in case final of
                    NotFinal -> (mat + mob + home, final)
                    WWins    -> (finalValW, final)
                    BWins    -> (vinalValB, final)
                          
--TODO: add draw conditions             
checkFinal :: CkNode -> FinalState
checkFinal n
    | moveCount n /= 0        = NotFinal
    | n^.ckPosition^.clr == 1 = BWins
    | otherwise               = WWins
 
-- TODO: implement
errorEvalNode :: CkNode -> Int
errorEvalNode n = fst $ evalNode n

pieceCount :: [Int] -> Int
pieceCount grid = count grid 1

kingCount :: [Int] -> Int
kingCount grid = count grid 2

count :: [Int] -> Int -> Int
count grid p = 
    let w = length $ filter (== p) grid  
        b = length $ filter (== (-p)) grid
    in  w-b
   
mobility :: CkNode -> Int
mobility node = wMoves - bMoves where
    wMoves = moveCount (node & ckPosition.clr .~ 1)
    bMoves = moveCount (node & ckPosition.clr .~ (-1))
    
homeRow :: [Int] -> Int
homeRow grid = homeRow' grid 1 - homeRow' grid (-1)

homeRow' :: [Int] -> Int -> Int
homeRow' grid 1    = checkRow grid 1 [5..8]
homeRow' grid (-1) = checkRow grid (-1) [37..40] 

checkRow :: [Int] -> Int -> [Int] -> Int
checkRow grid color range 
    | len == 4  = homeRowFull
    | len == 3  = homeRowPartial
    | len == 2  = if checkTwo then homeRowPartial else homeRowNone
    | otherwise = homeRowNone
    where 
        zipped = zip range (fmap (\x -> grid ^? ix x) range)
        filtered = filter (\(x, y) -> y == Just color || y == Just (color * 2)) zipped
        len = length filtered 
        checkTwo = foldl f 0 (fmap fst filtered) == 2
            where f :: Int -> Int -> Int
                  f r x = x - r 
            
---------------------------------------------------------------------------------------------------
-- get possible moves from a given position
---------------------------------------------------------------------------------------------------
getPossibleMoves :: CkNode -> [CkMove]
getPossibleMoves n = requireJumps $ foldr f [] (getPieceLocs n) where
                        f x r = r ++ pieceMoves n x ++ pieceJumps n x

requireJumps :: [CkMove] -> [CkMove]
requireJumps xs = case filter (^. isJump) xs of
                    [] -> xs    --no jumps
                    js -> js    --return only the jumps
                                               
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

moveCount :: CkNode -> Int
moveCount n = length $ getPossibleMoves n

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
-- calculate (possibly multiple) available jumps for a piece at a given index
---------------------------------------------------------------------------------------------------
pieceJumps :: CkNode -> Int -> [CkMove]
pieceJumps node idx = 
    let pos = node ^. ckPosition
        g = pos ^. grid
        color = pos ^. clr
    in case g ^? ix idx of
            Nothing -> []
            Just val -> multiJumps g color (offsets val) idx where
                offsets x
                    | isKing val = [JumpOff 4 8, JumpOff 5 10, JumpOff (-4) (-8), JumpOff (-5) (-10)]
                    | otherwise  = [JumpOff (4 * color) (8 * color), JumpOff (5 * color) (10 * color)]

                    
multiJumps :: [Int] -> Int -> [JumpOff] -> Int -> [CkMove]
multiJumps grid color offsets index = jmpsToCkMoves $ fmap reverse (outer grid index []) where
    outer g x xs = 
        let (nextIdxs, gNew) = jmpIndexes g color x offsets
        in case nextIdxs of
            [] -> [x : xs]
            _  -> inner gNew x nextIdxs xs where
                    inner gNew' y ys acc =  let acc'  = y : acc
                                                f x r = outer gNew' x acc' ++ r
                                            in foldr f [[]] ys
                                
                                      
jmpIndexes :: [Int] -> Int -> Int -> [JumpOff] -> ([Int], [Int])                  
jmpIndexes g color startIdx jos = 
    let pairs = catMaybes $ fmap f jos
        validIdxs = fmap fst pairs
        removed = fmap snd pairs
        newG = removeCaptured g removed
        f x = if isValidJump g color (startIdx + x ^. jmpOver) (startIdx + x ^. land)
                then Just (startIdx + x ^. land, startIdx + x ^. jmpOver) 
                else Nothing 
    in (validIdxs, newG)
   
               
isValidJump :: [Int] -> Int -> Int -> Int -> Bool
isValidJump g color loc1 loc2 = 
    let mOver = g ^? ix loc1
        mLand = g ^? ix loc2
    in fromMaybe False (liftM2 check mOver mLand)
        where 
            check over land = land == 0 && over /= 0 && over /= 99 && over * color < 0
 
 
removeCaptured :: [Int] -> [Int] -> [Int]
removeCaptured = foldr f where
                    f :: Int -> [Int] -> [Int]
                    f idx grid' = grid' & ix idx .~ 0   
                                              
 
jmpsToCkMoves :: [[Int]] -> [CkMove] 
jmpsToCkMoves = foldr f [] where
    f []  r = r
    f [x] r = r 
    f xs  r = CkMove { _isJump = True, _startIdx = head xs, _endIdx = last xs, 
                       _middleIdxs = init $ tail xs, 
                       _removedIdxs = fmap (\(m, n) -> (n-m) `div` 2 + m) (zip xs (tail xs))
                     } : r     
       

{--
let range = [37..40]
let grid = board10
let zipped = zip range (fmap (\x -> grid ^? ix x) range)
let color = -1
let filtered = filter (\(x, y) -> y == Just color || y == Just (color * 2)) zipped

let f x r = x - r
foldr f 0 (fmap fst filtered)
--}