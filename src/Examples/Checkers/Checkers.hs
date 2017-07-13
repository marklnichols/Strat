{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Checkers where
import qualified CkParser as Parser
import Prelude hiding (lookup)
import Control.Monad
import Data.Tree
import StratTree.TreeNode hiding (MoveScore, Result)
import Control.Lens
import Data.Char
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map
import Safe

---------------------------------------------------------------------------------------------------
-- Data types, type classes
---------------------------------------------------------------------------------------------------
data CkPosition = CkPosition {_grid :: V.Vector Int, _clr :: Int, _fin :: FinalState} deriving (Show)
makeLenses ''CkPosition

data CkMove = CkMove {_isJump :: Bool, _startIdx :: Int, _endIdx :: Int, _middleIdxs :: [Int], _removedIdxs :: [Int]}
    deriving (Eq, Ord)
makeLenses ''CkMove

data CkEval = CkEval {_total :: Int, _details :: String}
    deriving (Eq, Ord)
makeLenses ''CkEval

data CkNode = CkNode {_ckMove :: CkMove, _ckValue :: CkEval, _ckErrorValue :: CkEval, _ckPosition :: CkPosition}
makeLenses ''CkNode

data JumpOff = JumpOff {_jmpOver :: Int, _land :: Int}
makeLenses ''JumpOff

data PieceLoc = PieceLoc {pieceLoc :: Parser.Loc, pieceLocValue :: Int}

data PieceList = PieceList {pieceLocs :: [PieceLoc]}

instance PositionNode CkNode CkMove CkEval where
    newNode = calcNewNode
    possibleMoves = getAllowedMoves
    color = view (ckPosition . clr)
    final = view (ckPosition . fin)
    parseMove = parseCkMove

instance TreeNode CkNode CkMove CkEval where
    getMove = _ckMove
    getValue = _ckValue
    getErrorValue = _ckErrorValue

instance Show CkMove where
    show mv = show $ toParserMove mv
    
instance Show CkNode where
    show n = "move: " ++ show (n ^. ckMove) ++ " value: " ++ show (n ^. ckValue) ++ " errorValue: "
             ++ show (n ^. ckErrorValue) ++ " position: " ++ show (n ^. ckPosition)

instance Show CkEval where
    show e = "Total: " ++ show (e ^. total) ++ " (made up of " ++ (e ^. details) ++ ")"

instance Move CkMove

instance Eval CkEval where
    getInt e = e ^. total
    setInt e n = e & total .~ n
    fromInt n = CkEval {_total = n, _details = ""}

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
    CkNode {_ckMove = CkMove {_isJump = False, _startIdx = -1, _endIdx = -1, _middleIdxs = [],
            _removedIdxs = []}, _ckValue = CkEval {_total = 0, _details = ""},
            _ckErrorValue = CkEval {_total = 0, _details = ""},
            _ckPosition = CkPosition {_grid = mkStartGrid 1, _clr = 1, _fin = NotFinal}} []

---------------------------------------------------------------------------------------------------
-- Grid layout - indexes 0-45
---------------------------------------------------------------------------------------------------
{-- how indexes relate to board position (indexes in parens are not displayed):

   (41) (42) (43) (44) (45)

8|     37  38  39  40
7|   32  33  34  35  (36)
6|     28  29  30  31
5|   23  24  25  26  (27)
4|     19  20  21  22
3|   14  15  16  17  (18)
2|     10  11  12  13
1|   05  06  07  08  (09)

   (00) (01) (02) (03) (04)
     ---------------
     A B C D E F G H
--}

offBoard :: [Int]
offBoard = [0, 1, 2, 3, 4, 9, 18, 27, 36, 41, 42, 43, 44, 45]

--------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Initial board position
---------------------------------------------------------------------------------------------------
mkStartGrid :: Int -> V.Vector Int
mkStartGrid bottomColor =  V.map (indexToValue bottomColor) (V.fromList [0..45])

indexToValue :: Int -> Int -> Int
indexToValue bottomColor idx
    | idx < 5               = 99           -- off the edge
    | idx > 40              = 99           -- off the edge
    | idx `mod` 9 == 0      = 99           -- off the edge
    | idx > 18 && idx < 27  = 0            -- center, no initial pieces
    | idx < 18              = bottomColor  -- player at bottom of board initial pieces
    | otherwise             = negate bottomColor -- player at top initial pieces

---------------------------------------------------------------------------------------------------
-- Convert Parser's Move type to CkMove
---------------------------------------------------------------------------------------------------
--supports infering move from single loc if there is a unique one
--TODO fix: one move + one jump should == unique jump
toCkMove :: CkNode -> Parser.Move -> Either String CkMove
toCkMove node (Parser.Move xs) =     --parser guarentees at least one item in list. TODO: move to non-empty list
    let xs' = fmap locToInt xs
    in case length xs' of
        1 -> fromSingleLoc node (head xs')
        _ -> Right $ fromMultLoc xs'

--This variant doesnt require CkNode, but doesn't support inferring move from a single loc
parserToCkMove :: Parser.Move -> Maybe CkMove
parserToCkMove (Parser.Move xs)
    | length xs < 2     = Nothing
    | otherwise         = Just $ fromMultLoc $ fmap locToInt xs

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
        remvd = calcRemoved xs
        isJmp = not (null remvd)
    in  CkMove { _isJump = isJmp, _startIdx = st, _endIdx = end, _middleIdxs = mid,
                 _removedIdxs = remvd }


calcRemoved :: [Int] -> [Int]
calcRemoved xs = foldr f [] (zip xs (tail xs))  where
    f (prevX, x) r
        | abs (x - prevX) `elem` [8, 10] = prevX + (x-prevX) `div` 2 : r
        | otherwise                      = r

locToInt :: Parser.Loc -> Int  -- parser ensures valid key
locToInt (Parser.Loc c d) = 
    let col = ord (toUpper c) - 65
    in Map.findWithDefault (-1) d rowIndexes + (col `div` 2) 
     
rowIndexes :: Map.Map Int Int
rowIndexes = Map.fromList [(1, 5), (2, 10), (3, 14), (4, 19), (5, 23), (6, 28), (7, 32), (8, 37)]

---------------------------------------------------------------------------------------------------
-- Convert CkMove to Parser Move
---------------------------------------------------------------------------------------------------
toParserMove :: CkMove -> Parser.Move
toParserMove mv =
    let mAll = intToLoc (mv^.startIdx) : fmap intToLoc (mv^.middleIdxs) ++ [intToLoc (mv^.endIdx)]
    in Parser.Move mAll
    
maybeToParserMove :: Maybe CkMove -> Parser.Move
maybeToParserMove (Just mv) = toParserMove mv    
maybeToParserMove Nothing   = Parser.Move []          
      
intToLoc :: Int -> Parser.Loc
intToLoc n =
    let mchar = chr $ 65 + offset n + colPlus n
        num = rowNum n
    in Parser.Loc mchar num      

offset :: Int -> Int
offset n =  if ((n-5) `mod` 9) `div` 5 == 0 then 0 else 1

colPlus :: Int -> Int
colPlus n = (((n-5) `mod` 9) `mod` 5) * 2

rowNum :: Int -> Int
rowNum n = ((n-5) `div` 9) * 2 + ((n-5) `mod` 9) `div` 5 + 1

rowLabels :: String     
rowLabels = ['A'..'H']

---------------------------------------------------------------------------------------------------
-- Convert Board to a PieceList (for conversion to JSon, etc.)
---------------------------------------------------------------------------------------------------
boardAsPieceList :: CkNode -> PieceList
boardAsPieceList node =
    let g = node ^. (ckPosition . grid)
        range = V.fromList [0..45] :: V.Vector Int
        pairs = V.zip range g :: V.Vector (Int, Int)
        filtrd = V.filter pMatch pairs :: V.Vector (Int, Int) where
            pMatch pair = (abs (snd pair) > 0 && abs (snd pair) < 3)
        pLocs = V.foldr f z0 filtrd where
            z0 = [] :: [PieceLoc]
            --f x r = case intToLoc $ fst x of
            --            Nothing  -> r
            --            Just l -> PieceLoc {pieceLoc = l, pieceLocValue = snd x} : r
            f x r = PieceLoc {pieceLoc = intToLoc $ fst x, pieceLocValue = snd x} : r
        in PieceList {pieceLocs = pLocs}

---------------------------------------------------------------------------------------------------
-- calculate new node from a previous node and a move
---------------------------------------------------------------------------------------------------
calcNewNode :: CkNode -> CkMove -> CkNode
calcNewNode node mv =
    let moved = movePiece node (mv ^. startIdx) (mv ^. endIdx)
        captured = removeMultiple moved (mv ^. removedIdxs)      --remove any captured pieces
        clrFlipped = set (ckPosition . clr) (negate (captured ^. ckPosition ^. clr)) captured
        (eval, finalSt) = evalNode clrFlipped
        errEval = errorEvalNode clrFlipped
        finSet = set (ckPosition . fin) finalSt clrFlipped
        scoreSet = set ckValue eval finSet
        errScoreSet = set ckErrorValue errEval scoreSet
    in  set ckMove mv errScoreSet

removePiece :: CkNode -> Int -> CkNode
removePiece node idx = set (ckPosition . grid . ix idx) 0 node

removeMultiple :: CkNode -> [Int] -> CkNode
removeMultiple = foldr (flip removePiece)

movePiece :: CkNode -> Int -> Int -> CkNode
movePiece node pFrom pTo =
    let value = node ^? (ckPosition . grid . ix pFrom)
        valid = value >>= validPiece node
    in case valid of
        Nothing -> node
        Just x ->  let z = checkPromote node x pTo
                       p = set (ckPosition . grid . ix pTo) z node
                   in removePiece p pFrom

validPiece :: CkNode -> Int -> Maybe Int
validPiece _ x = if x /= 0 && abs x < 3 then Just x else Nothing

checkPromote :: CkNode -> Int -> Int -> Int
checkPromote node value toLoc
    | colr > 0 && toLoc > 36  = 2 * colr
    | colr < 0 && toLoc < 9   = 2 * colr
    | otherwise                = value
        where colr = node^.ckPosition^.clr

--------------------------------------------------------
-- Position Evaluation
--------------------------------------------------------
homeRowFull :: Int
homeRowFull    = 4      -- (home row values for non-kings only)

homeRowPartial :: Int
homeRowPartial = 2

homeRowNone :: Int
homeRowNone    = 0

kingVal :: Int
kingVal        = 15

pieceVal :: Int
pieceVal       = 5

mobilityVal :: Int
mobilityVal    = 1      -- range 0-2 for non-kings, 0-4 for kings

progressVal :: Int
progressVal    = 1      -- range: 0-4 (for non-kings only)

kProximityVal :: Int
kProximityVal  = 1      -- range 0-6, (for kings only)

finalValW :: Int
finalValW =  1000

finalValB :: Int
finalValB = -1000

drawVal :: Int
drawVal   =  0

evalNode :: CkNode -> (CkEval, FinalState)
evalNode n = let g =   n ^. ckPosition ^. grid
                 mat = totalKingCount g * kingVal + totalPieceCount g * pieceVal
                 mob = mobility n * mobilityVal
                 home = homeRow g
                 prog = progress n * progressVal
                 kProximity = kingProximity n * kProximityVal
                 finl = checkFinal n
             in case finl of
                    NotFinal ->
                        (CkEval {_total = mat + mob + home + prog,
                                 _details = "material<" ++ show mat ++ ">, mobility<" ++ show mob 
                                            ++ ">, home row occupation<"
                                            ++ show home ++ ">, forward progress<" ++ show prog 
                                            ++ ">, kings' proximity to enemy pieces<"
                                            ++ show kProximity ++ "> "}, finl)
                    WWins    -> (CkEval {_total = finalValW,
                                        _details="white wins<" ++ show finalValW ++ ">"}, finl)
                    BWins    -> (CkEval {_total = finalValB,
                                         _details="black wins<" ++ show finalValB ++ ">"}, finl)
                    Draw     -> (CkEval {_total = drawVal,
                                         _details="draw<" ++ show drawVal ++ ">"}, finl)

checkFinal :: CkNode -> FinalState
checkFinal n
    | numPieces == 0    = colorToWinState $ negate colr
    | moveCount n == 0  = Draw
    | otherwise         = NotFinal
        where
            g = n^.ckPosition^.grid
            numPieces = pieceCount g colr + kingCount g colr
            colr = n^.ckPosition^.clr

colorToWinState :: Int -> FinalState
colorToWinState 1 = WWins
colorToWinState _ = BWins

-- TODO: implement
errorEvalNode :: CkNode -> CkEval
errorEvalNode n = fst $ evalNode n

totalPieceCount :: V.Vector Int -> Int
totalPieceCount grd = pieceCount grd 1 - pieceCount grd (-1)

totalKingCount :: V.Vector Int -> Int
totalKingCount grd = kingCount grd 1 - kingCount grd (-1)

pieceCount :: V.Vector Int -> Int -> Int
pieceCount grd colr = V.length $ V.filter (== colr) grd

kingCount :: V.Vector Int -> Int -> Int
kingCount grd colr = V.length $ V.filter (== 2 * colr) grd

mobility :: CkNode -> Int
mobility node = wMoves - bMoves where
    wMoves = moveCount (setColor node 1)
    bMoves = moveCount (setColor node (-1))

homeRow :: V.Vector Int -> Int
homeRow grd = homeRow' grd 1 - homeRow' grd (-1)

homeRow' :: V.Vector Int -> Int -> Int
homeRow' grd 1    = checkRow grd 1 [5..8]
homeRow' grd (-1) = checkRow grd (-1) [37..40]
homeRow' _ _    = homeRowNone     --should never happen

checkRow :: V.Vector Int -> Int -> [Int] -> Int
checkRow grd colr range
    | len == 4  = homeRowFull
    | len == 3  = homeRowPartial
    | len == 2  = if checkTwo then homeRowPartial else homeRowNone
    | otherwise = homeRowNone
    where
        zipped = zip range (fmap (\x -> grd ^? ix x) range)
        filtrd = filter (\(_, y) -> y == Just colr || y == Just (colr * 2)) zipped
        len = length filtrd
        checkTwo = foldl f 0 (fmap fst filtrd) == 2
            where f :: Int -> Int -> Int
                  f r x = x - r

progress :: CkNode -> Int
progress node = pieceProgress (getNonKingLocs (setColor node 1)) 1
                -  pieceProgress (getNonKingLocs (setColor node (-1))) (-1)

pieceProgress :: [Int] -> Int -> Int
pieceProgress xs colr =
    let vals    = case colr of
                    1 -> [0, 0, 0, 0,  1,  2,  3,  4] :: [Int]
                    _ -> [4, 3, 2, 1,  0,  0,  0,  0] :: [Int]
        f x r   = r +  fromMaybe 0 (vals ^? ix (rowNum x - 1))
    in foldr f 0 xs

swapColor :: CkNode -> CkNode
swapColor node = setColor node $ negate (node ^. ckPosition.clr)

setColor :: CkNode -> Int -> CkNode
setColor node colr = node & ckPosition.clr .~ colr

---------------------------------------------------------------------------------------------------
-- get possible moves from a given position
---------------------------------------------------------------------------------------------------
getPossibleMoves :: CkNode -> [CkMove]
getPossibleMoves n = foldr f [] (getPieceLocs n) where
                        f x r = r ++ pieceMoves n x ++ pieceJumps n x

getAllowedMoves :: CkNode -> [CkMove]
getAllowedMoves = requireJumps . getPossibleMoves

requireJumps :: [CkMove] -> [CkMove]
requireJumps xs = case filter (^. isJump) xs of
                    [] -> xs    --no jumps
                    js -> js    --return only the jumps

getPieceLocs :: CkNode -> [Int]
getPieceLocs node =
    let pos = node ^. ckPosition
        c = pos ^. clr
        range = V.fromList [0..45] :: V.Vector Int
        pairs = V.zip range (pos ^. grid) :: V.Vector (Int, Int)
        filtrd = V.filter (pMatch c) pairs :: V.Vector (Int, Int)
        first = V.map fst filtrd :: V.Vector Int
    in V.toList first
        where pMatch colr pair =
                let val = snd pair
                    av = abs val
                in (av > 0 && av <3 && (val * colr) > 0)

getNonKingLocs :: CkNode -> [Int]
getNonKingLocs node = filterLocs node isNonKing

getKingLocs :: CkNode -> [Int]
getKingLocs node = filterLocs node isKing

filterLocs :: CkNode -> (Int -> Bool) -> [Int]
filterLocs node test = filter f (getPieceLocs node) where
    g =  node ^. (ckPosition . grid)
    f idx = case g ^? ix idx of
                    Nothing     -> False
                    (Just val)  -> test val

isKing :: Int -> Bool
isKing mv = abs mv > 1

isNonKing :: Int -> Bool
isNonKing mv = not $ isKing mv

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
       
forwardMoves :: V.Vector Int -> Int -> Int -> [CkMove]
forwardMoves g indx colr =
    let newIdxs = filter f [indx + (colr * 4), indx + (colr * 5)]
        f i = case g ^? ix i of
                        Nothing -> False
                        Just val -> val == 0
    in fmap h newIdxs where
        h newIdx = CkMove {_isJump = False, _startIdx = indx, _endIdx = newIdx, _middleIdxs = [], _removedIdxs = []}
        
kingMoves :: V.Vector Int -> Int -> [CkMove]
kingMoves g idx = forwardMoves g idx (-1) ++ forwardMoves g idx 1

---------------------------------------------------------------------------------------------------
-- calculate (possibly multiple) available jumps for a piece at a given index
---------------------------------------------------------------------------------------------------
pieceJumps :: CkNode -> Int -> [CkMove]
pieceJumps node idx =
    let pos = node ^. ckPosition
        grd = pos ^. grid
        colr = pos ^. clr
    in case grd ^? ix idx of
            Nothing -> []
            Just val -> multiJumps grd colr (offsets val) idx where
                offsets x
                    | isKing x = [JumpOff 4 8, JumpOff 5 10, JumpOff (-4) (-8), JumpOff (-5) (-10)]
                    | otherwise  = [JumpOff (4 * colr) (8 * colr), JumpOff (5 * colr) (10 * colr)]

multiJumps :: V.Vector Int -> Int -> [JumpOff] -> Int -> [CkMove]
multiJumps grd colr offsets indx = jmpsToCkMoves $ fmap reverse (outer grd indx []) where
    outer g x xs =
        let (nextIdxs, gNew) = jmpIndexes g colr x offsets
        in case nextIdxs of
            [] -> [x : xs]
            _  -> inner gNew x nextIdxs xs where
                    inner gNew' y ys acc =  let acc'  = y : acc
                                                f z r = outer gNew' z acc' ++ r
                                            in foldr f [[]] ys

jmpIndexes :: V.Vector Int -> Int -> Int -> [JumpOff] -> ([Int], V.Vector Int)
jmpIndexes grd colr start jos =
    let pairs = catMaybes $ fmap f jos
        validIdxs = fmap fst pairs
        removed = fmap snd pairs
        newG = removeCaptured grd removed
        f x = if isValidJump grd colr (start + x ^. jmpOver) (start + x ^. land)
                then Just (start + x ^. land, start + x ^. jmpOver)
                else Nothing
    in (validIdxs, newG)

isValidJump :: V.Vector Int -> Int -> Int -> Int -> Bool
isValidJump grd colr loc1 loc2 =
    let mOver = grd ^? ix loc1
        mLand = grd ^? ix loc2
    in fromMaybe False (liftM2 check mOver mLand)
        where
            check jmpOvr landOn = landOn == 0 && jmpOvr /= 0 && jmpOvr /= 99 && jmpOvr * colr < 0

removeCaptured :: V.Vector Int -> [Int] -> V.Vector Int
removeCaptured = foldr f where
                    f :: Int -> V.Vector Int -> V.Vector Int
                    f idx grid' = grid' & ix idx .~ 0

jmpsToCkMoves :: [[Int]] -> [CkMove]
jmpsToCkMoves = foldr f [] where
    f []  r = r
    f [_] r = r
    f xs  r = CkMove { _isJump = True, _startIdx = head xs, _endIdx = last xs,
                       _middleIdxs = init $ tail xs,
                       _removedIdxs = fmap (\(m, n) -> (n-m) `div` 2 + m) (zip xs (tail xs))
                     } : r

---------------------------------------------------------------------------------------------------
-- Give kings a preference to move towards other pieces
---------------------------------------------------------------------------------------------------
--evaluates white king farthest from any opposing pieces vs. black king farthest from any opposing pieces
kingProximity :: CkNode -> Int
kingProximity node =
    let wAll   = getPieceLocs (setColor node 1)
        bAll   = getPieceLocs (setColor node (-1))
        wKings = getKingLocs (setColor node 1)
        bKings = getKingLocs (setColor node (-1))
        glbClosestW = fromMaybe 7 $ maximumMay $ fmap (`closestToKing` bAll) wKings
        glbClosestB = fromMaybe 7 $ maximumMay $ fmap (`closestToKing` wAll) bKings
        --subtract from 7 to convert distance 1-7 into range 7-1 where closer means a higher score
        -- note: (7 - glbClosestW) - (7 - glbClosestB) === glbClosestB - closestW
        in glbClosestB - glbClosestW

closestToKing :: Int -> [Int] -> Int
closestToKing k foes = fromMaybe 7 $ minimumMay $ fmap (`distance` k) foes

distance :: Int -> Int -> Int
distance p1 p2 = max (hDistance p1 p2) (vDistance p1 p2)

hDistance :: Int -> Int -> Int
hDistance p1 p2 = abs $ (offset p1 + colPlus p1) - (offset p2 + colPlus p2)

vDistance :: Int -> Int -> Int
vDistance p1 p2 = abs $ rowNum p1 - rowNum p2
