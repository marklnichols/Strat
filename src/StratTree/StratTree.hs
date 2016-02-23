module StratTree.StratTree ( best, best', expandTree, processMove) where

import StratTree.Internal.Trees
import StratTree.TreeNode
import StratTree.Internal.General
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Tuple.Select

-------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------
--best' :: tree -> depth -> color -> ([best path])
best' :: TreeNode t => Tree t -> Int -> Int -> [Int]
best' tree depth color = sel1 $ best tree depth color


--best :: tree -> depth -> color -> ([best path], [equiv random choices], best score))
best :: TreeNode t => Tree t -> Int -> Int -> ([Int], [Int], Int)
best tree depth color = 
    let (path, rChoices, bestScore) = findBest tree depth color
        path' = tail path   --without the tree's starting "move"
        randChoices = head path' : rChoices   --current best move is one of the random choices not in the list
        moves = resolveRandom path' randChoices
    in (moves, randChoices, bestScore) 

    
--process a chosen move - prune the tree down so the selected move is the new head 
-- if there are no child moves at all, create a tree with just the single position corresponding to the move  
--processMove :: tree -> move -> tree
processMove :: PositionNode n => Tree n -> Int -> Tree n
processMove tree move = case subForest tree of 
    [] -> Node (newNode (rootLabel tree) move) []
    xs -> pruneToChild tree move  

    
--expandTree :: tree -> depth -> tree
expandTree :: PositionNode n => Tree n -> Int -> Tree n
expandTree tree maxDepth = visitTree tree maxDepth visitor  
    
---------------------------------------------------------------------------------------------------
-- non-exported functions
---------------------------------------------------------------------------------------------------    
--resolve any random choices for the next move into a particular choice    
--resolveRandom :: [path of moves incl. a default choice] -> [random equivalent choices] -> [new path of moves]
resolveRandom :: [Int] -> [Int] -> [Int]    
resolveRandom moves randChoices = moves     --nop   
{--
-- Random monad
Rand g a
Rand g Int

die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)      --from class MonadRandom

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die)

class (Monad m) => MonadRandom m where
    -- | Return a randomly-selected value of type @a@.  See
    -- 'System.Random.random' for details.
    getRandom :: (Random a) => m a
    -- | Return an infinite stream of random values of type @a@.  See
    -- 'System.Random.randoms' for details.
    getRandoms :: (Random a) => m [a]
    -- | Return a randomly-selected value of type @a@ in the range
    -- /[lo,hi]/.  See 'System.Random.randomR' for details.
    getRandomR :: (Random a) => (a,a) -> m a

--instances of MonadRandom include Control.Monad.Random.Rand

    
    
    
mkStdGen :: Int -> StdGen
random (mkStdGen 100) :: (Int, StdGen)  --LYAHFGG

random :: (RandomGen g, Random a) = > g -> (a, g)

randomR :: RandomGen g => (a, a) -> g -> (a, g)     --(a, a) === (lo, hi)    

runRand :: Rand g a -> g -> (a, g) 

main = do
  values <- evalRandIO (dice 2)
  putStrLn (show values)
  
----------------------------------------
so -- create generator from main / commandline
either pass in n to mkStGen n (for repeatability)
or if not passed in, use:
    x <- getStdGen :: IO StdGen

--then create the monad rand = (Rand g Int) from the generator g and pass that around
  
--!! the resolveRand function doesnt need the generator, since it returns the monad which can later
yield a value / new gen by passing in a gen.  
  

--generate the rand by:


and then extract the random value with runRand rand   
  
--}   
   
   
--findBest :: tree -> depth -> color -> ([best mv path], [equiv random choices], best score)
findBest :: TreeNode t => Tree t -> Int -> Int -> ([Int], [Int], Int)
findBest (Node n []) depth color = ([getMove n], [], color * getValue n)
findBest (Node n xs) 0 color = ([getMove n], [], color * getValue n)
findBest (Node n xs) depth color = 
    let f = bestFold depth color
    in
        let (bestMvs, randChoices,  bestVal) = foldl f ([], [], minBound) xs
        in (getMove n : bestMvs, randChoices, bestVal)

        
--bestFold :: depth -> color -> ([best move list], [equiv random choices], best score) 
bestFold :: TreeNode t => Int -> Int -> ([Int], [Int], Int) -> Tree t -> ([Int], [Int], Int)
bestFold depth color (rMvs, randChoices, rVal) t = 
    let (mvs, _, v) = findBest t (depth - 1) (-color)
        (newMvs, newVal) = (mvs, -v)
    in
        --if rVal < newVal then (newMvs, newVal) else (rMvs, rVal)
        case rVal `compare` newVal of 
            EQ -> (rMvs, head newMvs : randChoices, rVal)
            LT -> (newMvs, randChoices, newVal)
            GT ->  (rMvs, randChoices, rVal)
