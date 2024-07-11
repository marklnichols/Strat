{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module StratWeb.WebRunner
    ( Jsonable(..)
    , NodeWrapper(..)
    , processComputerMove
    , processPlayerMove
    , processStartGame
    ) where

import Control.Monad.Reader
import Data.Aeson

import Data.Text (pack)
import Data.Tree
import Data.Hashable
import Strat.Helpers
import Data.List
import Data.List.Extra (replace)
import Strat.StratIO
import Strat.StratTree.TreeNode
import qualified Strat.ZipTree as Z
import System.Random
import qualified Checkers as Ck
import qualified CheckersJson as J
-- import Text.Printf
-- import Debug.Trace

-- TODO: Split out the checkers-specific code to another file
data CheckersEnv = CheckersEnv
    { ceDepth :: Int, ceCritDepth ::Int,  ceEquivThreshold :: Float, ceP1Comp :: Bool ,ceP2Comp :: Bool } deriving (Show)

-- TODO: Update this to the use of ReaderT, env constraints, etc. as in the Chess example
gameEnv :: Z.ZipTreeEnv
gameEnv = Z.ZipTreeEnv
        { verbose = False
        , enablePruning = True
        , singleThreaded = False
        , enablePruneTracing = False
        , enableCmpTracing = False
        , enableRandom = True
        , maxRandomChange = 0.05
        , enablePreSort = False
        , moveTraceStr = pack ""
        , maxDepth = 6
        , maxCritDepth = 10
        , aiPlaysWhite = False
        , aiPlaysBlack = True
        }

data Jsonable = forall j. ToJSON j => Jsonable j

data NodeWrapper = NodeWrapper { getNode :: Tree Ck.CkNode
                               , getLastMove :: Maybe (MoveScore Ck.CkMove Ck.CkNode)
                               , getJsonable :: Jsonable }

----------------------------------------------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------------------------------------------
--start game request received
processStartGame :: Tree Ck.CkNode -> Bool -> IO (NodeWrapper, StdGen)
processStartGame node bComputerResponse = do
    gen <- getStdGen
    -- TODO: Test this with 1 1
    newTree <- liftIO $ runReaderT
        (expandToSingleThreaded node 2 2) gameEnv
    liftIO $ putStrLn $ "(processStartGame - treesize: " ++ show (Z.treeSize newTree) ++ ")"
    if bComputerResponse
        then do
            resp <- computerResponse newTree gen
            return (resp, gen)
        else return (createUpdate "Score for Player's position: 0" newTree newTree Nothing, gen)

processComputerMove :: RandomGen g => Tree Ck.CkNode -> g -> IO NodeWrapper
processComputerMove tree gen = do
    let posColor = color $ rootLabel tree
    putStrLn $ "Computer move (In processComputerMove), turn = " ++ show (colorToTurn posColor)
    computerResponse tree gen

--player move (web request) received
processPlayerMove :: RandomGen g => Tree Ck.CkNode -> Ck.CkMove -> Bool -> g -> IO NodeWrapper
processPlayerMove t mv bComputerResponse rnds = do
    -- populate 1 deep just so findMove can work with player vs player games
    -- TODO: skip this when not needed
    (tree, _) <- liftIO $ runReaderT
        (searchToSingleThreaded t (Nothing :: Maybe StdGen) 1 1) gameEnv
    case findMove tree mv of
        Left s -> error s -- this shouldn't happen
        Right processed -> do
            let done = checkGameOver processed
            if fst done
                then return $ createMessage (snd done) processed
                else if bComputerResponse
                    then do
                        let posColor = color $ rootLabel processed
                        putStrLn $ "Computer move (In processPlayerMove), turn = "
                                 ++ show (colorToTurn posColor)
                        computerResponse processed rnds
                    else return $ createUpdate "No computer move" processed processed Nothing

computerResponse :: RandomGen g => Tree Ck.CkNode -> g -> IO NodeWrapper
computerResponse prevNode gen = do
   (expandedT, result@Z.NegaResult{..}) <- searchTo prevNode (Just gen) (Z.maxDepth gameEnv)
                                           (Z.maxCritDepth gameEnv)
   putStrLn "\n--------------------------------------------------\n"
   let details = getMoveChoiceInfo expandedT result
   printMoveChoiceInfo expandedT result -- converts <br> to \n
   putStrLn details
   let nextNode = Z.nmNode (Z.picked result)
   let nextMove = getMove nextNode
   let newRoot = findMove expandedT nextMove
   case newRoot of
       Left s -> do
           let newNodeMoves = possibleMoves (rootLabel prevNode)
           liftIO $ putStrLn $ "Available moves:" ++ show newNodeMoves
           return $ createError s prevNode
       Right newTree -> do
          -- note: 'head' is safe in this context...
          let moveScores = mkMoveScores $ head (Z.nmMovePath picked) : (head . Z.nmMovePath  <$> alternatives)
          putStrLn $ "computerMove -- moveScores: " ++ show moveScores
          let (done, doneStr) = checkGameOver newTree
          if done
              then return $ createMessage doneStr newTree
            else do
              let ms = case moveScores of
                    [] -> Nothing
                    (x:_xs) -> Just x
              return $ createUpdate details prevNode newTree ms

searchTo :: (Z.ZipTreeNode a, Hashable a, Ord a, Show a, Eval a, RandomGen g)
         => Tree a -> Maybe g -> Int -> Int -> IO (Tree a, Z.NegaResult a)
searchTo t gen depth critDepth = do
    if Z.singleThreaded gameEnv
      then liftIO $ runReaderT (searchToSingleThreaded t gen depth critDepth) gameEnv
      else liftIO $ runReaderT (searchToMultiThreaded t gen depth critDepth) gameEnv

checkGameOver :: Tree Ck.CkNode -> (Bool, String)
checkGameOver node =
    case final $ rootLabel node of
        WWins -> (True, "White wins.")
        BWins -> (True, "Black wins.")
        Draw  -> (True, "Draw.")
        _     -> (False, "")

--convert +1, -1 to 1, 2
colorToTurn :: Int -> Int
colorToTurn 1 = 1
colorToTurn _ = 2

createMessage :: String -> Tree Ck.CkNode -> NodeWrapper
createMessage s node = NodeWrapper {getNode = node, getLastMove = Nothing,
                                    getJsonable = Jsonable (J.jsonMessage s)}

createUpdate :: String -> Tree Ck.CkNode -> Tree Ck.CkNode
             -> Maybe (MoveScore Ck.CkMove Ck.CkNode) -> NodeWrapper
createUpdate msg prevN newN ms =
    NodeWrapper
      { getNode = newN
      , getLastMove =  ms
      , getJsonable = Jsonable $ J.jsonUpdate msg (rootLabel prevN) (rootLabel newN)
                      (Ck.getAllowedMoves (rootLabel newN) ) ms }

createError :: String -> Tree Ck.CkNode-> NodeWrapper
createError s node = NodeWrapper {getNode = node, getLastMove = Nothing,
                                  getJsonable = Jsonable (J.jsonError s)}

----------------------------------------------------------------------------------------------------
-- Debugging output
----------------------------------------------------------------------------------------------------
printMoveChoiceInfo :: Tree Ck.CkNode -> Z.NegaResult Ck.CkNode -> IO ()
printMoveChoiceInfo tree result = do
    let s = getMoveChoiceInfo tree result
    let replaced = replace "<br>" "\n" s
    putStrLn replaced

getMoveChoiceInfo :: Tree Ck.CkNode -> Z.NegaResult Ck.CkNode -> String
getMoveChoiceInfo tree result =
    let str = "Tree size: " ++ show (Z.treeSize tree) ++ "<br>"
            ++ "Computer's move:<br>" ++ show (Z.picked result) ++ "<br>"
            ++ "score details:" ++ "<br>"
            ++ showScoreDetailsWeb (Ck._ckValue (Z.nmNode (Z.picked result))) ++ "<br>"
            ++ "Alternative moves:" ++ "<br>"
            ++ intercalate "<br>" (show <$> Z.alternatives result)
    in str

-- TODO: better, less confusing text vs web organization is needed here...
showScoreDetailsWeb :: Ck.CkEval -> String
showScoreDetailsWeb eval =
  let str = Ck.showScoreDetails eval
      replaced = replace "\n" "<br>" str
  in replaced
