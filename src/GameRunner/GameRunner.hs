module GameRunner where
import TicTac
import Checkers
import StratTree.TreeNode
import StratTree.StratTree
import StratTree.Trees
import StratIO.StratIO
import Data.Tree
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens

class Output o where 
    out :: o -> String -> IO ()
    updateAll :: 0 -> TBD -> IO ()
    getMove :: o -> TBD
  
gameEnv = Env {_depth = 6, _errorDepth = 4, _equivThreshold = 0, _errorEquivThreshold = 0,
     _p1Comp = False, _p2Comp = True}

startGame :: Output a => a -> [String] -> IO ()
startGame ["tictac"]   = loop getTicTacStart 1
startGame ["checkers"] = loop getCheckersStart 1
startGame _            = out "Usage: main tictac | checkers" -- TBD fix this

getTicTacStart :: Tree TTNode
getTicTacStart = TicTac.getStartNode

getCheckersStart :: Tree CkNode
getCheckersStart = Checkers.getStartNode

loop :: PositionNode n m e => Tree n -> Int -> IO ()
loop node turn = do
    out $ showPosition $ rootLabel node
    out ("Current position score: " ++ show (getValue (rootLabel node)))
    out ""
    theNext <- case final $ rootLabel node of
        WWins -> do
            out "White wins."
            return Nothing
        BWins -> do
            out "White wins."
            return Nothing
        Draw -> do
            out "Draw."
            return Nothing
        _ -> do
            nextNode <- if evalState (runReaderT (unRST (isCompTurn turn)) gameEnv) (GameState 0)
                            then computerMove node turn
                            else  playerMove node turn
            return (Just nextNode)
    case theNext of
        Nothing -> return ()
        Just next -> loop next (swapTurns turn)

playerMove :: PositionNode n m e => Tree n -> Int -> IO (Tree n)
playerMove tree turn = do
    out ("Enter player " ++ show turn ++ "'s move:")
    line <- getLine
    out ""
    case parseMove (rootLabel tree) line of
        Left err -> do
            out err
            playerMove tree turn
        Right mv -> if not (isLegal tree mv)
            then do
                out "Not a legal move."
                playerMove tree turn
            else return (processMove tree mv)

computerMove :: PositionNode n m e => Tree n -> Int -> IO (Tree n)
computerMove node turn = do
    out "Calculating computer move..."
    let newTree = evalState (runReaderT (unRST (expandTree node)) gameEnv) (GameState 0)
    let resultM = evalState (runReaderT (unRST (best newTree (turnToColor turn))) gameEnv) (GameState 0)
    case resultM of
        Nothing -> do
            out "Invalid result returned from best"
            exitFailure
        Just result -> do
            let badMovesM = checkBlunders newTree (turnToColor turn) (result^.moveScores)
            let badMoves = evalState (runReaderT (unRST badMovesM) gameEnv) (GameState 0)
            let finalChoices = fromMaybe (result ^. moveScores) badMoves
            out ("Choices from best: " ++ show (result^.moveScores))
            out ("Choices after checkBlunders: " ++ show finalChoices)
            moveM <- resolveRandom finalChoices
            case moveM of
                Nothing -> do
                    out "Invalid result from resolveRandom"
                    exitFailure
                Just move -> do
                    printMoveChoiceInfo newTree result move
                    return (processMove newTree move)

printMoveChoiceInfo :: PositionNode n m e => Tree n -> Result m e -> m -> IO ()
printMoveChoiceInfo tree result move = do
    out ("Tree size: " ++ show (treeSize tree))
    out ("Equivalent best moves: " ++ show (result^.moveChoices))
    out ("Following moves: " ++ show ( result^.followingMoves))
    out ("Computer's move:\n (m:" ++ show move ++
                  ", s:" ++ show (_score $ head $ result^.moveScores) ++ ")")
    out ""

isCompTurn :: Int -> RST Bool
isCompTurn turn = do
    p1 <- asks _p1Comp
    p2 <- asks _p2Comp
    return $ if turn == 1 then p1 else p2

--toBool :: "C" or "c" for computer -> True, "H" or "h" (or anything else for that matter) for Human -> False
toBool :: String -> Bool
toBool s = s == "c" || s == "C"

swapTurns :: Int -> Int
swapTurns t = 3-t   --alternate between 1 and 2

-- convert 1, 2 to +1, -1
turnToColor :: Int -> Int
turnToColor 2 = -1
turnToColor _ = 1
