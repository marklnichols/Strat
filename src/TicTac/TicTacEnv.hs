module TicTac.TicTacEnv where

import Control.Monad.Reader
import StratTree.TreeNode

ticTacEnv = Env {_depth = 5, _errorDepth = 5, _equivThreshold = 0, _errorEquivThreshold = 10,
     _p1Comp = True, _p2Comp = False}

