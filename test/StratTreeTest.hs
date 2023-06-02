{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module StratTreeTest (stratTreeTest) where

import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Data.Hashable
import Data.Text (pack)
import Data.Tree
import GHC.Generics
import System.Random
import Test.Hspec

import Strat.ZipTree

data NodeVal = NodeVal { nvalToInt :: Int, sign :: Sign }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance ZipTreeNode NodeVal where
  ztnEvaluate = fromIntegral . nvalToInt
  ztnMakeChildren _ = []
  ztnSign = sign
  ztnFinal _ = False

newtype TestPosState = TestPosState {unPosState :: String}

instance PositionState TestPosState where
  toString = unPosState
  combineTwo s _ = s

fakeState :: TestPosState
fakeState = TestPosState {unPosState = "Not implemented."}

--TODO: look into preSort problems -- disabled for now
testEnv :: ZipTreeEnv
testEnv = ZipTreeEnv
        { verbose = False
        , enablePruning = True
        , enablePruneTracing = False
        , singleThreaded = True
        , enableCmpTracing = False
        , enableRandom = False
        , maxRandomChange = 10.0
        , enablePreSort = False
        , moveTraceStr = pack ""
        , maxDepth = 5
        , maxCritDepth = 5
        , aiPlaysWhite = True
        , aiPlaysBlack = True
        }

stratTreeTest :: SpecWith ()
stratTreeTest = do
    describe "negaMax" $
        it "finds the best move from a tree of moves/opponent moves" $ do
            -- let NegaResult{..} = negaMax negaMaxTree False
            -- evalNode best `shouldBe` NodeVal {nvalToInt = 14, sign = Pos }
            (result1, _, _) <- runRWST (negaMax negaMaxTree (Nothing :: Maybe StdGen)) testEnv fakeState
            let NegaResult{..} = result1
            last (nmMovePath picked) `shouldBe` NodeVal {nvalToInt = 14, sign = Pos }
            nmMovePath picked `shouldBe` [ NodeVal {nvalToInt = 2, sign = Pos}
                                         , NodeVal {nvalToInt = -20, sign = Neg}
                                         , NodeVal {nvalToInt = 14, sign = Pos} ]
    describe "negaMax-b" $
        it "same as the previous, but if black moved next" $ do
            -- let NegaResult{..} = negaMax negaMaxTree False
            (result1, _, _) <- runRWST (negaMax negaMaxTree (Nothing :: Maybe StdGen)) testEnv fakeState
            let NegaResult{..} = result1
            last (nmMovePath picked) `shouldBe` NodeVal {nvalToInt = 12, sign = Neg }
            nmMovePath picked `shouldBe` [ NodeVal {nvalToInt = 3, sign = Pos}
                                         , NodeVal {nvalToInt = 35, sign = Neg}
                                         , NodeVal {nvalToInt = 12, sign = Pos} ]

negaMaxTree :: Tree NodeVal
negaMaxTree = Node (NodeVal 0 Neg)
  [ Node (NodeVal 1 Pos)
    [ Node (NodeVal (-5) Neg)
      [ Node (NodeVal 11 Pos) []
      , Node (NodeVal 12 Pos) []
      ]
    , Node (NodeVal (-10) Neg)
      [ Node (NodeVal (-10) Pos) []
      , Node (NodeVal 21 Pos) []
      ]
    , Node (NodeVal (-15) Neg)
      [ Node (NodeVal 18 Pos) []
      , Node (NodeVal 14 Pos) []
      ]
    ]
  , Node (NodeVal 2 Pos)
    [ Node (NodeVal (-20) Neg)
      [ Node (NodeVal 14 Pos) []
      , Node (NodeVal (-8) Pos) []
      ]
    , Node (NodeVal 25 Neg)
      [ Node (NodeVal 20 Pos) []
      , Node (NodeVal 16 Pos) []
      ]
    , Node (NodeVal (-30) Neg)
      [ Node (NodeVal 16 Pos) []
      , Node (NodeVal (-5) Pos) []
      ]
    ]
  , Node (NodeVal 3 Pos)
    [ Node (NodeVal 35 Neg)
      [ Node (NodeVal 12 Pos) []
      , Node (NodeVal 17 Pos) []
      ]
    , Node (NodeVal (-40) Neg)
      [ Node (NodeVal 7 Pos) []
      , Node (NodeVal 19 Pos) []
      ]
    , Node (NodeVal (-45) Neg)
      [ Node (NodeVal 6 Pos) []
      , Node (NodeVal 4 Pos) []
      ]
    ]
  ]
