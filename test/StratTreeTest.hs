{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module StratTreeTest (stratTreeTest) where

import Data.Tree
import Test.Hspec

import Strat.ZipTree

data NodeVal = NodeVal { nvalToInt :: Int, sign :: Sign }
  deriving (Eq, Ord, Show)

instance ZipTreeNode NodeVal where
  ztnEvaluate = fromIntegral . nvalToInt
  ztnMakeChildren _ = []
  ztnSign = sign

{-
data NegaMoves a = NegaMoves
  { branchScore :: a
  , moveSeq :: [a] }
  deriving (Show, Eq)

toNegaMoves :: TraceCmp a -> NegaMoves a
toNegaMoves (TraceCmp (x, xs)) =
    NegaMoves { branchScore = x
              , moveSeq = xs }

data NegaResult a = NegaResult
  { best :: NegaMoves a
  , alternatives :: [NegaMoves a] }
  deriving (Show, Eq)
-}

stratTreeTest :: SpecWith ()
stratTreeTest = do
    describe "negaMax" $
        it "finds the best move from a tree of moves/opponent moves" $ do
            -- let ((x, xs), _others) = negaMax negaMaxTree (Sign {signToInt = 1})
            -- x `shouldBe` NodeVal {nvalToInt = 14}
            -- xs `shouldBe` [ NodeVal {nvalToInt = 2}
            --               , NodeVal {nvalToInt = -20}
            --               , NodeVal {nvalToInt = 14} ]
            let NegaResult{..} = negaMax negaMaxTree False
            branchScore best `shouldBe` NodeVal {nvalToInt = 14, sign = Pos }
            moveSeq best `shouldBe` [ NodeVal {nvalToInt = 2, sign = Pos}
                                    , NodeVal {nvalToInt = -20, sign = Neg}
                                    , NodeVal {nvalToInt = 14, sign = Pos} ]
    describe "negaMax-b" $
        it "same as the previous, but if black moved next" $ do
            let NegaResult{..} = negaMax negaMaxTree False
            branchScore best `shouldBe` NodeVal {nvalToInt = 12, sign = Neg }
            moveSeq best `shouldBe` [ NodeVal {nvalToInt = 3, sign = Pos}
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
