{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module StratTreeTest (stratTreeTest) where

import Data.Mutable
import Data.Tree
import Test.Hspec

import Strat.StratTree

newtype NodeVal = NodeVal { nvalToInt :: Int }
  deriving (Eq, Ord, Show)

instance Mutable s NodeVal where
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
            let NegaResult{..} = negaMax negaMaxTree (Sign {signToInt = 1})
            branchScore best `shouldBe` NodeVal {nvalToInt = 14}
            moveSeq best `shouldBe` [ NodeVal {nvalToInt = 2}
                                    , NodeVal {nvalToInt = -20}
                                    , NodeVal {nvalToInt = 14} ]
    describe "negaMax-b" $
        it "same as the previous, but if black moved next" $ do
            let NegaResult{..} = negaMax negaMaxTree (Sign {signToInt = -1})
            branchScore best `shouldBe` NodeVal {nvalToInt = 12}
            moveSeq best `shouldBe` [ NodeVal {nvalToInt = 3}
                                    , NodeVal {nvalToInt = 35}
                                    , NodeVal {nvalToInt = 12} ]

negaMaxTree :: Tree NodeVal
negaMaxTree = Node (NodeVal 0)
  [ Node (NodeVal 1)
    [ Node (NodeVal (-5))
      [ Node (NodeVal 11) []
      , Node (NodeVal 12) []
      ]
    , Node (NodeVal (-10))
      [ Node (NodeVal (-10)) []
      , Node (NodeVal 21) []
      ]
    , Node (NodeVal (-15))
      [ Node (NodeVal 18) []
      , Node (NodeVal 14) []
      ]
    ]
  , Node (NodeVal 2)
    [ Node (NodeVal (-20))
      [ Node (NodeVal 14) []
      , Node (NodeVal (-8)) []
      ]
    , Node (NodeVal 25)
      [ Node (NodeVal 20) []
      , Node (NodeVal 16) []
      ]
    , Node (NodeVal (-30))
      [ Node (NodeVal 16) []
      , Node (NodeVal (-5)) []
      ]
    ]
  , Node (NodeVal 3)
    [ Node (NodeVal 35)
      [ Node (NodeVal 12) []
      , Node (NodeVal 17) []
      ]
    , Node (NodeVal (-40))
      [ Node (NodeVal 7) []
      , Node (NodeVal 19) []
      ]
    , Node (NodeVal (-45))
      [ Node (NodeVal 6) []
      , Node (NodeVal 4) []
      ]
    ]
  ]
