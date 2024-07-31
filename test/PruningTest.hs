{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module PruningTest (pruningTest) where

import Control.Monad.Reader
import Data.Hashable
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.Text (pack)
import Data.Tree
import GHC.Generics
import System.Random hiding (next)
import Test.Hspec
import Text.Printf
import Strat.ZipTree

data TestNode = TestNode
  { typ :: Int
  , nid :: Int
  , name :: String
  , tnValue :: Float
  , tnSign :: Sign
  , isCrit :: Bool
  }
  deriving (Eq, Generic, Hashable)

instance Show TestNode where
  show tn = printf "TestNode %d (%s), type = %d, sign = %s, val = %s, isCrit = %s"
                   (nid tn) (name tn) (typ tn) (show (tnSign tn)) (show (tnValue tn)) (show (isCrit tn))

instance Ord TestNode where
   (<=) tnx tny = tnValue tnx <= tnValue tny

instance ZipTreeNode TestNode where
  ztnEvaluate = tnValue
  ztnMakeChildren = makeTestChildren
  ztnSign = tnSign
  ztnFinal _ = False
  ztnDeepDescend = isCrit

--TODO: look into preSort problems -- disabled for now
testEnv :: ZipTreeEnv
testEnv = ZipTreeEnv
        { enablePruneTracing = False
        , enableCmpTracing = False
        , enableRandom = False
        , maxRandomChange = 0.0
        , enablePruning = True
        , singleThreaded = True
        , enablePreSort = False
        , moveTraceStr = pack ""
        , maxDepth = 5
        , maxCritDepth = 5
        , aiPlaysWhite = True
        , aiPlaysBlack = True
        }

pruningTest :: SpecWith ()
pruningTest = do
    describe "expandTo" $
        it "expands and modifies a tree of moves, utilizing alpha-beta pruning" $ do
            ----------------------------------------------------------------------------------------
            -- Alpha-Beta pruning example from Wikipedia:
            ----------------------------------------------------------------------------------------
            ----------------------------------------------------------------------------------
            -- testing expansion / evaluation level by level
            ----------------------------------------------------------------------------------
            --------------------------------------------------
            -- depth 1
            --------------------------------------------------
            let f1 :: (HasZipTreeEnv r) => ZipTreeM r (NegaResult TestNode)
                f1 = do
                  wikiTreeD1 <- expandTo rootWikiTree 1 1 1
                  negaMax wikiTreeD1 (Nothing :: Maybe StdGen)
            result1 <- runReaderT f1 testEnv

            let theBest1 = picked result1
            last (nmMovePath theBest1) `shouldBe`
                TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
            nmMovePath theBest1 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              ]

            --------------------------------------------------
            -- depth 2
            --------------------------------------------------
            let f2 :: (HasZipTreeEnv r) => ZipTreeM r (NegaResult TestNode)
                f2 = do
                  wikiTreeD2 <- expandTo rootWikiTree 1 2 2
                  negaMax wikiTreeD2 (Nothing :: Maybe StdGen)
            result2 <- runReaderT f2 testEnv

            let theBest2 = picked result2

            last (nmMovePath theBest2) `shouldBe`
                TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
            nmMovePath theBest2 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              ]

            --------------------------------------------------
            -- depth 3
            --------------------------------------------------
            let f3 :: (HasZipTreeEnv r) => ZipTreeM r (NegaResult TestNode)
                f3 = do
                  wikiTreeD3 <- expandTo rootWikiTree 1 3 3
                  negaMax wikiTreeD3 (Nothing :: Maybe StdGen)
            result3 <- runReaderT f3 testEnv
            let theBest3 = picked result3
            last (nmMovePath theBest3) `shouldBe`
                TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
            nmMovePath theBest3 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
              ]

            ----------------------------------------------------------------------------------
            -- testing the complete tree
            ----------------------------------------------------------------------------------
            newWikiTree <- runReaderT (expandTo rootWikiTree 1 4 4) testEnv

            -- first without pruning
            let noPruneEnv = testEnv {enablePruning = False}
            result4 <- runReaderT (negaMax newWikiTree (Nothing :: Maybe StdGen) ) noPruneEnv
            let theBest4 = picked result4
            last (nmMovePath theBest4) `shouldBe`
                TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}

            nmMovePath theBest4 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              ]

            treeSize newWikiTree `shouldBe` (33, [1, 3, 6, 9, 14])
            evalCount result4 `shouldBe` (33-1) -- 1===root node not counted

            ----------------------------------------------------------------------------------
            -- with pruning
            ----------------------------------------------------------------------------------
            result5 <- runReaderT (negaMax newWikiTree (Nothing :: Maybe StdGen) ) testEnv
            let theBest5 = picked result5
            last (nmMovePath theBest5) `shouldBe`
                TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}

            nmMovePath theBest5 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              ]

            evalCount result5 `shouldBe` (33-8-1) -- 8===nodes skipped from pruning, 1===root node not counted

            ----------------------------------------------------------------------------------
            -- negaRnd, but with the random tolerance at 0.0
            ----------------------------------------------------------------------------------
            rnd <- getStdGen
            result6 <- runReaderT (negaMax newWikiTree (Just rnd) ) testEnv

            let theBest6 = picked result6
            last (nmMovePath theBest6) `shouldBe`
                TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}

            nmMovePath theBest6 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              ]

            evalCount result6 `shouldBe` (33-8-1) -- 8===nodes skipped from pruning, 1===root node not counted

            ----------------------------------------------------------------------------------
            -- with crit processing only for the important nodes at the bottom two levels
            ----------------------------------------------------------------------------------
            newCritWikiTree <- runReaderT (expandTo rootWikiTree 1 2 4) testEnv
            result7 <- runReaderT (negaMax newCritWikiTree (Nothing :: Maybe StdGen) ) testEnv
            let theBest7 = picked result7
            last (nmMovePath theBest7) `shouldBe`
                TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}

            nmMovePath theBest7 `shouldBe`
              [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
              , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
              , TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
              ]

            ----------------------------------------------------------------------------------
            -- with incremental decent and sorting
            ----------------------------------------------------------------------------------
            -- let (newIncWikiTree, result8) = incrementalSearchTo rootWikiTree rnd 0.0 4
            -- (newIncWikiTree, result8) <- runReaderT (incrementalSearchTo rootWikiTree rnd 0.0 4) testEnv

            -- let theBest8 = picked result8
            -- evalNode theBest8 `shouldBe`
            --     TestNode { typ = 1, nid = 27, name = "01-03-07-15-27 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
            -- moveSeq theBest8 `shouldBe`
            --   [ TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False}
            --   , TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
            --   , TestNode { typ = 1, nid = 15, name = "01-03-07-15 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True}
            --   , TestNode { typ = 1, nid = 27, name = "01-03-07-15-27 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True}
            --   ]

            -- treeSize newIncWikiTree `shouldBe` (33, [1, 3, 6, 9, 14])
            -- evalCount result8 `shouldBe` 21 -- reduced count due to sorting

rootWikiTree :: Tree TestNode
rootWikiTree = Node TestNode { typ = 1, nid = 01, name = "01 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = False} []

makeTestChildren :: TestNode -> [Tree TestNode]
makeTestChildren TestNode {..} =
    wikiMap ! nid

-- example from: https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
wikiMap :: HashMap Int [Tree TestNode]
wikiMap = HM.fromList
  [ (01 :: Int, [ Node TestNode { typ = 1, nid = 02, name = "01-02 (3)", tnSign = Neg, tnValue = 3.0 , isCrit = False} []
         , Node TestNode { typ = 1, nid = 03, name = "01-03 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False} []
         , Node TestNode { typ = 1, nid = 04, name = "01-04 (5)", tnSign = Neg, tnValue = 5.0 , isCrit = False} [] ] )
  , (02 :: Int, [ Node TestNode { typ = 1, nid = 05, name = "01-02-05 (5)", tnSign = Pos, tnValue = 5.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 06, name = "01-02-06 (3)", tnSign = Pos , tnValue = 3.0 , isCrit = False} [] ] )
  , (03 :: Int, [ Node TestNode { typ = 1, nid = 07, name = "01-03-07 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 08, name = "01-03-08 (7)", tnSign = Pos , tnValue = 7.0 , isCrit = False} [] ] )
  , (04 :: Int, [ Node TestNode { typ = 1, nid = 09, name = "01-04-09 (5)", tnSign = Pos, tnValue = 5.0 , isCrit = False} []
         , Node TestNode { typ = 1, nid = 10, name = "01-04-10 (8)", tnSign = Pos, tnValue = 8.0 , isCrit = True} [] ] )
  , (05 :: Int, [ Node TestNode { typ = 1, nid = 11, name = "01-02-05-11 (5)", tnSign = Neg, tnValue = 5.0 , isCrit = False} []
         , Node TestNode { typ = 1, nid = 12, name = "01-02-05-12 (4)", tnSign = Neg, tnValue = 4.0 , isCrit = True} [] ] )
  , (06 :: Int, [ Node TestNode { typ = 1, nid = 13, name = "01-02-06-13 (3)", tnSign = Neg, tnValue = 3.0 , isCrit = False} [] ] )
  , (07 :: Int, [ Node TestNode { typ = 1, nid = 14, name = "01-03-07-14 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 15, name = "01-03-07-15 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = True} [] ] )
  , (08 :: Int, [ Node TestNode { typ = 1, nid = 16, name = "01-03-08-16 (7)", tnSign = Neg , tnValue = 7.0 , isCrit = False} [] ] )
  , (09 :: Int, [ Node TestNode { typ = 1, nid = 17, name = "01-04-09-17 (5)", tnSign = Neg, tnValue = 5.0 , isCrit = False} [] ] )
  , (10 :: Int, [ Node TestNode { typ = 1, nid = 18, name = "01-04-10-18 (8)", tnSign = Neg , tnValue = 8.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 19, name = "01-04-10-19 (6)", tnSign = Neg, tnValue = 6.0 , isCrit = False} [] ] )
  , (11 :: Int, [ Node TestNode { typ = 1, nid = 20, name = "01-02-05-11-20 (5)", tnSign = Pos, tnValue = 5.0 , isCrit = False} []
         , Node TestNode { typ = 1, nid = 21, name = "01-02-05-11-21 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = False} [] ] )
  , (12 :: Int, [ Node TestNode { typ = 1, nid = 22, name = "01-02-05-12-22 (7)", tnSign = Pos, tnValue = 7.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 23, name = "01-02-05-12-23 (4)", tnSign = Pos, tnValue = 4.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 24, name = "01-02-05-12-24 (5)", tnSign = Pos, tnValue = 5.0 , isCrit = False} [] ] )
  , (13 :: Int, [ Node TestNode { typ = 1, nid = 25, name = "01-02-06-13-25 (3)", tnSign = Pos, tnValue = 3.0 , isCrit = False} [] ] )
  , (14 :: Int, [ Node TestNode { typ = 1, nid = 26, name = "01-03-07-14-26 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True} [] ] )
  , (15 :: Int, [ Node TestNode { typ = 1, nid = 27, name = "01-03-07-15-27 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 28, name = "01-03-07-15-28 (9)", tnSign = Pos, tnValue = 9.0 , isCrit = True} [] ] )
  , (16 :: Int, [ Node TestNode { typ = 1, nid = 29, name = "01-03-08-16-29 (7)", tnSign = Pos, tnValue = 7.0 , isCrit = False} [] ] )
  , (17 :: Int, [ Node TestNode { typ = 1, nid = 30, name = "01-04-09-17-30 (5)", tnSign = Pos, tnValue = 5.0 , isCrit = False} [] ] )
  , (18 :: Int, [ Node TestNode { typ = 1, nid = 31, name = "01-04-10-18-31 (9)", tnSign = Pos, tnValue = 9.0 , isCrit = True} []
         , Node TestNode { typ = 1, nid = 32, name = "01-04-10-18-32 (8)", tnSign = Pos, tnValue = 8.0 , isCrit = True} [] ] )
  , (19 :: Int, [ Node TestNode { typ = 1, nid = 33, name = "01-04-10-19-33 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = False} [] ] )
  ]
