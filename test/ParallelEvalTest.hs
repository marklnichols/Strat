{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module ParallelEvalTest where

import Control.DeepSeq
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Tree
import GHC.Generics
import System.Time.Extra (duration, showDuration)
import Text.Printf
import Debug.Trace

import Strat.ZipTree

data TestNode = TestNode
  { typ :: Int
  , nid :: Int
  , name :: String
  , tnValue :: Float
  , tnSign :: Sign
  , isCrit :: Bool
  }
  deriving (Generic, Eq, NFData)

-- instance NFData TestNode where
  -- rnf tn =


instance Show TestNode where
  show tn = printf "TestNode %d (%s), type = %d, sign = %s, val = %s, isCrit = %s"
                   (nid tn) (name tn) (typ tn) (show (tnSign tn)) (show (tnValue tn)) (show (isCrit tn))

instance Ord TestNode where
   (<=) tnx tny = tnValue tnx <= tnValue tny

instance ZipTreeNode TestNode where
  ztnEvaluate = tnValue
  ztnMakeChildren = makeTestChildren
  ztnSign = tnSign
  ztnDeepDecend = isCrit

main :: IO ()
main = do
    (sec, _) <- duration $ do
        let newWikiTree = expandTo rootWikiTree 4 4
        let res = negaMax newWikiTree False
        putStrLn $ showResultsBrief res
        return ()
    putStrLn $ "Time for single threaded tree expansion: " ++ showDuration sec ++ "):"

showResultsBrief :: NegaResult TestNode -> String
showResultsBrief res =
  let bst = best res
  in printf "score: %f, sequence: %s" (evalScore bst) (show (moveSeq bst))

rootWikiTree :: Tree TestNode
rootWikiTree = Node TestNode { typ = 1, nid = 01, name = "01 (6)", tnSign = Pos, tnValue = 6.0 , isCrit = False} []

makeTestChildren :: TestNode -> [Tree TestNode]
makeTestChildren TestNode {..} =
  let primeCount = 10000 * nid
      largePrimes = take primeCount primes
      !largest = last largePrimes
      str = printf "nid: %d - %dth prime is %d" nid (nid * 100) largest
  in trace str (wikiMap ! nid)

----------------------------------------------------------------------------------------------------
-- Prime number generation to test parallel calculations
----------------------------------------------------------------------------------------------------

-- | Infinite lazy list of prime numbers.
primes :: [Integer]
primes = 2 : 3 : filter (\n -> length (primeFactors n) == 1) [5, 7 ..]

-- | Returns the list of prime factors of an Integer
--   Note that 1 is not included in the list.  For any prime p, the list [p] is returned.
primeFactors :: Integer -> [Integer]
primeFactors num = unfoldr f (testFactors num, num) where
    f (_, 1) = Nothing
    f (ps, n) = case find (\p -> (n `rem` p) == 0) ps of
                        Nothing -> Just (n, ([], 1)) -- prime
                        Just fact -> Just (fact, (dropWhile (< fact) ps, n`div` fact))

-- | List of primes needed to test as factors for a new candidate prime
testFactors :: Integer -> [Integer]
testFactors n = takeWhile ((<=n) . square) primes

-- avoiding some num conversion ugliness with (^2)
square :: Integer -> Integer
square x = x * x
----------------------------------------------------------------------------------------------------

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
