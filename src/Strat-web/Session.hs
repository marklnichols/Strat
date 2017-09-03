{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Session 
    ( get
    , HT
    , newHT
    , set
    ) where

import Prelude hiding (lookup)
import Control.Monad.ST
import Data.HashTable.ST.Basic
import Data.Tree
import qualified Data.Text.Lazy as T
import qualified Checkers as CK


--TODO: make this non-checkers specific

-- Hashtable parameterized by ST "thread"
type HT s = HashTable s T.Text (Tree CK.CkNode) 

newHT :: ST s (HT s)
--newHT = do
--  ht <- new
--  return ht
newHT = new  

set :: HT s -> T.Text -> Tree CK.CkNode -> ST s (HT s)
set ht key value = do
    insert ht key value
    return ht
  
get :: HT s -> T.Text -> ST s (Maybe (Tree CK.CkNode))
--get ht key = do
--  val <- lookup ht key
--  return val
get = lookup  


