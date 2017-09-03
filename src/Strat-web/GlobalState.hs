{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module GlobalState 
    ( AppState(..)
    , gets
    , modify
    , webM
    , WebM(..)
    ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.ST
import Data.Default.Class
import Session

----------------------------------------------------------------------------------------------------
-- Global state for the web app
-- Adapted from the Scotty example: globalstate.hs
----------------------------------------------------------------------------------------------------
--TODO: make this non-specific to checkers again after things are working
data AppState = forall s. AppState { tickCount :: Int, hashTable :: ST s (HT s)}

instance Default AppState where
    def = AppState { tickCount = 0, hashTable = newHT}
 
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
--gets f = ask >>= liftIO . readTVarIO >>= return . f
gets f = fmap f (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f
