{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module GlobalState where

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

{-
    TRY -- can I write a fn :: AppState -> ST s (HT s) 
      -- using the pattern matching syntax required?
      -- (and then use gets??)
      fn :: AppState -> ST s (HT s)
      fn {hashTable = ht} = ht

--separate function for this due to existential quantifier forall s. in AppState
getAppStateHT :: WebM ST s (HT s)
getAppStateHT = do
              -- WebM ST s (HT s) ::: WebM (ReaderT (TVar AppState) IO (ST s (HT s))  

    --gets f = ask >>= liftIO . readTVarIO >>= return . f
    
    ask -- ::  MonadReader r m -> m r
    ask -- :: ReaderT (TVar AppState) IO -> IO (TVar AppState)        

    liftIO -- :: IO a -> m a
    liftIO -- :: IO (ST s HT s)) -> ReaderT (TVar Appstate) IO (ST s (HT s))
        
    readTVarIO  -- readTVarIO :: TVar a -> IO a
    readTVarIO  -- TVar (ST s (HT s)) -> IO (ST s (HT s))
    
    return  -- (ST s (HT s)) -> WebM (ReaderT (TVar AppState) IO (ST s (HT s)) ??
    
    
    liftIO . readTVarIO -- :: TVar a -> m a
    liftIO . readTVarIO -- :: TVar (ST s (HT s)) -> ReaderT (TVar Appstate) IO (ST s (HT s))
    
    ask >>= liftIO . readTVarIO -- :: MonadReader r m -> m r >>= TVar a -> m a
                                -- ReaderT (TVar AppState) IO -> IO (TVar AppState)
                                --     >>=  (ReaderT (TVar AppState) IO -> IO (TVar AppState))
-}

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f
