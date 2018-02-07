{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State    (StateT)
import           Network.AWS

type GrootM = ReaderT Env
type GrootAWS = GrootM AWS
type GrootIO = GrootM IO

awsToGrootM :: (MonadResource m, MonadBaseControl IO m) => AWS a -> GrootM m a
awsToGrootM act = do
  env <- ask
  runAWS env act

awsToGrootM_ :: (MonadResource m, MonadBaseControl IO m) => AWS a -> GrootM m ()
awsToGrootM_ aws = (\_ -> pure ()) =<< awsToGrootM aws

runGroot :: (MonadBaseControl IO m, MonadIO m) => GrootM m a -> Env -> m a
runGroot = runReaderT
{-# INLINE runGroot #-}

mapGrootM :: (m a -> n b) -> GrootM m a -> GrootM n b
mapGrootM = mapReaderT
{-# INLINE mapGrootM #-}

class (MonadBaseControl n m, MonadIO m, MonadCatch m, MonadReader Env m) => MonadGroot n m where
  liftGroot :: GrootM n a -> m a

instance MonadGroot IO GrootIO where
  liftGroot = id

instance MonadGroot n m => MonadGroot n (IdentityT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (MaybeT      m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ReaderT Env m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ResourceT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (StateT    s m) where liftGroot = lift . liftGroot
