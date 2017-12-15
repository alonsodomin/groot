{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Resource
import           Network.AWS

type GrootM = ReaderT Env
type GrootAWS = GrootM AWS

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

class (MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadReader Env m) => MonadGroot m where
  liftGroot :: GrootM IO a -> m a

instance MonadGroot (GrootM IO) where
  liftGroot = id

instance MonadGroot m => MonadGroot (IdentityT   m) where liftGroot = lift . liftGroot
instance MonadGroot m => MonadGroot (MaybeT      m) where liftGroot = lift . liftGroot
instance MonadGroot m => MonadGroot (ReaderT Env m) where liftGroot = lift . liftGroot
instance MonadGroot m => MonadGroot (ResourceT   m) where liftGroot = lift . liftGroot
