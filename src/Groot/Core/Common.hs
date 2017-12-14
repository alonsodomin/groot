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

awsToGrootM :: (MonadResource m, MonadBaseControl IO m) => AWS a -> GrootM m a
awsToGrootM act = do
  env <- ask
  runAWS env act

runGroot :: (MonadBaseControl IO m, MonadIO m) => GrootM m a -> Env -> m a
runGroot = runReaderT
{-# INLINE runGroot #-}

class (MonadBaseControl IO m, MonadIO m, MonadCatch m) => MonadGroot m where
  liftGroot :: GrootM IO a -> m a

instance MonadGroot (GrootM IO) where
  liftGroot = id

instance MonadGroot m => MonadGroot (IdentityT m) where liftGroot = lift . liftGroot
instance MonadGroot m => MonadGroot (MaybeT    m) where liftGroot = lift . liftGroot
instance MonadGroot m => MonadGroot (ReaderT r m) where liftGroot = lift . liftGroot
