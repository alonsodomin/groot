{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Network.AWS

type GrootM = ReaderT Env

awsToGrootM :: (MonadResource m, MonadBaseControl IO m) => AWS a -> GrootM m a
awsToGrootM act = do
  env <- ask
  runAWS env act

class (Monad m, MonadIO m, MonadCatch m) => MonadGroot m where
  runGroot :: m a -> Env -> IO a

instance MonadGroot (GrootM IO) where
  runGroot = runReaderT
