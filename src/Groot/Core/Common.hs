{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Monad.Reader
import           Network.AWS

type GrootM  = ReaderT Env
type GrootIO = GrootM IO

-- | Simply a function alias for `runReaderT`
runGroot :: Monad m => GrootM m a -> Env -> m a
runGroot = runReaderT

class Monad m => MonadGroot m where
  getEnv :: m Env
