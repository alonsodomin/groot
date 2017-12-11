{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Monad.Reader
import           Network.AWS

type GrootM = ReaderT Env

class Monad m => MonadGroot m where
  getEnv :: m Env
