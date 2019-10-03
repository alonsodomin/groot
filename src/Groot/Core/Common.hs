{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Groot.Core.Common
     ( GrootT
     , runGrootT
     , mapGrootT
     , Groot
     , runGroot
     , GrootIO
     , GrootIOResource
     , MonadGroot(..)
     , noop
     , awsResource
     , awsResource_
     , useResource
     -- Re-exports
     , Env
     ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State    (StateT)
import           Data.Functor.Identity
import           Network.AWS

newtype GrootT m a = GrootT { unGrootT :: ReaderT Env m a }
  deriving (
      Functor
    , MFunctor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadThrow
    , MonadCatch
    , MonadReader Env
    , MonadResource
    )

instance MonadUnliftIO m => MonadUnliftIO (GrootT m) where
  askUnliftIO = GrootT $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . unGrootT))

runGrootT :: Monad m => GrootT m a -> Env -> m a
runGrootT m = runReaderT (unGrootT m)
{-# INLINE runGroot #-}

mapGrootT :: (m a -> n b) -> GrootT m a -> GrootT n b
mapGrootT f m = GrootT . mapReaderT f $ unGrootT m
{-# INLINE mapGrootT #-}

liftGrootT :: m a -> GrootT m a
liftGrootT m = GrootT . ReaderT $ const m
{-# INLINE liftGrootT #-}

type Groot = GrootT Identity

runGroot :: Groot a -> Env -> a
runGroot m env = runIdentity (runGrootT m env)

type GrootIO = GrootT IO
type GrootIOResource = GrootT (ResourceT IO)

noop :: Applicative m => GrootT m ()
noop = pure ()
{-# INLINE noop #-}

awsResource :: MonadResource m => AWS a -> GrootT m a
awsResource aws = GrootT $ do
  env <- ask
  runAWS env aws

awsResource_ :: MonadResource m => AWS a -> GrootT m ()
awsResource_ aws = void $ awsResource aws
{-# INLINE awsResource_ #-}

useResource :: MonadUnliftIO m => GrootT (ResourceT m) a -> GrootT m a
useResource = hoist runResourceT
{-# INLINE useResource #-}

class (MonadIO m, MonadCatch m, MonadReader Env m) => MonadGroot n m where
  liftGroot :: GrootT n a -> m a

instance MonadGroot IO (GrootT IO) where
  liftGroot = id

instance MonadGroot n m => MonadGroot n (IdentityT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (MaybeT      m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ReaderT Env m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ResourceT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (StateT    s m) where liftGroot = lift . liftGroot
