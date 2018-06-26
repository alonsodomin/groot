{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common
     ( GrootT(..)
     , GrootIO
     , GrootResource
     , MonadGroot(..)
     , mapGrootT
     , awsResource
     , awsResource_
     , runGrootResource
     , runGroot
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
import           Network.AWS

newtype GrootT m a = GrootT { runGrootT :: ReaderT Env m a }

type GrootIO = GrootT IO
type GrootResource = GrootT (ResourceT IO)

liftGrootT :: m a -> GrootT m a
liftGrootT m = GrootT . ReaderT $ const m
{-# INLINE liftGrootT #-}

mapGrootT :: (m a -> n b) -> GrootT m a -> GrootT n b
mapGrootT f m = GrootT . mapReaderT f $ runGrootT m
{-# INLINE mapGrootT #-}

instance Functor m => Functor (GrootT m) where
  fmap f = mapGrootT (fmap f)

instance MFunctor GrootT where
  hoist f m = GrootT . hoist f $ runGrootT m

instance Applicative m => Applicative (GrootT m) where
  pure = liftGrootT . pure
  (GrootT x) <*> (GrootT y) = GrootT $ x <*> y

instance Alternative m => Alternative (GrootT m) where
  empty = liftGrootT empty
  m <|> n = GrootT $ (runGrootT m) <|> (runGrootT n)

instance Monad m => Monad (GrootT m) where
  return = lift . return
  m >>= f = GrootT $ do
    a <- runGrootT m
    runGrootT $ f a

instance MonadTrans GrootT where
  lift = liftGrootT

instance MonadIO m => MonadIO (GrootT m) where
  liftIO = lift . liftIO

instance MonadUnliftIO m => MonadUnliftIO (GrootT m) where
  askUnliftIO = GrootT $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runGrootT))

instance MonadThrow m => MonadThrow (GrootT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (GrootT m) where
  catch m f = GrootT $ catch (runGrootT m) (\x -> runGrootT $ f x)

instance Monad m => MonadReader Env (GrootT m) where
  ask = GrootT $ ask
  local f m = GrootT $ local f (runGrootT m)
  reader = GrootT . reader

instance MonadResource m => MonadResource (GrootT m) where
  liftResourceT = lift . liftResourceT

instance MonadAWS m => MonadAWS (GrootT m) where
  liftAWS = lift . liftAWS

awsResource :: MonadResource m => AWS a -> GrootT m a
awsResource aws = GrootT $ do
  env <- ask
  runAWS env aws
{-# INLINE awsResource #-}

awsResource_ :: MonadResource m => AWS a -> GrootT m ()
awsResource_ aws = void $ awsResource aws
{-# INLINE awsResource_ #-}

runGrootResource :: GrootResource a -> GrootIO a
runGrootResource = hoist runResourceT
{-# INLINE runGrootResource #-}

runGroot :: Monad m => GrootT m a -> Env -> m a
runGroot m = runReaderT (runGrootT m)
{-# INLINE runGroot #-}

class (MonadIO m, MonadCatch m, MonadReader Env m) => MonadGroot n m where
  liftGroot :: GrootT n a -> m a

instance MonadGroot IO GrootIO where
  liftGroot = id

instance MonadGroot n m => MonadGroot n (IdentityT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (MaybeT      m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ReaderT Env m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ResourceT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (StateT    s m) where liftGroot = lift . liftGroot
