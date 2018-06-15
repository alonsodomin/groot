{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Common where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State    (StateT)
import           Network.AWS

newtype GrootT m a = GrootT { runGrootT :: ReaderT Env m a }
type GrootM = GrootT
{-# DEPRECATED GrootM "Use GrootT instead" #-}

type GrootAWS = GrootT AWS
type GrootIO = GrootT IO

liftGrootT :: m a -> GrootT m a
liftGrootT m = GrootT . ReaderT $ const m
{-# INLINE liftGrootT #-}

mapGrootT :: (m a -> n b) -> GrootT m a -> GrootT n b
mapGrootT f m = GrootT $ mapReaderT f $ runGrootT m
{-# INLINE mapGrootT #-}

mapGrootM :: (m a -> n b) -> GrootT m a -> GrootT n b
mapGrootM = mapGrootT
{-# INLINE mapGrootM #-}
{-# DEPRECATED mapGrootM "Use mapGrootT instead" #-}

instance Functor m => Functor (GrootT m) where
  fmap f = mapGrootT (fmap f)

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

awsToGrootT :: MonadResource m => AWS a -> GrootT m a
awsToGrootT act = GrootT $ do
  env <- ask
  runAWS env act

awsToGrootM :: MonadResource m => AWS a -> GrootT m a
awsToGrootM = awsToGrootT
{-# INLINE awsToGrootM #-}
{-# DEPRECATED awsToGrootM "Use awsToGrootM instead" #-}

awsToGrootT_ :: MonadResource m => AWS a -> GrootT m ()
awsToGrootT_ aws = (\_ -> pure ()) =<< awsToGrootT aws

awsToGrootM_ :: MonadResource m => AWS a -> GrootT m ()
awsToGrootM_ = awsToGrootT_
{-# INLINE awsToGrootM_ #-}
{-# DEPRECATED awsToGrootM_ "Use awsToGrootT_ instead" #-}

evalGrootT :: Monad m => GrootT m a -> Env -> m a
evalGrootT m = runReaderT (runGrootT m)
{-# INLINE evalGrootT #-}

runGroot :: Monad m => GrootT m a -> Env -> m a
runGroot = evalGrootT
{-# INLINE runGroot #-}
{-# DEPRECATED runGroot "Use evalGrootT instead" #-}

class (MonadIO m, MonadCatch m, MonadReader Env m) => MonadGroot n m where
  liftGroot :: GrootT n a -> m a

instance MonadGroot IO GrootIO where
  liftGroot = id

instance MonadGroot n m => MonadGroot n (IdentityT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (MaybeT      m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ReaderT Env m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (ResourceT   m) where liftGroot = lift . liftGroot
instance MonadGroot n m => MonadGroot n (StateT    s m) where liftGroot = lift . liftGroot
