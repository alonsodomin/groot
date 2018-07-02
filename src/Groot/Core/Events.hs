{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.Core.Events
       ( serviceEventLog
       , clusterServiceEventLog
       , printEventSink
       ) where

import           Control.Applicative
import           Control.Concurrent             (killThread)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Delay
import           Control.Concurrent.STM.TBMChan
import           Control.Exception.Lens
import           Control.Lens                   hiding (argument)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Lazy
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.Conduit.TMChan
import           Data.Maybe                     (listToMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time
import           Data.Typeable
import           Network.AWS                    hiding (await)
import qualified Network.AWS.ECS                as ECS
import qualified UnliftIO                       as Lifted

import           Groot.Internal.AWS
import           Groot.Console
import           Groot.Data.Filter
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

formatEventTime :: MonadIO m => UTCTime -> m Text
formatEventTime time = do
  dt <- liftIO $ utcToLocalZonedTime time
  return . T.pack $ formatTime defaultTimeLocale "%d/%m/%Y %T" dt

pollServiceEvents :: (MonadReader e m, MonadUnliftIO m, MonadIO m, HasEnv e)
                  => ContainerServiceCoords
                  -> Bool
                  -> Int
                  -> TBMChan ECS.ServiceEvent
                  -> m ()
                  -> m ()
pollServiceEvents (ContainerServiceCoords serviceRef clusterRef) forvr lastN chan onComplete =
  evalStateT loop Nothing
  where loop = do
          env           <- lift $ ask
          lastEventTime <- get
          events        <- liftIO . runResourceT . runAWS env $ serviceEvents lastEventTime
          liftIO $ forM_ (reverse events) $ atomically . tryWriteTBMChan chan
          if forvr then do
            delay <- liftIO $ newDelay 2000000
            let nextTime = listToMaybe events >>= view ECS.seCreatedAt
            liftIO . atomically $ waitDelay delay
            put $ nextTime <|> lastEventTime
            loop
          else lift $ onComplete

        serviceEvents :: MonadAWS m => Maybe UTCTime -> m [ECS.ServiceEvent]
        serviceEvents lastEventTime = do
          service  <- getService serviceRef (Just clusterRef)
          service' <- if (matches isActiveContainerService service)
                      then return service
                      else throwM $ inactiveService serviceRef clusterRef
          events  <- return $ service' ^. ECS.csEvents
          return $ case lastEventTime of
            Nothing -> take lastN events
            Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. ECS.seCreatedAt) events

serviceEventLog :: Typeable mi
                => MonadResource mi
                => MonadUnliftIO mi
                => MonadCatch mi
                => MonadReader e mi
                => MonadConsole mi
                => MonadIO mo
                => HasEnv e
                => [ContainerServiceCoords]
                -> Bool
                -> Int
                -> mi (ConduitT () ECS.ServiceEvent mo ())
serviceEventLog coords inf lastN = do
  chan     <- createChannel
  refCount <- liftIO . atomically . newTVar $ length coords
  forM_ coords (forkEventStream chan refCount)
  return $ sourceTBMChan chan

  where
    createChannel = liftIO . atomically $ newTBMChan 500

    forkEventStream chan refCount serviceCoord = Lifted.mask_ $ do
      threadId <- runResourceT $ resourceForkIO $ lift $ publishInto chan serviceCoord $ decRefCount refCount chan
      register $ killThread threadId

    modifyTVar'' :: TVar a -> (a -> a) -> STM a
    modifyTVar'' tvar f = do
      x <- f <$> readTVar tvar
      writeTVar tvar x
      return x

    decRefCount :: MonadIO m => TVar Int -> TBMChan a -> m ()
    decRefCount tvar chan = liftIO . atomically $ do
      n <- modifyTVar'' tvar (subtract 1)
      when (n == 0) $ closeTBMChan chan

    handleStreamErrors :: (Typeable m, MonadCatch m, MonadConsole m) => m () -> m ()
    handleStreamErrors action = catches action [
        handler _InactiveService (\_ -> pure ())
      , handler _TransportError  handleHttpException
      , handler _ServiceError    handleServiceError
      ]

    publishInto :: (Typeable m, MonadReader e m, MonadCatch m, MonadUnliftIO m, MonadIO m, MonadConsole m, HasEnv e)
                => TBMChan ECS.ServiceEvent
                -> ContainerServiceCoords
                -> m ()
                -> m ()
    publishInto chan crds onComplete =
      handleStreamErrors $ pollServiceEvents crds inf lastN chan onComplete

clusterServiceEventLog :: Typeable mi
                       => MonadResource mi
                       => MonadUnliftIO mi
                       => MonadCatch mi
                       => MonadIO mo
                       => MonadConsole mi
                       => MonadReader e mi
                       => HasEnv e
                       => [ClusterRef]
                       -> Bool
                       -> Int
                       -> mi (ConduitT () ECS.ServiceEvent mo ())
clusterServiceEventLog clusterRefs inf lastN = do
  env    <- ask
  coords <- runResourceT . runAWS env $ allServiceCoords
  serviceEventLog coords inf lastN
  where clusterServiceCoords :: MonadAWS m => ClusterRef -> m [ContainerServiceCoords]
        clusterServiceCoords cref = sourceToList $ fetchServices cref
          .| filterC isActiveContainerService
          .| CL.mapMaybe serviceCoords

        allServiceCoords :: MonadAWS m => m [ContainerServiceCoords]
        allServiceCoords = do
          coords <- concat <$> mapM clusterServiceCoords clusterRefs
          return coords

printEvent :: MonadIO m => ECS.ServiceEvent -> m ()
printEvent event = do
  eventTime <- maybe (return "") formatEventTime $ event ^. ECS.seCreatedAt
  displayLn $ (styled yellowStyle eventTime) <+> (styleless $ maybe "" id $ event ^. ECS.seMessage)

printEventSink :: MonadIO m => ConduitT ECS.ServiceEvent Void m ()
printEventSink = do
  mevent <- await
  case mevent of
    Just event -> do
      printEvent event
      printEventSink
    Nothing -> return ()
