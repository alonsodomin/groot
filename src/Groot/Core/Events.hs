{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Core.Events
       ( serviceEventLog
       , clusterServiceEventLog
       , printEvent
       , printEventSink
       ) where

import           Control.Applicative
import           Control.Concurrent             (killThread)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Delay
import           Control.Concurrent.STM.TBMChan
import qualified Control.Exception.Lifted       as Lifted
import           Control.Lens                   hiding (argument)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Lazy
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.Maybe                     (listToMaybe)
import qualified Data.Text                      as T
import           Data.Time
import           Network.AWS                    hiding (await)
import qualified Network.AWS.ECS                as ECS

import           Groot.AWS.Service
import           Groot.Core.Common
import           Groot.Core.Console
import           Groot.Data.Conduit.STM
import           Groot.Data.Filter
import           Groot.Exception
import           Groot.Types

formatEventTime :: MonadIO m => UTCTime -> m String
formatEventTime time = do
  dt <- liftIO $ utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%d/%m/%Y %T" dt

pollServiceEvents :: (MonadReader e m, MonadBaseControl IO m, MonadIO m, HasEnv e)
                  => ContainerServiceCoords
                  -> Bool
                  -> TBMChan ECS.ServiceEvent
                  -> (TBMChan ECS.ServiceEvent -> STM ())
                  -> m ()
pollServiceEvents (ContainerServiceCoords serviceRef clusterRef) inf chan onComplete =
  evalStateT loop Nothing
  where loop :: (MonadReader e m, MonadIO m, HasEnv e) => StateT (Maybe UTCTime) m ()
        loop = do
          env           <- lift $ ask
          lastEventTime <- get
          events        <- liftIO . runResourceT . runAWS env $ serviceEvents lastEventTime
          liftIO $ forM_ (reverse events) $ atomically . tryWriteTBMChan chan
          if inf then do
            delay <- liftIO $ newDelay 2000000
            let nextTime = listToMaybe events >>= view ECS.seCreatedAt
            liftIO . atomically $ waitDelay delay
            put $ nextTime <|> lastEventTime
            loop
          else liftIO . atomically $ onComplete chan

        serviceEvents :: MonadAWS m => Maybe UTCTime -> m [ECS.ServiceEvent]
        serviceEvents lastEventTime = do
          service  <- getService serviceRef (Just clusterRef)
          service' <- if (matches isActiveContainerService service)
                      then return service
                      else throwM $ inactiveService serviceRef clusterRef
          events  <- return $ service' ^. ECS.csEvents
          return $ case lastEventTime of
            Nothing -> take 25 events
            Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. ECS.seCreatedAt) events

serviceEventLog :: (MonadResource mi, MonadBaseControl IO mi, MonadCatch mi, MonadReader e mi, MonadIO mo, HasEnv e)
                => [ContainerServiceCoords]
                -> Bool
                -> mi (Source mo ECS.ServiceEvent)
serviceEventLog coords inf = do
  chan     <- liftIO . atomically $ newTBMChan 500
  refCount <- liftIO . atomically . newTVar $ length coords
  regs     <- forM coords (forkEventStream chan refCount)
  return $ chanSource chan readTBMChan (\ch -> do liftIO . atomically $ closeTBMChan ch
                                                  mapM_ release regs)

  where
    forkEventStream :: (MonadResource m, MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadReader e m, HasEnv e)
                    => TBMChan ECS.ServiceEvent
                    -> TVar Int
                    -> ContainerServiceCoords
                    -> m ReleaseKey
    forkEventStream chan refCount serviceCoord = Lifted.mask_ $ do
      threadId <- runResourceT $ resourceForkIO $ lift $ publishInto chan serviceCoord $ decRefCount refCount
      register . killThread $ threadId

    modifyTVar'' :: TVar a -> (a -> a) -> STM a
    modifyTVar'' tvar f = do
      x <- f <$> readTVar tvar
      writeTVar tvar x
      return x

    decRefCount :: TVar Int -> TBMChan a -> STM ()
    decRefCount tvar chan = do
      n <- modifyTVar'' tvar (subtract 1)
      when (n == 0) $ closeTBMChan chan

    ignoreServicesBecomingInactive :: MonadCatch m => m () -> m ()
    ignoreServicesBecomingInactive action =
      catching _InactiveService action $ \_ -> return ()

    publishInto :: (MonadReader e m, MonadCatch m, MonadBaseControl IO m, MonadIO m, HasEnv e)
                => TBMChan ECS.ServiceEvent
                -> ContainerServiceCoords
                -> (TBMChan ECS.ServiceEvent -> STM ())
                -> m ()
    publishInto chan crds onComplete =
      ignoreServicesBecomingInactive $ pollServiceEvents crds inf chan onComplete

clusterServiceEventLog :: (MonadResource mi, MonadBaseControl IO mi, MonadCatch mi, MonadIO mo)
                       => [ClusterRef]
                       -> Bool
                       -> GrootM mi (Source mo ECS.ServiceEvent)
clusterServiceEventLog clusterRefs inf = do
  env    <- ask
  coords <- runResourceT . runAWS env $ allServiceCoords
  serviceEventLog coords inf
  where clusterServiceCoords :: MonadAWS m => ClusterRef -> m [ContainerServiceCoords]
        clusterServiceCoords cref = sourceToList $ fetchServices cref
          =$= filterC isActiveContainerService
          =$= CL.mapMaybe serviceCoords

        allServiceCoords :: MonadAWS m => m [ContainerServiceCoords]
        allServiceCoords = do
          coords <- concat <$> mapM clusterServiceCoords clusterRefs
          return coords

printEvent :: MonadIO m => ECS.ServiceEvent -> m ()
printEvent event = do
  eventTime <- maybe (return "") formatEventTime $ event ^. ECS.seCreatedAt
  liftIO $ do
    runResourceT $ withSGR yellowText $ putStr eventTime
    putStrLn $ ' ':(maybe "" T.unpack $ event ^. ECS.seMessage)

printEventSink :: MonadIO m => Sink ECS.ServiceEvent m ()
printEventSink = do
  mevent <- await
  case mevent of
    Just event -> do
      printEvent event
      printEventSink
    Nothing -> return ()
