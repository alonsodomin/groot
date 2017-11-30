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
import           Control.Monad.IO.Class
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
import           Groot.Core.Console
import           Groot.Data.Conduit.STM
import           Groot.Data.Filter
import           Groot.Exception
import           Groot.Types

formatEventTime :: MonadIO m => UTCTime -> m String
formatEventTime time = do
  dt <- liftIO $ utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%d/%m/%Y %T" dt

pollServiceEvents :: Env
                  -> ContainerServiceCoords
                  -> Bool
                  -> TBMChan ECS.ServiceEvent
                  -> (TBMChan ECS.ServiceEvent -> STM ())
                  -> IO ()
pollServiceEvents env (ContainerServiceCoords serviceRef clusterRef) inf chan onComplete =
  liftIO $ evalStateT loop Nothing
  where loop :: MonadIO m => StateT (Maybe UTCTime) m ()
        loop = do
          lastEventTime <- get
          events <- liftIO . runResourceT . runAWS env $ serviceEvents lastEventTime
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

serviceEventLog :: (MonadResource mi, MonadBaseControl IO mi, MonadIO mo)
                => Env
                -> [ContainerServiceCoords]
                -> Bool
                -> mi (Source mo ECS.ServiceEvent)
serviceEventLog env coords inf = do
  chan     <- liftIO . atomically $ newTBMChan 500
  refCount <- liftIO . atomically . newTVar $ length coords
  regs     <- forM coords $ \sc -> Lifted.mask_ $ do
    register . killThread <=< runResourceT $ resourceForkIO $ liftIO . publishInto chan sc $ decRefCount refCount
  return $ chanSource chan readTBMChan (\ch -> do liftIO . atomically $ closeTBMChan ch
                                                  mapM_ release regs)

  where
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

    publishInto :: TBMChan ECS.ServiceEvent
                -> ContainerServiceCoords
                -> (TBMChan ECS.ServiceEvent -> STM ())
                -> IO ()
    publishInto chan crds onComplete =
      ignoreServicesBecomingInactive $ pollServiceEvents env crds inf chan onComplete

clusterServiceEventLog :: (MonadResource mi, MonadBaseControl IO mi, MonadIO mo)
                       => Env
                       -> [ClusterRef]
                       -> Bool
                       -> mi (Source mo ECS.ServiceEvent)
clusterServiceEventLog env clusterRefs inf = do
  coords <- runResourceT . runAWS env $ allServiceCoords
  serviceEventLog env coords inf
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
