{-# LANGUAGE FlexibleContexts #-}

module Groot.AWS.Service
     (
       serviceCoords
     , findServices
     , findService
     , findServiceCoords
     , getService
     , fetchServices
     , fetchAllServices
     , serviceEventLog
     , clusterServiceEventLog
     ) where

import Control.Applicative
import qualified Control.Concurrent as TH
import Control.Concurrent.STM.Delay
import Control.Concurrent.STM.TBMChan
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Resource
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CH
import Data.Foldable
import Data.Maybe (listToMaybe)
import Data.Time
import Groot.AWS.Cluster
import Groot.Data
import Groot.Exception
import Network.AWS hiding (await)
import qualified Network.AWS.ECS as ECS
import Network.AWS.Data.Text

serviceCoords :: ECS.ContainerService -> Maybe ServiceCoords
serviceCoords service = ServiceCoords <$> serviceRef <*> clusterRef
  where serviceRef = ServiceRef <$> service ^. ECS.csServiceARN
        clusterRef = ClusterRef <$> service ^. ECS.csClusterARN

findServiceCoords :: (MonadAWS m, Traversable t) => t ServiceRef -> m (t ServiceCoords)
findServiceCoords = traverse singleServiceCoords
  where singleServiceCoords :: MonadAWS m => ServiceRef -> m ServiceCoords
        singleServiceCoords serviceRef = do
          mcoords <- (serviceCoords <$> getService serviceRef Nothing)
          case mcoords of
            Just c  -> return c
            Nothing -> throwM $ serviceNotFound serviceRef Nothing

fetchServiceBatch :: MonadAWS m => [ServiceRef] -> ClusterRef -> m [ECS.ContainerService]
fetchServiceBatch []       _    = return []
fetchServiceBatch services cref =
  let fetch = do
        res <- send $ ECS.dCluster ?~ (toText cref)
               $ ECS.dServices .~ (toText <$> services)
               $ ECS.describeServices
        return $ res ^. ECS.dssrsServices
  in handleClusterNotFoundException cref fetch

fetchServices :: MonadAWS m => ClusterRef -> Source m ECS.ContainerService
fetchServices clusterRef =
  handleClusterNotFoundException clusterRef (paginate (ECS.lsCluster ?~ (toText clusterRef) $ ECS.listServices))
    =$= CL.concatMapM (\x -> fetchServiceBatch (ServiceRef <$> x ^. ECS.lsrsServiceARNs) clusterRef)

fetchAllServices :: MonadAWS m => Source m ECS.ContainerService
fetchAllServices = fetchClusters
  =$= CL.mapMaybe clusterName
  =$= fetchAllServicesC

fetchServicesC' :: MonadAWS m => [ServiceRef] -> Conduit ClusterRef m (ClusterRef, ECS.ContainerService)
fetchServicesC' services =
  awaitForever (\cref -> yieldM $ do
    servs <- fetchServiceBatch services cref
    return $ map ((,) cref) servs
  ) =$= CL.concat

fetchServicesC :: MonadAWS m => [ServiceRef] -> Conduit ClusterRef m ECS.ContainerService
fetchServicesC services = fetchServicesC' services =$= CL.map snd

fetchAllServicesC :: MonadAWS m => Conduit ClusterRef m ECS.ContainerService
fetchAllServicesC = awaitForever (\cref -> yieldM . sourceToList $ fetchServices cref) =$= CL.concat

findServices' :: MonadAWS m => [ServiceRef] -> Maybe ClusterRef -> Source m (ClusterRef, ECS.ContainerService)
findServices' services (Just clusterRef) =
  yield clusterRef =$= fetchServicesC' services
findServices' services _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchServicesC' services

findServices :: MonadAWS m => [ServiceRef] -> Maybe ClusterRef -> Source m ECS.ContainerService
findServices services cref = findServices' services cref =$= CL.map snd

findService :: MonadAWS m => ServiceRef -> Maybe ClusterRef -> MaybeT m ECS.ContainerService
findService sref cref = MaybeT $ extractRequested cref
  where extractRequested (Just _) =
          runConduit $ findServices' [sref] cref =$= CL.map snd =$= CL.head
        extractRequested _ = do
          found <- sourceToList $ findServices' [sref] Nothing
                   =$= filterOnC snd (ServiceRefFilter sref)
          if (length found) > 1
          then throwM $ ambiguousServiceName sref (fst <$> found)
          else return $ snd <$> listToMaybe found

getService :: MonadAWS m => ServiceRef -> Maybe ClusterRef -> m ECS.ContainerService
getService serviceName clusterRef = do
  serv <- runMaybeT $ findService serviceName clusterRef
  case serv of
    Just x  -> return x
    Nothing -> throwM $ serviceNotFound serviceName clusterRef

pollServiceEvents :: Env
                  -> ServiceCoords
                  -> Bool
                  -> TBMChan ECS.ServiceEvent
                  -> IO ()
pollServiceEvents env (ServiceCoords serviceRef clusterRef) inf chan = do
  TH.forkIO $ evalStateT loop Nothing
  return ()
    where
      loop :: StateT (Maybe UTCTime) IO ()
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
        else return ()

      serviceEvents :: MonadAWS m => Maybe UTCTime -> m [ECS.ServiceEvent]
      serviceEvents lastEventTime = do
        service  <- getService serviceRef (Just clusterRef)
        service' <- if (matches isActiveService service)
                    then return service
                    else throwM $ inactiveService serviceRef clusterRef
        events  <- return $ service' ^. ECS.csEvents
        return $ case lastEventTime of
          Nothing -> take 25 events
          Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. ECS.seCreatedAt) events

serviceEventLog :: Env
                -> [ServiceCoords]
                -> Bool
                -> IO (Source IO ECS.ServiceEvent)
serviceEventLog env coords inf = do
  chan <- newTBMChanIO 500
  forM_ coords $ eventSource chan
  return $ CH.sourceTBMChan chan
    where
      ignoreServicesBecomingInactive :: MonadCatch m => m () -> m ()
      ignoreServicesBecomingInactive action =
        catching _InactiveService action $ \_ -> return ()

      eventSource :: TBMChan ECS.ServiceEvent -> ServiceCoords -> IO ()
      eventSource chan crds =
        ignoreServicesBecomingInactive $ pollServiceEvents env crds inf chan

clusterServiceEventLog :: Env
                       -> [ClusterRef]
                       -> Bool
                       -> IO (Source IO ECS.ServiceEvent)
clusterServiceEventLog env clusterRefs inf = do
  coords <- runResourceT . runAWS env $ allServiceCoords
  serviceEventLog env coords inf
    where clusterServiceCoords :: MonadAWS m => ClusterRef -> m [ServiceCoords]
          clusterServiceCoords cref = sourceToList $ fetchServices cref
            =$= filterC isActiveService
            =$= CL.mapMaybe serviceCoords

          allServiceCoords :: MonadAWS m => m [ServiceCoords]
          allServiceCoords = do
            coords <- concat <$> mapM clusterServiceCoords clusterRefs
            return coords
