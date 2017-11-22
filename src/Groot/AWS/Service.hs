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
     , servicesEventLog
     , clusterServiceEventLog
     ) where

import Control.Applicative
import qualified Control.Concurrent as TH
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CH
import Data.Maybe (listToMaybe)
import Data.Time
import Groot.AWS.Cluster
import Groot.Data
import qualified Groot.Data.Conduit as CM
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

serviceEventLog :: MonadAWS m
                => ServiceCoords
                -> Bool
                -> Source m ECS.ServiceEvent
serviceEventLog (ServiceCoords serviceRef clusterRef) inf = yield Nothing =$= loop
  where serviceEvents :: MonadAWS m => Maybe UTCTime -> m [ECS.ServiceEvent]
        serviceEvents lastEventTime = do
          service  <- getService serviceRef (Just clusterRef)
          service' <- if (matches isActiveService service)
                      then return service
                      else throwM $ inactiveService serviceRef clusterRef
          events  <- return $ service' ^. ECS.csEvents
          return $ case lastEventTime of
            Nothing -> events
            Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. ECS.seCreatedAt) events

        fetch :: MonadAWS m => Maybe UTCTime -> m ([ECS.ServiceEvent], Maybe UTCTime)
        fetch lastEventTime = do
          events <- serviceEvents lastEventTime
          return $ (events, listToMaybe events >>= view ECS.seCreatedAt)

        loop :: MonadAWS m => Conduit (Maybe UTCTime) m ECS.ServiceEvent
        loop = do
          prev <- await
          case prev of
            Nothing            -> return ()
            Just lastEventTime -> do
              (events, nextTime) <- lift $ fetch lastEventTime              
              CL.sourceList $ reverse events
              if inf then do                
                liftIO $ TH.threadDelay 1000000
                leftover $ nextTime <|> lastEventTime
                loop
              else return ()

servicesEventLog' :: (MonadResource m) 
                  => [ServiceCoords]
                  -> Bool
                  -> m [Source m ECS.ServiceEvent]
servicesEventLog' coords inf = merge eventSources
  where ignoreServicesBecomingInactive :: MonadCatch m => m () -> m ()
        ignoreServicesBecomingInactive action =
          catching _InactiveService action $ \_ -> return ()

        --safeEventSource :: MonadAWS m => ServiceCoords -> Source m ECS.ServiceEvent
        safeEventSource s = ignoreServicesBecomingInactive (serviceEventLog s inf)

        --eventSources :: MonadAWS m => [Source m ECS.ServiceEvent]
        eventSources = safeEventSource <$> coords

        merge sources = CH.mergeSources sources 500

servicesEventLog :: (MonadAWS m, Traversable t) => t ServiceCoords -> Bool -> Source m ECS.ServiceEvent
servicesEventLog coords inf = CM.mergeSourcesOn eventOrd eventSources
  where epoch = UTCTime (ModifiedJulianDay 40587) (secondsToDiffTime 0)

        eventOrd :: ECS.ServiceEvent -> UTCTime
        eventOrd event = maybe epoch id $ event ^. ECS.seCreatedAt

        ignoreServicesBecomingInactive :: MonadCatch m => m () -> m ()
        ignoreServicesBecomingInactive action =
          catching _InactiveService action $ \_ -> return ()

        safeEventSource s = ignoreServicesBecomingInactive (serviceEventLog s inf)
        eventSources = safeEventSource <$> coords

clusterServiceEventLog :: (MonadAWS m, Traversable t) => t ClusterRef -> Bool -> Source m ECS.ServiceEvent
clusterServiceEventLog clusterRefs inf = 
  let clusterServiceCoords :: MonadAWS m => ClusterRef -> m [ServiceCoords]
      clusterServiceCoords cref = sourceToList $ fetchServices cref
        =$= filterC isActiveService
        =$= CL.mapMaybe serviceCoords

      allServiceCoords :: MonadAWS m => m [ServiceCoords]
      allServiceCoords = do
        coords <- concat <$> mapM clusterServiceCoords clusterRefs
        liftIO . putStrLn $ "Found the following services: " ++ (show coords)
        return coords

      mergeEvents :: MonadAWS m => Conduit [ServiceCoords] m ECS.ServiceEvent
      mergeEvents = awaitForever (\coords -> toProducer $ servicesEventLog coords inf)
  in yieldM allServiceCoords =$= mergeEvents