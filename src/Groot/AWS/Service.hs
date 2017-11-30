{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Groot.AWS.Service
     (
       serviceCoords
     , findServices
     , findService
     , findServiceCoords
     , getService
     , fetchServices
     , fetchAllServices
     ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (listToMaybe)
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
