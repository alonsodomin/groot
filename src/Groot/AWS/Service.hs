{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Groot.AWS.Service
     (
       serviceCoords
     , serviceTaskDefArn
     , findServices
     , findService
     , findServiceCoords
     , getService
     , fetchServices
     , fetchAllServices
     ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Maybe                (listToMaybe)
import           Network.AWS               hiding (await)
import           Network.AWS.Data.Text
import qualified Network.AWS.ECS           as ECS

import           Groot.AWS.Cluster
import           Groot.Data.Filter
import           Groot.Exception
import           Groot.Types

serviceCoords :: ECS.ContainerService -> Maybe ContainerServiceCoords
serviceCoords service = ContainerServiceCoords <$> serviceRef <*> clusterRef
  where serviceRef = ContainerServiceRef <$> service ^. ECS.csServiceARN
        clusterRef = ClusterRef          <$> service ^. ECS.csClusterARN

serviceTaskDefArn :: ECS.ContainerService -> Maybe TaskDefArn
serviceTaskDefArn service = join $ (either (\_ -> Nothing) Just . fromText) <$> service ^. ECS.csTaskDefinition

findServiceCoords :: (MonadAWS m, Traversable t)
                  => t ContainerServiceRef
                  -> m (t ContainerServiceCoords)
findServiceCoords = traverse singleServiceCoords
  where singleServiceCoords :: MonadAWS m => ContainerServiceRef -> m ContainerServiceCoords
        singleServiceCoords serviceRef = do
          mcoords <- serviceCoords <$> getService serviceRef Nothing
          case mcoords of
            Just c  -> return c
            Nothing -> throwM $ serviceNotFound serviceRef Nothing

fetchServiceBatch :: MonadAWS m => [ContainerServiceRef] -> ClusterRef -> m [ECS.ContainerService]
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
    =$= CL.concatMapM (\x -> fetchServiceBatch (ContainerServiceRef <$> x ^. ECS.lsrsServiceARNs) clusterRef)

fetchAllServices :: MonadAWS m => Source m ECS.ContainerService
fetchAllServices = fetchClusters
  =$= CL.mapMaybe clusterName
  =$= fetchAllServicesC

fetchServicesC' :: MonadAWS m
                => [ContainerServiceRef]
                -> Conduit ClusterRef m (ClusterRef, ECS.ContainerService)
fetchServicesC' services =
  awaitForever (\cref -> yieldM $ do
    servs <- fetchServiceBatch services cref
    return $ map ((,) cref) servs
  ) =$= CL.concat

fetchAllServicesC :: MonadAWS m => Conduit ClusterRef m ECS.ContainerService
fetchAllServicesC = awaitForever (\cref -> toProducer $ fetchServices cref)

findServices' :: MonadAWS m
              => [ContainerServiceRef]
              -> Maybe ClusterRef
              -> Source m (ClusterRef, ECS.ContainerService)
findServices' services (Just clusterRef) =
  yield clusterRef =$= fetchServicesC' services
findServices' services _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchServicesC' services

findServices :: MonadAWS m
             => [ContainerServiceRef]
             -> Maybe ClusterRef
             -> Source m ECS.ContainerService
findServices services cref = findServices' services cref =$= CL.map snd

findService :: MonadAWS m
            => ContainerServiceRef
            -> Maybe ClusterRef
            -> MaybeT m ECS.ContainerService
findService sref cref = MaybeT $ extractRequested cref
  where extractRequested (Just _) =
          runConduit $ findServices' [sref] cref =$= CL.map snd =$= CL.head
        extractRequested _ = do
          found <- sourceToList $ findServices' [sref] Nothing
                   =$= filterOnC snd (isContainerService sref)
          if (length found) > 1
          then throwM $ ambiguousServiceName sref (fst <$> found)
          else return $ snd <$> listToMaybe found

getService :: MonadAWS m
           => ContainerServiceRef
           -> Maybe ClusterRef
           -> m ECS.ContainerService
getService serviceName clusterRef = do
  serv <- runMaybeT $ findService serviceName clusterRef
  case serv of
    Just x  -> return x
    Nothing -> throwM $ serviceNotFound serviceName clusterRef
