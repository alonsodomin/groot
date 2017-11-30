{-# LANGUAGE TypeFamilies #-}

module Groot.AWS.Cluster
     (
       clusterName
     , clusterExists
     , fetchClusters
     , findCluster
     , getCluster
     , handleClusterNotFoundException
     , taskCluster
     , serviceCluster
     ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Maybe                (isJust, listToMaybe)
import qualified Data.Text                 as T
import           Network.AWS
import           Network.AWS.Data.Text
import qualified Network.AWS.ECS           as ECS

import           Groot.Exception
import           Groot.Types

clusterName :: ECS.Cluster -> Maybe ClusterRef
clusterName cluster = ClusterRef <$> cluster ^. ECS.cClusterName

clusterExists :: MonadAWS m => ClusterRef -> m Bool
clusterExists clusterRef = isJust <$> (runMaybeT $ findCluster clusterRef)

taskCluster :: MonadAWS m => ECS.Task -> MaybeT m ECS.Cluster
taskCluster tsk = do
  arn <- MaybeT . return $ ClusterRef <$> tsk ^. ECS.tClusterARN
  getCluster arn

serviceCluster :: MonadAWS m => ECS.ContainerService -> MaybeT m ECS.Cluster
serviceCluster service = do
  arn <- MaybeT . return $ ClusterRef <$> service ^. ECS.csClusterARN
  getCluster arn

handleClusterNotFoundException :: MonadCatch m => ClusterRef -> m a -> m a
handleClusterNotFoundException clusterRef action =
  catching ECS._ClusterNotFoundException action $ \_ -> throwM $ clusterNotFound clusterRef

fetchClusters' :: MonadAWS m => [ClusterRef] -> m [ECS.Cluster]
fetchClusters' []          = return []
fetchClusters' clusterRefs = do
  res <- send $ ECS.dcClusters .~ (toText <$> clusterRefs) $ ECS.describeClusters
  return $ res ^. ECS.dcrsClusters

fetchClusters :: MonadAWS m => Source m ECS.Cluster
fetchClusters =
  paginate ECS.listClusters
    =$= CL.concatMapM (\x -> fetchClusters' (ClusterRef <$> x ^. ECS.lcrsClusterARNs))

findCluster :: MonadAWS m => ClusterRef -> MaybeT m ECS.Cluster
findCluster ref = MaybeT $ listToMaybe <$> fetchClusters' [ref]

getCluster :: MonadAWS m => ClusterRef -> m ECS.Cluster
getCluster clusterRef = do
  cluster <- runMaybeT $ findCluster clusterRef
  case cluster of
    Just c  -> return c
    Nothing -> throwM $ clusterNotFound clusterRef
