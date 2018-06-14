{-# LANGUAGE TypeFamilies #-}

module Groot.AWS.Cluster
     ( clusterName
     , clusterExists
     , fetchClusters
     , findCluster
     , getCluster
     , handleClusterNotFoundException
     , taskCluster
     , serviceCluster
     ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Maybe                (isJust, listToMaybe)
import           Network.AWS
import qualified Network.AWS.ECS           as ECS

import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

-- |Extracts the cluster name as a 'ClusterRef'
clusterName :: ECS.Cluster -> Maybe ClusterRef
clusterName cluster = ClusterRef <$> cluster ^. ECS.cClusterName

-- |Checks whether the given cluster reference is pointing to an existing cluster
clusterExists :: MonadAWS m => ClusterRef -> m Bool
clusterExists clusterRef = isJust <$> (runMaybeT $ findCluster clusterRef)

-- |Obtains the cluster details of a given task
taskCluster :: MonadAWS m => ECS.Task -> MaybeT m ECS.Cluster
taskCluster tsk = do
  arn <- MaybeT . return $ ClusterRef <$> tsk ^. ECS.tClusterARN
  getCluster arn

-- |Obtains the cluster details of a given service
serviceCluster :: MonadAWS m => ECS.ContainerService -> MaybeT m ECS.Cluster
serviceCluster service = do
  arn <- MaybeT . return $ ClusterRef <$> service ^. ECS.csClusterARN
  getCluster arn

-- |Translates an AWS 'ClusterNotFoundException' into a Groot's 'ClusterNotFound'
handleClusterNotFoundException :: MonadCatch m => ClusterRef -> m a -> m a
handleClusterNotFoundException clusterRef action =
  catching ECS._ClusterNotFoundException action $ \_ -> throwM $ clusterNotFound clusterRef

fetchClusters' :: MonadAWS m => [ClusterRef] -> m [ECS.Cluster]
fetchClusters' []          = return []
fetchClusters' clusterRefs = do
  res <- send $ ECS.dcClusters .~ (toText <$> clusterRefs) $ ECS.describeClusters
  return $ res ^. ECS.dcrsClusters

-- |Builds a Conduit with all the clusters available
fetchClusters :: MonadAWS m => ConduitT () ECS.Cluster m ()
fetchClusters =
  paginate ECS.listClusters
    .| CL.concatMapM (\x -> fetchClusters' (ClusterRef <$> x ^. ECS.lcrsClusterARNs))

-- |Tries to find a given cluster by its reference
findCluster :: MonadAWS m => ClusterRef -> MaybeT m ECS.Cluster
findCluster ref = MaybeT $ listToMaybe <$> fetchClusters' [ref]

-- |Same as 'findCluster' but it will throw a 'ClusterNotFound' in the Monad 'm'
-- in case the cluster does not exist
getCluster :: MonadAWS m => ClusterRef -> m ECS.Cluster
getCluster clusterRef = do
  cluster <- runMaybeT $ findCluster clusterRef
  case cluster of
    Just c  -> return c
    Nothing -> throwM $ clusterNotFound clusterRef
