module Groot.AWS.Cluster
     (
       clusterName
     , clusterExists
     , fetchClusters
     , findCluster
     , getCluster
     , handleClusterNotFoundException
     , taskCluster
     ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (listToMaybe, isJust)
import Groot.Data
import Groot.Exception
import Network.AWS
import qualified Network.AWS.ECS as ECS
import Network.AWS.Data.Text

clusterName :: ECS.Cluster -> Maybe ClusterRef
clusterName cluster = ClusterRef <$> cluster ^. ECS.cClusterName

clusterExists :: MonadAWS m => ClusterRef -> m Bool
clusterExists clusterRef = isJust <$> (runMaybeT $ findCluster clusterRef)

taskCluster :: MonadAWS m => ECS.Task -> MaybeT m ECS.Cluster
taskCluster tsk = do
  arn <- MaybeT . return $ ClusterRef <$> tsk ^. ECS.tClusterARN
  getCluster arn

handleClusterNotFoundException :: MonadCatch m => ClusterRef -> m a -> m a
handleClusterNotFoundException clusterRef action =
  catching ECS._ClusterNotFoundException action $ \_ -> throwM $ clusterNotFound clusterRef

fetchClusterBatch :: MonadAWS m => [ClusterRef] -> m [ECS.Cluster]
fetchClusterBatch []          = return []
fetchClusterBatch clusterRefs = do
  res <- send $ ECS.dcClusters .~ (toText <$> clusterRefs) $ ECS.describeClusters
  return $ res ^. ECS.dcrsClusters

fetchClusters :: MonadAWS m => Source m ECS.Cluster
fetchClusters =
  paginate ECS.listClusters
    =$= CL.concatMapM (\x -> fetchClusterBatch (ClusterRef <$> x ^. ECS.lcrsClusterARNs))

findCluster :: MonadAWS m => ClusterRef -> MaybeT m ECS.Cluster
findCluster (ClusterRef cref) = MaybeT $ do
  res <- send $ ECS.dcClusters .~ [cref] $ ECS.describeClusters
  return $ listToMaybe (res ^. ECS.dcrsClusters)

getCluster :: MonadAWS m => ClusterRef -> m ECS.Cluster
getCluster clusterRef = do
  cluster <- runMaybeT $ findCluster clusterRef
  case cluster of
    Just c  -> return c
    Nothing -> throwM $ clusterNotFound clusterRef