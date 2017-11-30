module Groot.AWS.Instance
     (
       fetchInstances
     , fetchAllInstances
     , findInstances
     , findInstance
     , getInstance
     , taskInstance
     ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Groot.AWS.Cluster
import           Groot.Data
import           Groot.Exception
import           Network.AWS
import           Network.AWS.Data.Text
import qualified Network.AWS.ECS           as ECS

taskInstance :: MonadAWS m => ECS.Task -> MaybeT m ECS.ContainerInstance
taskInstance tsk = do
  clusterArn  <- MaybeT . return $ ClusterRef <$> tsk ^. ECS.tClusterARN
  instanceArn <- MaybeT . return $ InstanceRef <$> tsk ^. ECS.tContainerInstanceARN
  getInstance instanceArn (Just clusterArn)

fetchInstanceBatch :: MonadAWS m => [InstanceRef] -> ClusterRef -> m [ECS.ContainerInstance]
fetchInstanceBatch []           _          = return []
fetchInstanceBatch instanceRefs clusterRef =
  let fetch = do
        res <- send $ ECS.dciCluster ?~ (toText clusterRef)
               $ ECS.dciContainerInstances .~ (toText <$> instanceRefs)
               $ ECS.describeContainerInstances
        return $ res ^. ECS.dcisrsContainerInstances
  in handleClusterNotFoundException clusterRef fetch

fetchInstances :: MonadAWS m => ClusterRef -> Source m ECS.ContainerInstance
fetchInstances cref@(ClusterRef ref) =
  handleClusterNotFoundException cref (paginate (ECS.lciCluster ?~ ref $ ECS.listContainerInstances))
    =$= CL.concatMapM (\x -> fetchInstanceBatch (InstanceRef <$> x ^. ECS.lcirsContainerInstanceARNs) cref)

fetchInstancesC :: MonadAWS m => [InstanceRef] -> Conduit ClusterRef m ECS.ContainerInstance
fetchInstancesC instances =
  awaitForever (\cref -> yieldM $ fetchInstanceBatch instances cref) =$= CL.concat

fetchAllInstances :: MonadAWS m => Source m ECS.ContainerInstance
fetchAllInstances =
  let fetchAllInstancesC = awaitForever (\cref -> yieldM . sourceToList $ fetchInstances cref) =$= CL.concat
  in fetchClusters
     =$= CL.mapMaybe clusterName
     =$= fetchAllInstancesC

findInstances :: MonadAWS m => [InstanceRef] -> Maybe ClusterRef -> Source m ECS.ContainerInstance
findInstances instances (Just clusterRef) =
  yield clusterRef =$= fetchInstancesC instances
findInstances instances _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchInstancesC instances

findInstance :: MonadAWS m => InstanceRef -> Maybe ClusterRef -> MaybeT m ECS.ContainerInstance
findInstance iref cref = MaybeT . runConduit $ findInstances [iref] cref =$= CL.head

getInstance :: MonadAWS m => InstanceRef -> Maybe ClusterRef -> m ECS.ContainerInstance
getInstance iref cref = do
  inst <- runMaybeT $ findInstance iref cref
  case inst of
    Just x  -> return x
    Nothing -> throwM $ instanceNotFound iref cref
