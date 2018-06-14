module Groot.AWS.Instance
     ( fetchInstances
     , fetchAllInstances
     , findInstances
     , findInstance
     , getInstance
     , taskInstance
     -- EC2 instances
     , findEc2Instances
     ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Network.AWS
import qualified Network.AWS.EC2           as EC2
import qualified Network.AWS.ECS           as ECS

import           Groot.AWS.Cluster
import           Groot.Data.Text
import           Groot.Exception
import           Groot.Types

-- |Obtains the instance of a given task
taskInstance :: MonadAWS m => ECS.Task -> MaybeT m ECS.ContainerInstance
taskInstance tsk = do
  clusterArn  <- MaybeT . return $ ClusterRef <$> tsk ^. ECS.tClusterARN
  instanceArn <- MaybeT . return $ ContainerInstanceRef <$> tsk ^. ECS.tContainerInstanceARN
  getInstance instanceArn (Just clusterArn)

fetchInstanceBatch :: MonadAWS m => [ContainerInstanceRef] -> ClusterRef -> m [ECS.ContainerInstance]
fetchInstanceBatch []           _          = return []
fetchInstanceBatch instanceRefs clusterRef =
  let fetch = do
        res <- send $ ECS.dciCluster ?~ (toText clusterRef)
               $ ECS.dciContainerInstances .~ (toText <$> instanceRefs)
               $ ECS.describeContainerInstances
        return $ res ^. ECS.dcisrsContainerInstances
  in handleClusterNotFoundException clusterRef fetch

-- |Builds a Conduit with all the task instances in a given cluster
fetchInstances :: MonadAWS m => ClusterRef -> ConduitT () ECS.ContainerInstance m ()
fetchInstances cref@(ClusterRef ref) =
  --handleClusterNotFoundException cref (paginate (ECS.lciCluster ?~ ref $ ECS.listContainerInstances))
  paginate (ECS.lciCluster ?~ ref $ ECS.listContainerInstances)
    .| CL.concatMapM (\x -> fetchInstanceBatch (ContainerInstanceRef <$> x ^. ECS.lcirsContainerInstanceARNs) cref)

fetchInstancesC :: MonadAWS m => [ContainerInstanceRef] -> ConduitT ClusterRef ECS.ContainerInstance m ()
fetchInstancesC instances =
  awaitForever (\cref -> yieldM $ fetchInstanceBatch instances cref) .| CL.concat

-- |Builds a Conduit with all the task instances from all the clusters
fetchAllInstances :: MonadAWS m => ConduitT () ECS.ContainerInstance m ()
fetchAllInstances =
  let fetchAllInstancesC = awaitForever (\cref -> toProducer $ fetchInstances cref)
  in fetchClusters
     .| CL.mapMaybe clusterName
     .| fetchAllInstancesC

-- |Efficiently obtain all task instances whilst filtering a given cluster reference
-- or a set of instance references
findInstances :: MonadAWS m => [ContainerInstanceRef] -> Maybe ClusterRef -> ConduitT () ECS.ContainerInstance m ()
findInstances instances (Just clusterRef) =
  yield clusterRef .| fetchInstancesC instances
findInstances instances _ =
  fetchClusters .| CL.mapMaybe clusterName .| fetchInstancesC instances

-- |Same as 'findInstances' but for a single instance reference
findInstance :: MonadAWS m => ContainerInstanceRef -> Maybe ClusterRef -> MaybeT m ECS.ContainerInstance
findInstance iref cref = MaybeT . runConduit $ findInstances [iref] cref .| CL.head

-- |Same as 'findInstance' but will throw an 'InstanceNotFound' in the monad 'm'
-- in case the given instance reference does not exist
getInstance :: MonadAWS m => ContainerInstanceRef -> Maybe ClusterRef -> m ECS.ContainerInstance
getInstance iref cref = do
  inst <- runMaybeT $ findInstance iref cref
  case inst of
    Just x  -> return x
    Nothing -> throwM $ instanceNotFound iref cref

findEc2Instances :: MonadAWS m => [EC2InstanceId] -> ConduitT () EC2.Instance m ()
findEc2Instances ids = paginate (EC2.diiInstanceIds .~ (toText <$> ids) $ EC2.describeInstances)
  .| CL.concatMap (\x -> x ^. EC2.dirsReservations)
  .| CL.concatMap (\x -> x ^. EC2.rInstances)
