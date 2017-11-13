module Groot.AWS.Instance
     (
       fetchInstances
     , fetchAllInstances
     , findInstances
     , findInstance
     , getInstance
     ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Groot.AWS.Cluster
import Groot.Data
import Groot.Exception
import Network.AWS
import qualified Network.AWS.ECS as ECS
import Network.AWS.Data.Text

fetchInstanceBatch :: MonadAWS m => ClusterRef -> [InstanceRef] -> m [ECS.ContainerInstance]
fetchInstanceBatch _          []           = return []
fetchInstanceBatch clusterRef instanceRefs =
  let fetch = do
        res <- send $ ECS.dciCluster ?~ (toText clusterRef)
               $ ECS.dciContainerInstances .~ (toText <$> instanceRefs)
               $ ECS.describeContainerInstances
        return $ res ^. ECS.dcisrsContainerInstances
  in handleClusterNotFoundException clusterRef fetch

fetchInstances :: MonadAWS m => ClusterRef -> Source m ECS.ContainerInstance
fetchInstances cref@(ClusterRef ref) =
  paginate (ECS.lciCluster ?~ ref $ ECS.listContainerInstances)
    =$= CL.concatMapM (\x -> fetchInstanceBatch cref (InstanceRef <$> x ^. ECS.lcirsContainerInstanceARNs))

fetchInstancesC :: MonadAWS m => [InstanceRef] -> Conduit ClusterRef m ECS.ContainerInstance
fetchInstancesC instances =
  awaitForever (\cref -> yieldM $ fetchInstanceBatch cref instances) =$= CL.concat

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
