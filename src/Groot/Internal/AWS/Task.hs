module Groot.Internal.AWS.Task
     ( fetchTasks
     , fetchAllTasks
     , fetchServiceTasks
     , findTasks
     , findTask
     , getTask
     ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Network.AWS
import           Network.AWS.Data.Text
import qualified Network.AWS.ECS           as ECS

import           Groot.Internal.AWS.Cluster
import           Groot.Exception
import           Groot.Types

fetchTaskBatch :: MonadAWS m => [TaskRef] -> ClusterRef -> m [ECS.Task]
fetchTaskBatch []       _          = return []
fetchTaskBatch taskRefs clusterRef =
  let fetch = do
        res <- send $ ECS.dtCluster ?~ (toText clusterRef)
               $ ECS.dtTasks .~ (toText <$> taskRefs)
               $ ECS.describeTasks
        return $ res ^. ECS.dtrsTasks
  in handleClusterNotFoundException clusterRef fetch

fetchTasksC :: MonadAWS m => [TaskRef] -> ConduitT ClusterRef ECS.Task m ()
fetchTasksC tasks = awaitForever (\cref -> yieldM $ fetchTaskBatch tasks cref) .| CL.concat

fetchTasks :: MonadAWS m => ClusterRef -> ConduitT () ECS.Task m ()
fetchTasks cref@(ClusterRef ref) =
  --handleClusterNotFoundException cref (paginate (ECS.ltCluster ?~ ref $ ECS.listTasks))
  paginate (ECS.ltCluster ?~ ref $ ECS.listTasks)
    .| CL.concatMapM (\x -> fetchTaskBatch (TaskRef <$> x ^. ECS.ltrsTaskARNs) cref)

fetchAllTasks :: MonadAWS m => ConduitT () ECS.Task m ()
fetchAllTasks =
  let fetchAllTasksC = awaitForever (\cref -> toProducer $ fetchTasks cref)
  in fetchClusters
     .| CL.mapMaybe clusterName
     .| fetchAllTasksC

findTasks :: MonadAWS m => [TaskRef] -> Maybe ClusterRef -> ConduitT () ECS.Task m ()
findTasks tasks (Just clusterRef) = yield clusterRef .| fetchTasksC tasks
findTasks tasks _                 = fetchClusters .| CL.mapMaybe clusterName .| fetchTasksC tasks

findTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> MaybeT m ECS.Task
findTask tref cref = MaybeT . runConduit $ findTasks [tref] cref .| CL.head

getTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> m ECS.Task
getTask tref cref = do
  t <- runMaybeT $ findTask tref cref
  case t of
    Just x  -> return x
    Nothing -> throwM $ taskNotFound tref cref

fetchServiceTasks :: MonadAWS m => Maybe ClusterRef -> ContainerServiceRef -> ConduitT () ECS.Task m ()
fetchServiceTasks Nothing sref =
  fetchClusters
    .| CL.mapMaybe clusterName
    .| awaitForever (\c -> toProducer $ fetchServiceTasks (Just c) sref)
fetchServiceTasks (Just cref@(ClusterRef cluster)) (ContainerServiceRef service) = tasks
  --catching ECS._ServiceNotFoundException tasks $ \_ -> CL.sourceNull
  where tasks = paginate (ECS.ltCluster ?~ cluster $ ECS.ltServiceName ?~ service $ ECS.listTasks)
                .| CL.map (\x -> TaskRef <$> x ^. ECS.ltrsTaskARNs)
                .| awaitForever (\ts -> toProducer $ findTasks ts (Just cref))
