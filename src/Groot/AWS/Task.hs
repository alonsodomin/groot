module Groot.AWS.Task
     (
       fetchTasks
     , fetchAllTasks
     , findTasks
     , findTask
     , getTask
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

fetchTaskBatch :: MonadAWS m => ClusterRef -> [TaskRef] -> m [ECS.Task]
fetchTaskBatch _          []       = return []
fetchTaskBatch clusterRef taskRefs =
  let fetch = do
        res <- send $ ECS.dtCluster ?~ (toText clusterRef)
               $ ECS.dtTasks .~ (toText <$> taskRefs)
               $ ECS.describeTasks
        return $ res ^. ECS.dtrsTasks
  in handleClusterNotFoundException clusterRef fetch

fetchTasksC :: MonadAWS m => [TaskRef] -> Conduit ClusterRef m ECS.Task
fetchTasksC tasks = awaitForever (\cref -> yieldM $ fetchTaskBatch cref tasks) =$= CL.concat

fetchTasks :: MonadAWS m => ClusterRef -> Source m ECS.Task
fetchTasks cref@(ClusterRef ref) =
  paginate (ECS.ltCluster ?~ ref $ ECS.listTasks)
    =$= CL.concatMapM (\x -> fetchTaskBatch cref (TaskRef <$> x ^. ECS.ltrsTaskARNs))

fetchAllTasks :: MonadAWS m => Source m ECS.Task
fetchAllTasks =
  let fetchAllTasksC = awaitForever (\cref -> yieldM . sourceToList $ fetchTasks cref) =$= CL.concat
  in fetchClusters
     =$= CL.mapMaybe clusterName
     =$= fetchAllTasksC

findTasks :: MonadAWS m => [TaskRef] -> Maybe ClusterRef -> Source m ECS.Task
findTasks tasks (Just clusterRef) =
  yield clusterRef =$= fetchTasksC tasks
findTasks tasks _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchTasksC tasks

findTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> MaybeT m ECS.Task
findTask tref cref = MaybeT . runConduit $ findTasks [tref] cref =$= CL.head

getTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> m ECS.Task
getTask tref cref = do
  t <- runMaybeT $ findTask tref cref
  case t of
    Just x  -> return x
    Nothing -> throwM $ taskNotFound tref cref