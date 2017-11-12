{-# LANGUAGE RankNTypes #-}

module Groot.Core 
     ( 
     -- Clusters
       clusterName
     , clusterExists
     , findCluster
     , getCluster
     , fetchClusters
     -- Container Instances
     , findInstances
     , findInstance
     , getInstance
     , fetchInstances
     , fetchAllInstances
     -- Tasks
     , findTasks
     , findTask
     , getTask
     , fetchTasks
     , fetchAllTasks
     , stopTask
     , startTask
     , restartTask
     -- Task Definitions
     , taskDefFromTask
     , getTaskDef
     , fetchTaskDefs
     -- Services
     , serviceCoords
     , findServices
     , findService
     , getService
     , fetchServices
     , fetchAllServices
     , serviceEventLog
     ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (listToMaybe, isJust)
import qualified Data.Text as T
import Data.Time.Clock
import Network.AWS hiding (await)
import qualified Network.AWS as A
import Network.AWS.Data.Text
import Network.AWS.ECS hiding (cluster, stopTask, startTask, Running, Stopped)
import qualified Network.AWS.ECS as ECS
import Network.AWS.Waiter

import Groot.Data
import Groot.Exception

-- Clusters

clusterName :: Cluster -> Maybe ClusterRef
clusterName cluster = ClusterRef <$> cluster ^. cClusterName

clusterExists :: MonadAWS m => ClusterRef -> m Bool
clusterExists clusterRef = isJust <$> (runMaybeT $ findCluster clusterRef)

findCluster :: MonadAWS m => ClusterRef -> MaybeT m Cluster
findCluster (ClusterRef cref) = MaybeT $ do
  res <- send $ dcClusters .~ [cref] $ describeClusters
  return $ listToMaybe (res ^. dcrsClusters)

getCluster :: MonadAWS m => ClusterRef -> m Cluster
getCluster clusterRef = do
  cluster <- runMaybeT $ findCluster clusterRef
  case cluster of
    Just c  -> return c
    Nothing -> throwM $ clusterNotFound clusterRef

fetchClusters :: MonadAWS m => Source m Cluster
fetchClusters =
  let getClusterBatch batch = do
        res <- send $ dcClusters .~ batch $ describeClusters
        return $ res ^. dcrsClusters
  in paginate listClusters
      =$= CL.concatMapM (\x -> getClusterBatch (x ^. lcrsClusterARNs))

-- Container Instances

fetchInstancesC :: MonadAWS m => [InstanceRef] -> Conduit ClusterRef m ContainerInstance
fetchInstancesC instances =
  awaitForever (\cref -> yieldM $ do
    res <- send $ dciCluster ?~ (toText cref) $ dciContainerInstances .~ (toText <$> instances) $ describeContainerInstances
    return $ res ^. dcisrsContainerInstances
  ) =$= CL.concat

fetchAllInstancesC :: MonadAWS m => Conduit ClusterRef m ContainerInstance
fetchAllInstancesC = awaitForever (\cref -> yieldM . sourceToList $ fetchInstances cref) =$= CL.concat

findInstances :: MonadAWS m => [InstanceRef] -> Maybe ClusterRef -> Source m ContainerInstance
findInstances instances (Just clusterRef) =
  yield clusterRef =$= fetchInstancesC instances
findInstances instances _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchInstancesC instances

findInstance :: MonadAWS m => InstanceRef -> Maybe ClusterRef -> MaybeT m ContainerInstance
findInstance iref cref = MaybeT . runConduit $ findInstances [iref] cref =$= CL.head

getInstance :: MonadAWS m => InstanceRef -> Maybe ClusterRef -> m ContainerInstance
getInstance iref cref = do
  inst <- runMaybeT $ findInstance iref cref
  case inst of
    Just x  -> return x
    Nothing -> throwM $ instanceNotFound iref cref

fetchInstances :: MonadAWS m => ClusterRef -> Source m ContainerInstance
fetchInstances (ClusterRef cref) =
  let getInstanceBatch []    = return []
      getInstanceBatch batch = do
        res <- send $ dciCluster ?~ cref $ dciContainerInstances .~ batch $ describeContainerInstances
        return $ res ^. dcisrsContainerInstances
  in paginate (lciCluster ?~ cref $ listContainerInstances)
     =$= CL.concatMapM (\x -> getInstanceBatch (view lcirsContainerInstanceARNs x))

fetchAllInstances :: MonadAWS m => Source m ContainerInstance
fetchAllInstances = fetchClusters
  =$= CL.mapMaybe clusterName
  =$= fetchAllInstancesC

-- Tasks

fetchTasksC :: MonadAWS m => [TaskRef] -> Conduit ClusterRef m Task
fetchTasksC tasks =
  awaitForever (\cref -> yieldM $ do
    res <- send $ dtCluster ?~ (toText cref) $ dtTasks .~ (toText <$> tasks) $ describeTasks
    return $ listToMaybe $ res ^. dtrsTasks
  ) =$= CL.concat

fetchAllTasksC :: MonadAWS m => Conduit ClusterRef m Task
fetchAllTasksC = awaitForever (\cref -> yieldM . sourceToList $ fetchTasks cref) =$= CL.concat

findTasks :: MonadAWS m => [TaskRef] -> Maybe ClusterRef -> Source m Task
findTasks tasks (Just clusterRef) =
  yield clusterRef =$= fetchTasksC tasks
findTasks tasks _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchTasksC tasks

findTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> MaybeT m Task
findTask tref cref = MaybeT . runConduit $ findTasks [tref] cref =$= CL.head

getTask :: MonadAWS m => TaskRef -> Maybe ClusterRef -> m Task
getTask tref cref = do
  t <- runMaybeT $ findTask tref cref
  case t of
    Just x  -> return x
    Nothing -> throwM $ taskNotFound tref cref

fetchTasks :: MonadAWS m => ClusterRef -> Source m Task
fetchTasks (ClusterRef cref) =
  let getTaskBatch []    = return []
      getTaskBatch batch = do
        res <- send $ dtCluster ?~ cref $ dtTasks .~ batch $ describeTasks
        return $ res ^. dtrsTasks
  in paginate (ltCluster ?~ cref $ listTasks)
     =$= CL.concatMapM (\x -> getTaskBatch (view ltrsTaskARNs x))

fetchAllTasks :: MonadAWS m => Source m Task
fetchAllTasks = fetchClusters
  =$= CL.mapMaybe clusterName
  =$= fetchAllTasksC

stopTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
stopTask tref@(TaskRef taskRef) clusterRef = 
  let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
  in do
    send $ stCluster ?~ (toText clusterRef) $ ECS.stopTask taskRef
    liftIO . putStr $ "Stopping task '" ++ (T.unpack taskRef) 
      ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "
    result <- A.await tasksStopped describeReq
    case result of
      AcceptSuccess -> liftIO $ putStrLn "OK"
      _             -> do
        liftIO $ putStrLn "FAILED"
        throwM $ taskStatusTransitionFailed tref Running Stopped

startTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
startTask tref@(TaskRef taskRef) clusterRef = 
  let describeReq = dtCluster ?~ (toText clusterRef) $ dtTasks .~ [taskRef] $ describeTasks
  in do
    send $ sCluster ?~ (toText clusterRef) $ ECS.startTask taskRef
    liftIO . putStr $ "Starting task '" ++ (T.unpack taskRef) 
      ++ "' in cluster '" ++ (T.unpack . toText $ clusterRef) ++ "'... "
    result <- A.await tasksRunning describeReq
    case result of
      AcceptSuccess -> liftIO $ putStrLn "OK"
      _             -> do
        liftIO $ putStrLn "FAILED"
        throwM $ taskStatusTransitionFailed tref Stopped Running

restartTask :: MonadAWS m => TaskRef -> ClusterRef -> m ()
restartTask taskRef clusterRef = do
  stopTask taskRef clusterRef
  startTask taskRef clusterRef

-- Task Definitions

getTaskDef :: MonadAWS m => TaskDefRef -> MaybeT m TaskDefinition
getTaskDef (TaskDefRef arn) = MaybeT $ do
  res <- send $ describeTaskDefinition arn
  return $ res ^. desrsTaskDefinition

taskDefFromTask :: MonadAWS m => Task -> MaybeT m TaskDefinition
taskDefFromTask tsk = do
  arn <- MaybeT . return $ TaskDefRef <$> tsk ^. tTaskDefinitionARN
  getTaskDef arn

fetchTaskDefs :: (MonadAWS m, Foldable f) => f TaskDefFilter -> Source m TaskDefinition
fetchTaskDefs filters =
  let tds :: TaskDefStatus -> TaskDefinitionStatus
      tds TaskActive   = TDSActive
      tds TaskInactive = TDSInactive

      withFilter :: TaskDefFilter -> ListTaskDefinitions -> ListTaskDefinitions
      withFilter (FamilyFilter (TaskFamily f)) = ltdFamilyPrefix ?~ f
      withFilter (StatusFilter s) = ltdStatus ?~ (tds s)

  in paginate (foldr withFilter listTaskDefinitions filters)
     =$= CL.concatMap (view ltdrsTaskDefinitionARNs)
     =$= CL.map TaskDefRef
     =$= CL.mapMaybeM (\x -> runMaybeT (getTaskDef x))

-- Services

serviceCoords :: ContainerService -> Maybe ServiceCoords
serviceCoords service = ServiceCoords <$> serviceRef <*> clusterRef
  where serviceRef = ServiceRef <$> service ^. csServiceARN
        clusterRef = ClusterRef <$> service ^. csClusterARN

fetchServiceBatch :: MonadAWS m => [ServiceRef] -> ClusterRef -> m [ContainerService]
fetchServiceBatch []       _    = return []
fetchServiceBatch services cref =
  let handleCNFException = throwM $ clusterNotFound cref

      fetch :: MonadAWS m => m [ContainerService]
      fetch = do
        res <- send $ dCluster ?~ (toText cref) $ dServices .~ (toText <$> services) $ describeServices
        return $ res ^. dssrsServices
  in catching _ClusterNotFoundException fetch $ \_ -> handleCNFException

fetchServicesC' :: MonadAWS m => [ServiceRef] -> Conduit ClusterRef m (ClusterRef, ContainerService)
fetchServicesC' services =
  awaitForever (\cref -> yieldM $ do
    servs <- fetchServiceBatch services cref
    return $ map ((,) cref) servs
  ) =$= CL.concat

fetchServicesC :: MonadAWS m => [ServiceRef] -> Conduit ClusterRef m ContainerService
fetchServicesC services = fetchServicesC' services =$= CL.map snd

fetchAllServicesC :: MonadAWS m => Conduit ClusterRef m ContainerService
fetchAllServicesC = awaitForever (\cref -> yieldM . sourceToList $ fetchServices cref) =$= CL.concat

findServices' :: MonadAWS m => [ServiceRef] -> Maybe ClusterRef -> Source m (ClusterRef, ContainerService)
findServices' services (Just clusterRef) =
  yield clusterRef =$= fetchServicesC' services
findServices' services _ =
  fetchClusters =$= CL.mapMaybe clusterName =$= fetchServicesC' services

findServices :: MonadAWS m => [ServiceRef] -> Maybe ClusterRef -> Source m ContainerService
findServices services cref = findServices' services cref =$= CL.map snd

findService :: MonadAWS m => ServiceRef -> Maybe ClusterRef -> MaybeT m ContainerService
findService sref cref = MaybeT $ extractRequested cref
  where extractRequested (Just ref) =
          runConduit $ findServices' [sref] cref =$= CL.map snd =$= CL.head
        extractRequested _ = do
          found <- sourceToList $ findServices' [sref] Nothing
                   =$= CL.filter (\x -> matches (ServiceRefFilter sref) (snd x))
          if (length found) > 1
          then throwM $ ambiguousServiceName sref (fst <$> found)
          else return . listToMaybe $ snd <$> found

getService :: MonadAWS m => ServiceRef -> Maybe ClusterRef -> m ContainerService
getService serviceName clusterRef = do
  serv <- runMaybeT $ findService serviceName clusterRef
  case serv of
    Just x  -> return x
    Nothing -> throwM $ serviceNotFound serviceName clusterRef

fetchServices :: MonadAWS m => ClusterRef -> Source m ContainerService
fetchServices clusterRef =
  paginate (lsCluster ?~ (toText clusterRef) $ listServices)
    =$= CL.concatMapM (\x -> fetchServiceBatch (ServiceRef <$> x ^. lsrsServiceARNs) clusterRef)

fetchAllServices :: MonadAWS m => Source m ContainerService
fetchAllServices = fetchClusters
  =$= CL.mapMaybe clusterName
  =$= fetchAllServicesC

serviceEventLog :: MonadAWS m
                => ServiceCoords
                -> Bool
                -> Source m ServiceEvent
serviceEventLog (ServiceCoords serviceRef clusterRef) inf = yield Nothing =$= loop
  where serviceEvents :: MonadAWS m => Maybe UTCTime -> m [ServiceEvent]
        serviceEvents lastEventTime = do
          service  <- getService serviceRef (Just clusterRef)
          service' <- if (matches isActiveService service)
            then return service
            else throwM $ inactiveService serviceRef clusterRef
          events  <- return $ service' ^. csEvents
          return $ case lastEventTime of
            Nothing -> events
            Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. seCreatedAt) events
    
        fetch :: MonadAWS m => Maybe UTCTime -> m ([ServiceEvent], Maybe UTCTime)
        fetch lastEventTime = do
          events <- serviceEvents lastEventTime
          return $ (events, listToMaybe events >>= view seCreatedAt)

        loop :: MonadAWS m => Conduit (Maybe UTCTime) m ServiceEvent
        loop = do
          prev <- await
          case prev of
            Nothing            -> return ()
            Just lastEventTime -> do
              (events, nextTime) <- lift $ fetch lastEventTime
              CL.sourceList $ reverse events
              if inf then do
                leftover $ nextTime <|> lastEventTime
                liftIO $ threadDelay 1000000
                loop
              else return ()
