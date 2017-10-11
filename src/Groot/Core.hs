module Groot.Core where

import Control.Lens
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text hiding (foldr, takeWhile)
import Data.Time.Clock
import Network.AWS hiding (await)
import Network.AWS.ECS

import Groot.Data

data GrootError = ServiceNotFound Text ClusterId

type GrootAction = ExceptT GrootError AWS

runActionM :: Env -> GrootAction a -> (a -> IO b) -> IO b
runActionM env action success = do
  result <- runResourceT . runAWS env . runExceptT $ action
  case result of
    Left (ServiceNotFound serviceName (ClusterId clusterId)) ->
      fail $ "Could not find service '" ++ (unpack serviceName) ++ "' in cluster " ++ (unpack clusterId)
    Right a -> success a

runAction :: Env -> GrootAction a -> (a -> b) -> IO b
runAction env action success = runActionM env action (\x -> return $ success x)

-- Clusters

getCluster :: ClusterId -> MaybeT AWS Cluster
getCluster (ClusterId clusterId) = MaybeT $ do
  res <- send $ dcClusters .~ [clusterId] $ describeClusters
  return $ listToMaybe (res ^. dcrsClusters)

fetchClusters :: Source AWS Cluster
fetchClusters = 
  let getClusterBatch batch = do
        res <- send $ dcClusters .~ batch $ describeClusters
        return $ res ^. dcrsClusters
  in paginate listClusters
      =$= CL.concatMapM (\x -> getClusterBatch (x ^. lcrsClusterARNs))

-- Container Instances

getInstance :: ClusterId -> InstanceARN -> MaybeT AWS ContainerInstance
getInstance (ClusterId cId) (InstanceARN iArn) = MaybeT $ do
  res <- send $ dciCluster ?~ cId $ dciContainerInstances .~ [iArn] $ describeContainerInstances
  return $ listToMaybe (res ^. dcisrsContainerInstances)

fetchInstances :: ClusterId -> Source AWS ContainerInstance
fetchInstances (ClusterId cId) =
  let getInstanceBatch batch = do
        res <- send $ dciCluster ?~ cId $ dciContainerInstances .~ batch $ describeContainerInstances
        return $ res ^. dcisrsContainerInstances
  in paginate (lciCluster ?~ cId $ listContainerInstances)
     =$= CL.concatMapM (\x -> getInstanceBatch (view lcirsContainerInstanceARNs x))

fetchAllInstances :: Source AWS ContainerInstance
fetchAllInstances =
  let fetchInstancesC :: Conduit ClusterId AWS [ContainerInstance]
      fetchInstancesC =
        awaitForever (\x -> yieldM $ sourceToList $ fetchInstances x)
  in fetchClusters
     =$= CL.mapMaybe (\x -> ClusterId <$> x ^. cClusterName)
     =$= fetchInstancesC
     =$= CL.concat

-- Tasks

getTask :: ClusterId -> TaskARN -> MaybeT AWS Task
getTask (ClusterId cId) (TaskARN tArn) = MaybeT $ do
  res <- send $ dtCluster ?~ cId $ dtTasks .~ [tArn] $ describeTasks
  return $ listToMaybe $ res ^. dtrsTasks

fetchTasks :: ClusterId -> Source AWS Task
fetchTasks (ClusterId cId) =
  let getTaskBatch batch = do
        res <- send $ dtCluster ?~ cId $ dtTasks .~ batch $ describeTasks
        return $ res ^. dtrsTasks
  in paginate (ltCluster ?~ cId $ listTasks)
     =$= CL.concatMapM (\x -> getTaskBatch (view ltrsTaskARNs x))

fetchAllTasks :: Source AWS Task
fetchAllTasks =
  let fetchTasksC :: Conduit ClusterId AWS [Task]
      fetchTasksC =
        awaitForever (\x -> yieldM $ sourceToList $ fetchTasks x)
  in fetchClusters
     =$= CL.mapMaybe (\x -> ClusterId <$> x ^. cClusterName)
     =$= fetchTasksC
     =$= CL.concat
    
-- Task Definitions

getTaskDef :: TaskDefARN -> MaybeT AWS TaskDefinition
getTaskDef (TaskDefARN arn) = MaybeT $ do
  res <- send $ describeTaskDefinition arn
  return $ res ^. desrsTaskDefinition

taskDefFromTask :: Task -> MaybeT AWS TaskDefinition
taskDefFromTask tsk = do
  arn <- MaybeT . return $ TaskDefARN <$> tsk ^. tTaskDefinitionARN
  getTaskDef arn

fetchTaskDefs :: Foldable f => f TaskDefFilter -> Source AWS TaskDefinition
fetchTaskDefs filters =
  let tds :: TaskDefStatus -> TaskDefinitionStatus
      tds TaskActive   = TDSActive
      tds TaskInactive = TDSInactive

      withFilter :: TaskDefFilter -> ListTaskDefinitions -> ListTaskDefinitions
      withFilter (FamilyFilter (TaskFamily f)) = ltdFamilyPrefix ?~ f
      withFilter (StatusFilter s) = ltdStatus ?~ (tds s)

  in paginate (foldr withFilter listTaskDefinitions filters)
     =$= CL.concatMap (view ltdrsTaskDefinitionARNs)
     =$= CL.map TaskDefARN
     =$= CL.mapMaybeM (\x -> runMaybeT (getTaskDef x))

-- Services

getService :: Text -> ClusterId -> MaybeT AWS ContainerService
getService serviceName (ClusterId clusterId) = MaybeT $ do
  res <- send $ dCluster ?~ clusterId $ dServices .~ [serviceName] $ describeServices
  return $ listToMaybe (res ^. dssrsServices)

fetchServices :: ClusterId -> Source AWS ContainerService
fetchServices (ClusterId clusterId) =
  let getServiceBatch batch = do
        res <- send $ dCluster ?~ clusterId $ dServices .~ batch $ describeServices
        return $ res ^. dssrsServices
  in paginate (lsCluster ?~ clusterId $ listServices)
     =$= CL.concatMapM (\x -> getServiceBatch (view lsrsServiceARNs x))

fetchAllServices :: Source AWS ContainerService
fetchAllServices =
  let fetchServicesC :: Conduit ClusterId AWS [ContainerService]
      fetchServicesC =
        awaitForever (\x -> yieldM $ sourceToList $ fetchServices x)
  in fetchClusters
     =$= CL.mapMaybe (\x -> ClusterId <$> x ^. cClusterName)
     =$= fetchServicesC
     =$= CL.concat

serviceEvents :: Text -> ClusterId -> Maybe UTCTime -> GrootAction [ServiceEvent]
serviceEvents serviceName clusterId lastEventTime = do
  maybeEvts <- liftAWS $ runMaybeT $ do
    service <- getService serviceName clusterId
    return $ service ^. csEvents
  evts <- case maybeEvts of
    Nothing -> throwError $ ServiceNotFound serviceName clusterId
    Just x  -> return x
  return $ case lastEventTime of
    Nothing -> evts
    Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. seCreatedAt) $ evts

serviceEventLog :: Env -> Text -> ClusterId -> Source IO ServiceEvent
serviceEventLog env serviceName clusterId =
  let events :: Maybe UTCTime -> AWS [ServiceEvent] 
      events lastEvtTime = do
        maybeEvts <- runMaybeT $ do
          service <- getService serviceName clusterId
          return $ service ^. csEvents
        evts <- return $ maybe [] id maybeEvts
        return $ case lastEvtTime of
          Nothing -> maybeToList . listToMaybe $ evts
          Just t  -> takeWhile (\ev -> maybe False (< t) $ ev ^. seCreatedAt) $ evts
      
      lastEventTime :: [ServiceEvent] -> Maybe UTCTime
      lastEventTime evts = listToMaybe evts >>= view seCreatedAt

      loop :: Maybe UTCTime -> Source IO ([ServiceEvent], Maybe UTCTime)
      loop lastEvtTime = do
        yieldM $ do
          evts     <- runResourceT . runAWS env $ events lastEvtTime
          nextTime <- return $ lastEventTime evts
          return (evts, nextTime)
        
  in loop Nothing =$= CL.map fst =$= CL.concat