module Groot.Core where

import Control.Lens
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (listToMaybe, maybeToList)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Network.AWS hiding (await)
import Network.AWS.ECS hiding (cluster)

import Groot.Data

data GrootError =
    ServiceNotFound Text (Maybe ClusterId)
  | AmbiguousServiceName Text [ClusterId]

type GrootAction = ExceptT GrootError AWS

runActionM :: Env -> GrootAction a -> (a -> IO b) -> IO b
runActionM env action success = do
  result <- runResourceT . runAWS env . runExceptT $ action
  case result of
    Left (ServiceNotFound serviceName cid) ->
      fail $ "Could not find service '" ++ (T.unpack serviceName) ++ "'" ++
           maybe "" (\x -> " in cluster " ++ (show x)) cid
    Left (AmbiguousServiceName serviceName clusters) ->
      fail $ "Service name '" ++ (T.unpack serviceName) ++ "' is ambiguous. It was found in the following clusters:\n" ++ (clusterList clusters)
      where clusterList cls = intercalate "\n  - " $ map show cls
    Right a -> success a

runAction :: Env -> GrootAction a -> (a -> b) -> IO b
runAction env action success = runActionM env action (\x -> return $ success x)

-- Clusters

clusterId :: Cluster -> Maybe ClusterId
clusterId cluster = ClusterId <$> cluster ^. cClusterName

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
     =$= CL.mapMaybe clusterId
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
     =$= CL.mapMaybe clusterId
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

findService :: Text -> Maybe ClusterId -> GrootAction ContainerService
findService serviceName (Just cid) = do
  found <- liftAWS . runMaybeT $ getService serviceName cid
  maybe (throwError $ ServiceNotFound serviceName (Just cid)) return found
findService serviceName _ = do
  found <- liftAWS . sourceToList $ fetchClusters
           =$= CL.mapMaybe clusterId
           =$= CL.mapMaybeM (\x -> runMaybeT $ fmap (\y -> (y,x)) $ getService serviceName x)
  if (length found) > 1
  then throwError $ AmbiguousServiceName serviceName (map snd found)
  else maybe (throwError $ ServiceNotFound serviceName Nothing) (return . fst) $ listToMaybe found
  
serviceEvents :: ContainerService -> Maybe UTCTime -> GrootAction [ServiceEvent]
serviceEvents service lastEventTime = do
  events <- return $ service ^. csEvents
  return $ case lastEventTime of
    Nothing -> events
    Just t  -> takeWhile (\ev -> maybe False (> t) $ ev ^. seCreatedAt) $ events

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