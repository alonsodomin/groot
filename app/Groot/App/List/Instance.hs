{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Instance
     ( printInstanceSummary
     ) where

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import Data.List
import qualified Data.Text as T
import GHC.Generics
import Network.AWS
import qualified Network.AWS.ECS as ECS
import Numeric
import Text.PrettyPrint.Tabulate

import Groot.App.List.Base
import Groot.Core
import Groot.Data
import Groot.Data.Text
import Groot.Data.Text.Lens
import Groot.Data.Text.PrettyPrint
import Groot.Types

data ResourceType =
    Memory
  | CPU
  deriving (Eq, Show, Generic, Data)

data ResourceSummary = ResourceSummary
  { resourceType :: ResourceType
  , allocated    :: Int
  , available    :: Int
  } deriving (Eq, Generic, Data)

instance Show ResourceSummary where
  show (ResourceSummary resType alloc avail) = concat [show avail, "/", show alloc, " ", units, " ", percent]
    where units = case resType of
            Memory -> "mb"
            CPU    -> "units"
          percent =
            let pvalue = (fromIntegral avail) / (fromIntegral alloc) * 100.0 :: Float
            in concat ["(", showFFloat (Just 1) pvalue "", " %)"]

instance CellValueFormatter ResourceSummary

resourceSummary :: ResourceType -> ECS.ContainerInstance -> Maybe ResourceSummary
resourceSummary resType inst = (ResourceSummary resType) <$> rAlloc <*> rAvail
  where resName = case resType of
          Memory -> "MEMORY"
          CPU    -> "CPU"
        
        findResource :: [ECS.Resource] -> Maybe Int
        findResource rs = (view ECS.rIntegerValue) =<< find (\x -> maybe False (== resName) $ x ^. ECS.rName) rs
        
        rAlloc = findResource $ inst ^. ECS.ciRegisteredResources
        rAvail = findResource $ inst ^. ECS.ciRemainingResources

data ContainerInstanceAttr =
    CIId
  | CIEC2Id
  | CIStatus
  | CIRunningTasks
  | CIPendingTasks
  | CIMemory
  | CICPU
  | CIAgentVersion
  | CIDockerVersion
  deriving (Eq, Show, Enum, Ord, Bounded)

instance PrettyColumn ContainerInstanceAttr where
  columnHeader CIId            = "INSTANCE ID"
  columnHeader CIEC2Id         = "EC2 ID"
  columnHeader CIStatus        = "STATUS"
  columnHeader CIRunningTasks  = "RUNNING TASKS"
  columnHeader CIPendingTasks  = "PENDING TASKS"
  columnHeader CIMemory        = "MEMORY"
  columnHeader CICPU           = "CPU"
  columnHeader CIAgentVersion  = "AGENT"
  columnHeader CIDockerVersion = "DOCKER"

  columnCell CIId = ECS.ciContainerInstanceARN . asArn . (arnContainerInstanceId . _Just)

data InstanceSummary = InstanceSummary
  { instanceId      :: String
  , ec2InstaceId    :: String
  , status          :: String
  , runningTasks    :: Int
  , pendingTasks    :: Int
  , memory          :: ResourceSummary
  , cpu             :: ResourceSummary
  , agentVersion    :: String
  , dockerVersion   :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate InstanceSummary

instance HasSummary ECS.ContainerInstance InstanceSummary where
  summarize inst = InstanceSummary <$> iId <*> iEc2Id <*> iStatus <*> iRunning <*> iPending <*> iMem <*> iCpu <*> iAgentV <*> iDockerV
    where iId      = (asString . view arnContainerInstanceId) <$> viewArn (ECS.ciContainerInstanceARN . _Just) inst
          iEc2Id   = T.unpack <$> inst ^. ECS.ciEc2InstanceId
          iStatus  = T.unpack <$> inst ^. ECS.ciStatus
          iRunning = inst ^. ECS.ciRunningTasksCount
          iPending = inst ^. ECS.ciPendingTasksCount
          iMem     = resourceSummary Memory inst
          iCpu     = resourceSummary CPU inst
          iAgentV  = T.unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viAgentVersion)
          iDockerV = T.unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viDockerVersion)

summarizeInstances :: Maybe ClusterRef -> AWS [InstanceSummary]
summarizeInstances cId = 
  sourceToList $ instanceSource cId =$= CL.mapMaybe summarize
  where instanceSource Nothing  = fetchAllInstances
        instanceSource (Just x) = fetchInstances x

printInstanceSummary :: Maybe ClusterRef -> Env -> IO ()
printInstanceSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeInstances cId
  printTable' "No container instances found" xs