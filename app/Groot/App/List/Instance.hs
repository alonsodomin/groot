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
import Data.Text
import GHC.Generics
import Text.PrettyPrint.Tabulate
import Network.AWS
import qualified Network.AWS.ECS as ECS

import Groot.App.List.Base
import Groot.Core
import Groot.Data

data InstanceSummary = InstanceSummary
  { ec2Id           :: String
  , arn             :: String
  , status          :: String
  , runningTasks    :: Int
  , pendingTasks    :: Int
  , ecsAgentVersion :: String
  , dockerVersion   :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate InstanceSummary

instance HasSummary ECS.ContainerInstance InstanceSummary where
  summarize inst = InstanceSummary <$> iId <*> iArn <*> iStatus <*> iRunning <*> iPending <*> iAgentV <*> iDockerV
    where iId      = unpack <$> inst ^. ECS.ciEc2InstanceId
          iArn     = unpack <$> inst ^. ECS.ciContainerInstanceARN
          iStatus  = unpack <$> inst ^. ECS.ciStatus
          iRunning = inst ^. ECS.ciRunningTasksCount
          iPending = inst ^. ECS.ciPendingTasksCount
          iAgentV  = unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viAgentVersion)
          iDockerV = unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viDockerVersion)

summarizeInstances :: Maybe ClusterRef -> AWS [InstanceSummary]
summarizeInstances cId = 
  sourceToList $ instanceSource cId =$= CL.mapMaybe summarize
  where instanceSource Nothing  = fetchAllInstances
        instanceSource (Just x) = fetchInstances x

printInstanceSummary :: Maybe ClusterRef -> Env -> IO ()
printInstanceSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeInstances cId
  printTable' "No container instances found" xs