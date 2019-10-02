{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.ContainerInstance
     ( printInstanceSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Text.PrettyPrint.Tabulate (Tabulate, printTable)
import qualified Text.PrettyPrint.Tabulate as Tabs

import           Groot.CLI.List.Common
import           Groot.Console
import           Groot.Core
import           Groot.Types

data InstanceSummary = InstanceSummary
  { instanceId    :: String
  , ec2InstaceId  :: String
  , status        :: String
  , runningTasks  :: Int
  , pendingTasks  :: Int
  , memory        :: ResourceUsage
  , cpu           :: ResourceUsage
  , agentVersion  :: String
  , dockerVersion :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate InstanceSummary Tabs.ExpandWhenNested

instance HasSummary ECS.ContainerInstance InstanceSummary where
  summarize inst = InstanceSummary <$> iId <*> iEc2Id <*> iStatus <*> iRunning <*> iPending <*> iMem <*> iCpu <*> iAgentV <*> iDockerV
    where iId      = (asString . view arnContainerInstanceId) <$> viewArn (ECS.ciContainerInstanceARN . _Just) inst
          iEc2Id   = T.unpack <$> inst ^. ECS.ciEc2InstanceId
          iStatus  = T.unpack <$> inst ^. ECS.ciStatus
          iRunning = inst ^. ECS.ciRunningTasksCount
          iPending = inst ^. ECS.ciPendingTasksCount
          iMem     = instanceResourceUsage Memory inst
          iCpu     = instanceResourceUsage CPU inst
          iAgentV  = T.unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viAgentVersion)
          iDockerV = T.unpack <$> (inst ^. ECS.ciVersionInfo >>= view ECS.viDockerVersion)

summarizeInstances :: Maybe ClusterRef -> AWS [InstanceSummary]
summarizeInstances cId =
  sourceToList $ instanceSource cId .| CL.mapMaybe summarize
  where instanceSource Nothing  = fetchAllInstances
        instanceSource (Just x) = fetchInstances x

printInstanceSummary :: Maybe ClusterRef -> GrootIO ()
printInstanceSummary cId = useResource $ do
  desc <- awsResource $ summarizeInstances cId
  case desc of
    [] -> putWarn ("No container instances found" :: Text)
    xs -> liftIO $ printTable xs
