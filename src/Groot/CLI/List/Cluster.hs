{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.Cluster
     ( printClusterSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Maybe                (maybeToList)
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

data ClusterSummary = ClusterSummary
  { name         :: String
  , status       :: String
  , runningTasks :: Int
  , pendingTasks :: Int
  , instances    :: Int
  , memory       :: ResourceSummary
  , cpu          :: ResourceSummary
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ClusterSummary Tabs.ExpandWhenNested

data ClusterDetails = ClusterDetails ECS.Cluster ResourceSummary ResourceSummary

instance HasSummary ClusterDetails ClusterSummary where
  summarize (ClusterDetails cls mem cpu) = ClusterSummary
       <$> cName <*> cStatus <*> cRunning <*> cPending <*> cInstances <*> (pure mem) <*> (pure cpu)
     where cName      = T.unpack <$> cls ^. ECS.cClusterName
           cStatus    = T.unpack <$> cls ^. ECS.cStatus
           cRunning   = cls ^. ECS.cRunningTasksCount
           cPending   = cls ^. ECS.cPendingTasksCount
           cInstances = cls ^. ECS.cRegisteredContainerInstancesCount

clusterMemory :: ECS.Cluster -> AWS ResourceSummary
clusterMemory = clusterResourceSummary Memory

clusterCpu :: ECS.Cluster -> AWS ResourceSummary
clusterCpu = clusterResourceSummary CPU

clusterDetails :: ECS.Cluster -> AWS ClusterDetails
clusterDetails cluster = (ClusterDetails cluster) <$> (clusterMemory cluster) <*> (clusterCpu cluster)

summarizeClusters :: Maybe ClusterRef -> AWS [ClusterSummary]
summarizeClusters Nothing  = runConduit $ fetchClusters .| CL.mapM clusterDetails .| CL.mapMaybe summarize .| CL.consume
summarizeClusters (Just c) = maybeToList <$> do
  cl      <- runMaybeT (findCluster c)
  details <- sequence $ clusterDetails <$> cl
  return $ details >>= summarize

printClusterSummary :: Maybe ClusterRef -> GrootIO ()
printClusterSummary x = runGrootResource $ do
  desc <- awsResource $ summarizeClusters x
  case desc of
    [] -> putWarn ("No clusters found" :: Text)
    xs -> liftIO $ printTable xs
