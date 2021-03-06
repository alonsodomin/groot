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
import qualified Data.HashMap.Strict       as Map
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
  , memory       :: Maybe ResourceUsage
  , cpu          :: Maybe ResourceUsage
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ClusterSummary Tabs.ExpandWhenNested

data ClusterDetails = ClusterDetails ECS.Cluster (Maybe ResourceUsage) (Maybe ResourceUsage)

instance HasSummary ClusterDetails ClusterSummary where
  summarize (ClusterDetails cls mem cpu) = mkClusterSummary <$> cName <*> cStatus <*> cRunning <*> cPending <*> cInstances
     where mkClusterSummary name status running pending instances =
              ClusterSummary name status running pending instances mem cpu
           
           cName      = T.unpack <$> cls ^. ECS.cClusterName
           cStatus    = T.unpack <$> cls ^. ECS.cStatus
           cRunning   = cls ^. ECS.cRunningTasksCount
           cPending   = cls ^. ECS.cPendingTasksCount
           cInstances = cls ^. ECS.cRegisteredContainerInstancesCount

clusterDetails :: ECS.Cluster -> AWS ClusterDetails
clusterDetails cluster =
  let clusterResources = clusterResourceSummary allResourceTypes cluster
      memory = (Map.lookup Memory) <$> clusterResources
      cpu    = (Map.lookup CPU)    <$> clusterResources
  in (ClusterDetails cluster) <$> memory <*> cpu

summarizeClusters :: Maybe ClusterRef -> AWS [ClusterSummary]
summarizeClusters Nothing  = runConduit $ fetchClusters .| CL.mapM clusterDetails .| CL.mapMaybe summarize .| CL.consume
summarizeClusters (Just c) = maybeToList <$> do
  cl      <- runMaybeT (findCluster c)
  details <- sequence $ clusterDetails <$> cl
  return $ details >>= summarize

printClusterSummary :: Maybe ClusterRef -> GrootIO ()
printClusterSummary x = useResource $ do
  desc <- awsResource $ summarizeClusters x
  case desc of
    [] -> putInfo ("No clusters found" :: Text)
    xs -> liftIO $ printTable xs
