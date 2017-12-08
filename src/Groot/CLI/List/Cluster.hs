{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.Cluster
     ( printClusterSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Maybe                (maybeToList)
import           Data.Text
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Text.PrettyPrint.Tabulate

import           Groot.CLI.List.Common
import           Groot.Core
import           Groot.Types

data ClusterSummary = ClusterSummary
  { name         :: String
  , status       :: String
  , runningTasks :: Int
  , pendingTasks :: Int
  , instances    :: Int
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ClusterSummary

instance HasSummary ECS.Cluster ClusterSummary where
  summarize cls = ClusterSummary <$> cName <*> cStatus <*> cRunning <*> cPending <*> cInstances
     where cName      = unpack <$> cls ^. ECS.cClusterName
           cStatus    = unpack <$> cls ^. ECS.cStatus
           cRunning   = cls ^. ECS.cRunningTasksCount
           cPending   = cls ^. ECS.cPendingTasksCount
           cInstances = cls ^. ECS.cRegisteredContainerInstancesCount

summarizeClusters :: Maybe ClusterRef -> AWS [ClusterSummary]
summarizeClusters Nothing  = runConduit $ fetchClusters =$= CL.mapMaybe summarize =$ CL.consume
summarizeClusters (Just c) = maybeToList <$> do
  cl <- runMaybeT (findCluster c)
  return $ cl >>= summarize

printClusterSummary :: Maybe ClusterRef -> GrootM IO ()
printClusterSummary x = do
  env <- ask
  xs <- runResourceT . runAWS env $ summarizeClusters x
  liftIO $ printTable' "No clusters found" xs
