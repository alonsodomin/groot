{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Service
     ( printServiceSummary
     ) where

import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import Data.Text (unpack)
import GHC.Generics
import Text.PrettyPrint.Tabulate
import Network.AWS
import qualified Network.AWS.ECS as ECS

import Groot.App.List.Base
import Groot.Core
import Groot.Data

data ServiceSummary = ServiceSummary
  { name       :: String
  , arn        :: String
  , clusterArn :: String
  , running    :: Int
  , pending    :: Int
  , desired    :: Int
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ServiceSummary

instance HasSummary ECS.ContainerService ServiceSummary where
  summarize service = ServiceSummary <$> sName <*> sArn <*> sClusterArn <*> sRunning <*> sPending <*> sDesired
    where sName       = unpack <$> service ^. ECS.csServiceName
          sArn        = unpack <$> service ^. ECS.csServiceARN
          sClusterArn = unpack <$> service ^. ECS.csClusterARN
          sRunning    = service ^. ECS.csRunningCount
          sPending    = service ^. ECS.csPendingCount
          sDesired    = service ^. ECS.csDesiredCount

summarizeServices :: Maybe ClusterRef -> AWS [ServiceSummary]
summarizeServices clusterId =
  sourceToList $ serviceSource clusterId =$= CL.mapMaybe summarize
    where serviceSource Nothing    = fetchAllServices
          serviceSource (Just cid) = fetchServices cid

printServiceSummary :: Maybe ClusterRef -> Env -> IO ()
printServiceSummary clusterId env = do
  xs <- runResourceT . runAWS env $ summarizeServices clusterId
  printTable' "No services found" xs