{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Task
     ( printTaskSummary
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

data TaskSummary = TaskSummary
  { arn        :: String
  , status     :: String
  , clusterArn :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskSummary

instance HasSummary ECS.Task TaskSummary where
  summarize task = TaskSummary <$> tArn <*> tStatus <*> tClusterArn
    where tArn        = unpack <$> task ^. ECS.tTaskARN
          tStatus     = unpack <$> task ^. ECS.tLastStatus
          tClusterArn = unpack <$> task ^. ECS.tClusterARN

summarizeTasks :: Maybe ClusterRef -> AWS [TaskSummary]
summarizeTasks mcid = sourceToList $ taskSource mcid =$= CL.mapMaybe summarize
  where taskSource Nothing    = fetchAllTasks
        taskSource (Just cid) = fetchTasks cid

printTaskSummary :: Maybe ClusterRef -> Env -> IO ()
printTaskSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeTasks cId
  printTable' "No tasks found" xs