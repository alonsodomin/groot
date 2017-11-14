{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Task
     ( printTaskSummary
     ) where

import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import qualified Data.Text as T
import GHC.Generics
import Text.PrettyPrint.Tabulate
import Network.AWS
import Network.AWS.Data.Text
import qualified Network.AWS.ECS as ECS

import Groot.App.List.Base
import Groot.Core
import Groot.Data

data TaskSummary = TaskSummary
  { task       :: String
  , status     :: String
  , clusterArn :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskSummary

newtype TaskAndDef = TaskAndDef (ECS.Task, ECS.TaskDefinition)

instance HasSummary TaskAndDef TaskSummary where
  summarize (TaskAndDef (task, td)) = TaskSummary <$> tTaskDef <*> tStatus <*> tClusterArn
    where tFamily     = td ^. ECS.tdFamily
          tRevision   = toText <$> td ^. ECS.tdRevision
          tTaskDef    = T.unpack <$> ((\x y -> T.concat [x, ":", y]) <$> tFamily <*> tRevision)
          tStatus     = T.unpack <$> task ^. ECS.tLastStatus
          tClusterArn = T.unpack <$> task ^. ECS.tClusterARN

annotateWithTaskDef :: MonadAWS m => Conduit ECS.Task m TaskAndDef
annotateWithTaskDef = CL.mapMaybeM (\t -> runMaybeT $ fmap (TaskAndDef . ((,) t)) $ taskDefFromTask t)

summarizeTasks :: Maybe ClusterRef -> AWS [TaskSummary]
summarizeTasks mcid = sourceToList $ taskSource mcid =$= annotateWithTaskDef =$= CL.mapMaybe summarize
  where taskSource Nothing    = fetchAllTasks
        taskSource (Just cid) = fetchTasks cid

printTaskSummary :: Maybe ClusterRef -> Env -> IO ()
printTaskSummary cId env = do
  xs <- runResourceT . runAWS env $ summarizeTasks cId
  printTable' "No tasks found" xs