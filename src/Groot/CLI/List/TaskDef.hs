{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.TaskDef
     ( printTaskDefsSummary
     , TaskDefFilter(..)
     ) where

import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Text                 hiding (foldr)
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Text.PrettyPrint.Tabulate

import           Groot.CLI.List.Common
import           Groot.Core
import           Groot.Data

data TaskDefSummary = TaskDefSummary
  { family   :: String
  , revision :: Int
  , status   :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskDefSummary

instance HasSummary ECS.TaskDefinition TaskDefSummary where
  summarize taskDef = TaskDefSummary <$> tFamily <*> tRev <*> tStatus
    where tFamily = unpack <$> taskDef ^. ECS.tdFamily
          tRev    = taskDef ^. ECS.tdRevision
          tStatus = statusAsText <$> taskDef ^. ECS.tdStatus
            where statusAsText ECS.TDSActive   = "Active"
                  statusAsText ECS.TDSInactive = "Inactive"

summarizeTaskDefs :: [TaskDefFilter] -> AWS [TaskDefSummary]
summarizeTaskDefs filters =
  runConduit $ (fetchTaskDefs filters)
     =$= CL.mapMaybe summarize
     =$ CL.consume

printTaskDefsSummary :: [TaskDefFilter] -> Env -> IO ()
printTaskDefsSummary filters env = do
  xs <- runResourceT . runAWS env $ summarizeTaskDefs filters
  printTable' "No task definitions found" xs
