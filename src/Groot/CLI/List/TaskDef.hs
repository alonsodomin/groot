{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.TaskDef
     ( printTaskDefsSummary
     , TaskDefFilter(..)
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
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
import           Groot.Types

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

printTaskDefsSummary :: [TaskDefFilter] -> GrootM IO ()
printTaskDefsSummary filters = do
  env <- ask
  xs <- runResourceT . runAWS env $ summarizeTaskDefs filters
  liftIO $ printTable' "No task definitions found" xs
