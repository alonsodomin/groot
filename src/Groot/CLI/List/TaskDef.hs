{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.TaskDef
     ( ListTaskDefsOpts
     , listTaskDefsOpts
     , printTaskDefsSummary
     , TaskDefFilterPart(..)
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Maybe                (maybeToList)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Options.Applicative
import           Text.PrettyPrint.Tabulate (Tabulate, printTable)
import qualified Text.PrettyPrint.Tabulate as Tabs

import           Groot.CLI.Common
import           Groot.CLI.List.Common
import           Groot.Console
import           Groot.Core
import           Groot.Types

data ListTaskDefsOpts =
  ListTaskDefsOpts Bool (Maybe TaskFamily)
  deriving (Eq, Show)

listTaskDefsOpts :: Parser ListTaskDefsOpts
listTaskDefsOpts = ListTaskDefsOpts
              <$> switch
                ( long "inactive"
                <> short 'i'
                <> help "Show inactive task definitions" )
              <*> optional taskFamilyOpt

data TaskDefSummary = TaskDefSummary
  { family   :: String
  , revision :: Int
  , status   :: String
  } deriving (Eq, Show, Generic, Data)

instance Tabulate TaskDefSummary Tabs.ExpandWhenNested

instance HasSummary ECS.TaskDefinition TaskDefSummary where
  summarize taskDef = TaskDefSummary <$> tFamily <*> tRev <*> tStatus
    where tFamily = T.unpack <$> taskDef ^. ECS.tdFamily
          tRev    = taskDef ^. ECS.tdRevision
          tStatus = statusAsText <$> taskDef ^. ECS.tdStatus
            where statusAsText ECS.TDSActive   = "Active"
                  statusAsText ECS.TDSInactive = "Inactive"

summarizeTaskDefs :: [TaskDefFilterPart] -> AWS [TaskDefSummary]
summarizeTaskDefs filters =
  runConduit $ (fetchTaskDefs filters)
     .| CL.mapMaybe summarize
     .| CL.consume

printTaskDefsSummary :: ListTaskDefsOpts -> GrootIO ()
printTaskDefsSummary (ListTaskDefsOpts showInactive fam) =
  let statusFilter = if showInactive then [TDFStatus TDSInactive] else []
      familyFilter = maybeToList $ TDFFamily <$> fam
      filters      = statusFilter ++ familyFilter
  in runGrootResource $ do
    desc <- awsResource $ summarizeTaskDefs filters
    case desc of
      [] -> putWarn ("No task definitions found" :: Text)
      xs -> liftIO $ printTable xs
