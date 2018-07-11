{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.ContainerService
     ( printServiceSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
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
import           Groot.Internal.Data.Text
import           Groot.Types

data ServiceSummary = ServiceSummary
  { name    :: String
  , cluster :: String
  , task    :: String
  , running :: Int
  , pending :: Int
  , desired :: Int
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ServiceSummary Tabs.ExpandWhenNested

data ServiceAndRelatives = SR ECS.ContainerService ECS.Cluster

instance HasSummary ServiceAndRelatives ServiceSummary where
  summarize (SR service cluster) = ServiceSummary <$> sName <*> sClusterName <*> sTask <*> sRunning <*> sPending <*> sDesired
    where sName        = T.unpack <$> service ^. ECS.csServiceName
          sClusterName = T.unpack <$> cluster ^. ECS.cClusterName
          sTask        = T.unpack . toText . (view arnTaskDefId) <$> serviceTaskDefArn service
          sRunning     = service ^. ECS.csRunningCount
          sPending     = service ^. ECS.csPendingCount
          sDesired     = service ^. ECS.csDesiredCount

annotateService :: MonadAWS m => ConduitT ECS.ContainerService ServiceAndRelatives m ()
annotateService = CL.mapMaybeM (\s -> runMaybeT $
    (SR s) <$> serviceCluster s
  )

summarizeServices :: Maybe ClusterRef -> AWS [ServiceSummary]
summarizeServices clusterId =
  sourceToList $ serviceSource clusterId .| annotateService .| CL.mapMaybe summarize
    where serviceSource Nothing    = fetchAllServices
          serviceSource (Just cid) = fetchServices cid

printServiceSummary :: Maybe ClusterRef -> GrootIO ()
printServiceSummary clusterId = runGrootResource $ do
  desc <- awsResource $ summarizeServices clusterId
  case desc of
    [] -> putWarn ("No services found" :: Text)
    xs -> liftIO $ printTable xs
