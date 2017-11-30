{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.ContainerService
     ( printServiceSummary
     ) where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Text                 (unpack)
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Text.PrettyPrint.Tabulate

import           Groot.CLI.List.Common
import           Groot.Core
import           Groot.Data

data ServiceSummary = ServiceSummary
  { name    :: String
  , cluster :: String
  , running :: Int
  , pending :: Int
  , desired :: Int
  } deriving (Eq, Show, Generic, Data)

instance Tabulate ServiceSummary

data ServiceAndRelatives = SR ECS.ContainerService ECS.Cluster

instance HasSummary ServiceAndRelatives ServiceSummary where
  summarize (SR service cluster) = ServiceSummary <$> sName <*> sClusterName <*> sRunning <*> sPending <*> sDesired
    where sName        = unpack <$> service ^. ECS.csServiceName
          sClusterName = unpack <$> cluster ^. ECS.cClusterName
          sRunning     = service ^. ECS.csRunningCount
          sPending     = service ^. ECS.csPendingCount
          sDesired     = service ^. ECS.csDesiredCount

annotateService :: MonadAWS m => Conduit ECS.ContainerService m ServiceAndRelatives
annotateService = CL.mapMaybeM (\s -> runMaybeT $
    (SR s) <$> serviceCluster s
  )

summarizeServices :: Maybe ClusterRef -> AWS [ServiceSummary]
summarizeServices clusterId =
  sourceToList $ serviceSource clusterId =$= annotateService =$= CL.mapMaybe summarize
    where serviceSource Nothing    = fetchAllServices
          serviceSource (Just cid) = fetchServices cid

printServiceSummary :: Maybe ClusterRef -> Env -> IO ()
printServiceSummary clusterId env = do
  xs <- runResourceT . runAWS env $ summarizeServices clusterId
  printTable' "No services found" xs
