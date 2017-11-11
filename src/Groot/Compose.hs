{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Groot.Compose
     (
       GrootCompose (..)
     , GrootTaskDef (..)
     , GrootServiceDef (..)
     , GrootDeployment (..)
     , taskRole
     , serviceRole
     , taskDef
     , serviceDef
     , grootDeploy
     ) where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List (drop, find)
import qualified Data.List.NonEmpty as NEL
import Data.Text hiding (drop, find)
import qualified Data.Vector as V
import Network.AWS
import Network.AWS.ECS

import Groot.Core
import Groot.Data

data Protocol = TCP | UDP
  deriving (Eq, Show, Ord, Read, Generic)

data GrootPortMappings = GrootPortMappings
  { _containerPort :: Int
  , _hostPort :: Int
  , _portProtocol :: Protocol
  } deriving (Eq, Show, Generic)

data GrootContainerDef = GrootContainerDef
  { _containerName :: Text
  , _containerImage :: Text
  , _containerMemory :: Maybe Int
  , _containerCpu :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON GrootContainerDef where
  parseJSON (Object o) = do
    _containerName <- pack <$> o .: "name"
    _containerImage <- pack <$> o .: "image"
    _containerMemory <- o .:? "memory"
    _containerCpu <- o .:? "cpu"
    return GrootContainerDef{..}

data GrootTaskDef = GrootTaskDef
  { _taskName :: Text
  , _taskRole :: Text
  , _taskContainers :: [GrootContainerDef]
  } deriving (Eq, Show, Generic)

makeLenses ''GrootTaskDef

parseContainers :: Value -> Parser [GrootContainerDef]
parseContainers = withArray "containers" $ \arr -> mapM parseJSON (V.toList arr)

instance FromJSON GrootTaskDef where
  parseJSON (Object o) = do
    _taskName <- pack <$> o .: "name"
    _taskRole <- pack <$> o .: "role"
    _taskContainers <- o .: "containers" >>= parseContainers
    return GrootTaskDef{..}

data GrootServiceDef = GrootServiceDef
  { _serviceRole :: Text }
  deriving (Eq, Show, Generic)

makeLenses ''GrootServiceDef

instance FromJSON GrootServiceDef where
  parseJSON (Object o) = do
    _serviceRole <- o .: "role"
    return GrootServiceDef{..}

data GrootDeployment = GrootDeployment
  { _taskDef :: GrootTaskDef
  , _serviceDef :: GrootServiceDef
  } deriving (Eq, Show, Generic)

makeLenses ''GrootDeployment

instance FromJSON GrootDeployment where
  parseJSON (Object o) = do
    _taskDef    <- o .: "task"
    _serviceDef <- o .: "service"
    return GrootDeployment{..}

data GrootCompose = GrootCompose [GrootDeployment] deriving (Eq, Show, Generic)

instance FromJSON GrootCompose where
  parseJSON = genericParseJSON defaultOptions {
                 fieldLabelModifier = drop 1 }

-- Validates that the given id points to an active cluster
findActiveCluster :: ClusterRef -> MaybeT AWS Cluster
findActiveCluster clusterRef =
  filterM (ClusterStatusFilter ClusterActive) (findCluster clusterRef)

findActiveService :: ServiceRef -> ClusterRef -> MaybeT AWS ContainerService
findActiveService serviceRef clusterRef =
  filterM (ServiceStatusFilter ServiceActive) (getService serviceRef (Just clusterRef))
  
grootDeploy :: GrootCompose -> NEL.NonEmpty Text -> AWS ()
grootDeploy = undefined
