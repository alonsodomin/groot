{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Compose where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.List.NonEmpty        as NEL
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS

import           Groot.Core
import           Groot.Types

data Protocol = TCP | UDP
  deriving (Eq, Show, Ord, Enum, Bounded, Read, Generic)

defaultProtocol :: Protocol
defaultProtocol = TCP

instance FromJSON Protocol where
  parseJSON = withText "protocol" $ \str ->
    case str of
      "TCP" -> return TCP
      "UDP" -> return UDP
      _     -> fail $ "Invalid protocol: " ++ (T.unpack str)

data PortMapping = PortMapping
  { _pmContainerPort :: Int
  , _pmHostPort      :: Maybe Int
  , _pmProtocol      :: Protocol
  } deriving (Eq, Show, Generic)

makeLenses ''PortMapping

instance FromJSON PortMapping where
  parseJSON = withObject "port mapping" $ \o -> do
    _pmContainerPort <- o .: "container-port"
    _pmHostPort      <- o .:? "host-port"
    _pmProtocol      <- maybe defaultProtocol id <$> o .:? "protocol"
    return PortMapping{..}

data ContainerLogConfig = ContainerLogConfig
  { _clcDriver  :: Text
  , _clcOptions :: HashMap Text String
  } deriving (Eq, Show, Generic)

instance FromJSON ContainerLogConfig where
  parseJSON = withObject "log configuration" $ \o -> do
    _clcDriver  <- o .: "driver"
    _clcOptions <- o .: "options"
    return ContainerLogConfig{..}

data Container = Container
  { _cName         :: Text
  , _cImage        :: Text
  , _cMemory       :: Maybe Int
  , _cCpu          :: Maybe Int
  , _cPortMappings :: [PortMapping]
  , _cEnvironment  :: HashMap Text String
  , _cLogConfig    :: Maybe ContainerLogConfig
  } deriving (Eq, Show, Generic)

instance FromJSON Container where
  parseJSON = withObject "container" $ \o -> do
    _cName         <- T.pack <$> o .: "name"
    _cImage        <- T.pack <$> o .: "image"
    _cMemory       <- o .:? "memory"
    _cCpu          <- o .:? "cpu"
    _cPortMappings <- maybe [] id <$> o .:? "port-mappings"
    _cEnvironment  <- maybe Map.empty id <$> o .:? "environment"
    _cLogConfig    <- o .:? "logging"
    return Container{..}

data TaskDetails = TaskDetails
  { _tdName       :: Text
  , _tdRole       :: Text
  , _tdContainers :: [Container]
  } deriving (Eq, Show, Generic)

makeLenses ''TaskDetails

instance FromJSON TaskDetails where
  parseJSON = withObject "task definition" $ \o -> do
    _tdName       <- T.pack <$> o .: "name"
    _tdRole       <- T.pack <$> o .: "role"
    _tdContainers <- o .: "containers"
    return TaskDetails{..}

data DeploymentStrategy =
    DSBlueGreen
  | DSRolling
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

defaultDeploymentStrategy :: DeploymentStrategy
defaultDeploymentStrategy = DSBlueGreen

instance FromJSON DeploymentStrategy where
  parseJSON = withText "deployment strategy" $ \txt ->
    case txt of
      "blue-green" -> return DSBlueGreen
      "rolling"    -> return DSRolling
      _            -> fail $ "Invalid deployment strategy: " ++ (T.unpack txt)

data ServiceDetails = ServiceDetails
  { _sdName               :: Text
  , _sdRole               :: Text
  , _sdDesiredCount       :: Int
  , _sdDeploymentStrategy :: DeploymentStrategy
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceDetails

instance FromJSON ServiceDetails where
  parseJSON = withObject "service definition" $ \o -> do
    _sdName               <- o .: "name"
    _sdRole               <- o .: "role"
    _sdDesiredCount       <- o .: "desired"
    _sdDeploymentStrategy <- maybe defaultDeploymentStrategy id <$> o .:? "deployment-strategy"
    return ServiceDetails{..}

data Deployment = Deployment
  { _dTaskDetails    :: TaskDetails
  , _dServiceDetails :: ServiceDetails
  } deriving (Eq, Show, Generic)

makeLenses ''Deployment

instance FromJSON Deployment where
  parseJSON = withObject "deployment" $ \o -> do
    _dTaskDetails    <- o .: "task"
    _dServiceDetails <- o .: "service"
    return Deployment{..}

data GrootCompose = GrootCompose [Deployment] deriving (Eq, Show, Generic)

instance FromJSON GrootCompose where
  parseJSON = genericParseJSON defaultOptions {
                 fieldLabelModifier = drop 1 }

-- Validates that the given id points to an active cluster
findActiveCluster :: ClusterRef -> MaybeT AWS ECS.Cluster
findActiveCluster clusterRef = undefined
--  filterM (ClusterStatusFilter ClusterActive) (findCluster clusterRef)

findActiveService :: ContainerServiceRef -> ClusterRef -> MaybeT AWS ECS.ContainerService
findActiveService serviceRef clusterRef = undefined
--  filterM (ServiceStatusFilter ServiceActive) (getService serviceRef (Just clusterRef))

grootDeploy :: GrootCompose -> NEL.NonEmpty Text -> AWS ()
grootDeploy = undefined
