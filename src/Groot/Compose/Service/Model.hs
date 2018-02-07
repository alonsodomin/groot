{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Compose.Service.Model where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import qualified Network.AWS.ECS           as ECS

data PortELBLink =
    ELBNameLink Text
  | TargetGroupLink Text
  deriving (Eq, Show)

data PortMapping = PortMapping
  { _pmContainerPort :: Int
  , _pmHostPort      :: Maybe Int
  , _pmProtocol      :: Maybe ECS.TransportProtocol
  , _pmElbLink       :: Maybe PortELBLink
  } deriving (Eq, Show, Generic)

makeLenses ''PortMapping

instance FromJSON PortMapping where
  parseJSON = withObject "port mapping" $ \o -> do
    _pmContainerPort <- o .: "container-port"
    _pmHostPort      <- o .:? "host-port"
    _pmProtocol      <- o .:? "protocol"

    -- Load balancer link
    let nameLink        = ELBNameLink     <$> (MaybeT $ o .:? "lb-name")
    let targetGroupLink = TargetGroupLink <$> (MaybeT $ o .:? "target-group")
    _pmElbLink       <- runMaybeT $ nameLink <|> targetGroupLink

    return PortMapping{..}

newtype SharedMemory = SharedMemory Int
  deriving (Eq, Show, Generic)

newtype ReservedMemory = ReservedMemory Int
  deriving (Eq, Show, Generic)

data Container = Container
  { _cName         :: Text
  , _cImage        :: Text
  , _cMemory       :: Maybe Int
  , _cCpu          :: Maybe Int
  , _cPortMappings :: [PortMapping]
  , _cEnvironment  :: HashMap Text Text
  , _cLogConfig    :: Maybe ECS.LogConfiguration
  , _cEntryPoint   :: [Text]
  , _cCommand      :: [Text]
  } deriving (Eq, Show, Generic)

makeLenses ''Container

instance FromJSON Container where
  parseJSON = withObject "container" $ \o -> do
    _cName         <- T.pack <$> o .: "name"
    _cImage        <- T.pack <$> o .: "image"
    _cMemory       <- o .:? "memory"
    _cCpu          <- o .:? "cpu"
    _cPortMappings <- maybe [] id <$> o .:? "port-mappings"
    _cEnvironment  <- maybe Map.empty id <$> o .:? "environment"
    _cLogConfig    <- o .:? "logging"
    _cEntryPoint   <- maybe [] id <$> o .:? "entry-point"
    _cCommand      <- maybe [] id <$> o .:? "command"
    return Container{..}

data DeploymentStrategy =
    DSBlueGreen
  | DSRolling
  | DSTearDown
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

defaultDeploymentStrategy :: DeploymentStrategy
defaultDeploymentStrategy = DSBlueGreen

instance FromJSON DeploymentStrategy where
  parseJSON = withText "deployment strategy" $ \txt ->
    case (T.toLower txt) of
      "blue-green" -> pure DSBlueGreen
      "rolling"    -> pure DSRolling
      "tear-down"  -> pure DSTearDown
      _            -> fail $ "Invalid deployment strategy: " ++ (T.unpack txt)

data ServiceDeployment = ServiceDeployment
  { _sdTaskRole           :: Maybe Text
  , _sdServiceRole        :: Maybe Text
  , _sdDesiredCount       :: Int
  , _sdDeploymentStrategy :: DeploymentStrategy
  , _sdContainers         :: [Container]
  , _sdNetworkMode        :: Maybe ECS.NetworkMode
  , _sdPlacementStrategy  :: [ECS.PlacementStrategy]
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceDeployment

type NamedServiceDeployment = (Text, ServiceDeployment)

instance FromJSON ServiceDeployment where
  parseJSON = withObject "service deployment" $ \o -> do
    _sdTaskRole           <- o .:? "task-role"
    _sdServiceRole        <- o .:? "service-role"
    _sdDesiredCount       <- maybe 1 id <$> o .:? "desired-count"
    _sdDeploymentStrategy <- maybe defaultDeploymentStrategy id <$> o .:? "deployment-strategy"
    _sdContainers         <- o .: "containers"
    _sdNetworkMode        <- o .:? "network"
    _sdPlacementStrategy  <- maybe [] id <$> o .:? "placement-strategy"
    return ServiceDeployment{..}

data ServiceCompose = ServiceCompose
  { _scServices :: HashMap Text ServiceDeployment
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceCompose

instance FromJSON ServiceCompose where
  parseJSON = withObject "service compose" $ \o -> do
    _scServices <- maybe Map.empty id <$> o .:? "services"
    return ServiceCompose{..}
