{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Manifest where

import           Control.Applicative
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch       hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.Semigroup            ((<>))
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.These
import           Data.Typeable
import           Data.Yaml                 (decodeFileEither,
                                            prettyPrintParseException)
import           GHC.Generics
import qualified Network.AWS.ECS           as ECS

import           Groot.Console
import           Groot.Data.Text

toHashMap :: (Hashable k, Eq k) => (a -> k) -> [a] -> HashMap k a
toHashMap f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

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

data Volume = Volume
  { _vName       :: Text
  , _vSourcePath :: Text
  } deriving (Eq, Show, Generic)

makeLenses ''Volume

instance FromJSON Volume where
  parseJSON = withObject "cluster volume" $ \o -> do
    _vName       <- o .: "name"
    _vSourcePath <- o .: "source-path"
    return Volume{..}

data MountPoint = MountPoint
  { _mpVolume     :: Text
  , _mpTargetPath :: Text
  , _mpReadOnly   :: Maybe Bool
  } deriving (Eq, Show, Generic)

makeLenses ''MountPoint

instance FromJSON MountPoint where
  parseJSON = withObject "container mount point" $ \o -> do
    _mpVolume     <- o .: "volume"
    _mpTargetPath <- o .: "target-path"
    _mpReadOnly   <- o .:? "read-only"
    return MountPoint{..}

type AssignedMemory = Int
type ReservedMemory = Int
type Memory         = These AssignedMemory ReservedMemory

_AssignedMemory :: Prism' Memory AssignedMemory
_AssignedMemory = prism This $ \case
  This  a   -> Right a
  These a _ -> Right a
  x         -> Left x

_ReservedMemory :: Prism' Memory ReservedMemory
_ReservedMemory = prism That $ \case
  That  r   -> Right r
  These _ r -> Right r
  x         -> Left x

data Container = Container
  { _cName         :: Text
  , _cImage        :: Text
  , _cMemory       :: Memory
  , _cCpu          :: Maybe Int
  , _cHostname     :: Maybe Text
  , _cExtraHosts   :: HashMap Text Text
  , _cPortMappings :: [PortMapping]
  , _cEnvironment  :: HashMap Text Text
  , _cLinks        :: [Text]
  , _cLabels       :: HashMap Text Text
  , _cLogConfig    :: Maybe ECS.LogConfiguration
  , _cPriviledged  :: Maybe Bool
  , _cEssential    :: Maybe Bool
  , _cWorkDir      :: Maybe Text
  , _cUser         :: Maybe Text
  , _cMountPoints  :: [MountPoint]
  , _cDnsSearch    :: [Text]
  , _cEntryPoint   :: [Text]
  , _cCommand      :: [Text]
  } deriving (Eq, Show, Generic)

makeLenses ''Container

instance FromJSON Container where
  parseJSON = withObject "container" $ \o -> do
    _cName         <- T.pack <$> o .: "name"
    _cImage        <- T.pack <$> o .: "image"

    -- Parse container memory
    let memAssign = o .:? "memory"
        memReserv = o .:? "memory-reserved"

        memChoice ((Just a), (Just r)) = pure $ These a r
        memChoice ((Just a), _)        = pure $ This a
        memChoice (_,        (Just r)) = pure $ That r
        memChoice (_,        _)        =
          fail "One or both of 'memory' or 'memory-reserved' are needed per container"

    _cMemory       <- memChoice =<< ((,) <$> memAssign <*> memReserv)

    _cCpu          <- o .:? "cpu"
    _cHostname     <- o .:? "hostname"
    _cExtraHosts   <- maybe Map.empty id <$> o .:? "extra-hosts"
    _cPortMappings <- maybe [] id <$> o .:? "port-mappings"
    _cEnvironment  <- maybe Map.empty id <$> o .:? "environment"
    _cLinks        <- maybe [] id <$> o .:? "links"
    _cLabels       <- maybe Map.empty id <$> o .:? "labels"
    _cLogConfig    <- o .:? "logging"
    _cPriviledged  <- o .:? "priviledged"
    _cEssential    <- o .:? "essential"
    _cWorkDir      <- o .:? "workdir"
    _cUser         <- o .:? "user"
    _cMountPoints  <- maybe [] id <$> o .:? "mount-points"
    _cDnsSearch    <- maybe [] id <$> o .:? "dns-search"
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

data DeploymentConstraint =
  InstanceAttributesConstraint (HashMap Text Text)
  deriving (Eq, Show)

instance FromJSON DeploymentConstraint where
  parseJSON = withObject "deployment constaint" $ \o -> do
    attrs <- o .: "instance-attrs"
    return $ InstanceAttributesConstraint attrs

data ServiceDeployment = ServiceDeployment
  { _sdTaskRole              :: Maybe Text
  , _sdServiceRole           :: Maybe Text
  , _sdDesiredCount          :: Int
  , _sdDeploymentStrategy    :: DeploymentStrategy
  , _sdContainers            :: [Container]
  , _sdNetworkMode           :: Maybe ECS.NetworkMode
  , _sdDeploymentConstraints :: [DeploymentConstraint]
  , _sdPlacementStrategy     :: [ECS.PlacementStrategy]
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceDeployment

type NamedServiceDeployment = (Text, ServiceDeployment)

instance FromJSON ServiceDeployment where
  parseJSON = withObject "service deployment" $ \o -> do
    _sdTaskRole              <- o .:? "task-role"
    _sdServiceRole           <- o .:? "service-role"
    _sdDesiredCount          <- maybe 1 id <$> o .:? "desired-count"
    _sdDeploymentStrategy    <- maybe defaultDeploymentStrategy id <$> o .:? "deployment-strategy"
    _sdContainers            <- o .: "containers"
    _sdNetworkMode           <- o .:? "network"
    _sdDeploymentConstraints <- maybe [] id <$> o .:? "deployment-constraints"
    _sdPlacementStrategy     <- maybe [] id <$> o .:? "placement-strategy"
    return ServiceDeployment{..}

data GrootManifest = GrootManifest
  { _gmServices :: HashMap Text ServiceDeployment
  , _gmVolumes  :: HashMap Text Volume
  } deriving (Eq, Show, Generic)

makeLenses ''GrootManifest

instance FromJSON GrootManifest where
  parseJSON = withObject "manifest" $ \o -> do
    _gmServices <- maybe Map.empty id <$> o .:? "services"
    _gmVolumes  <- maybe Map.empty (toHashMap $ view vName) <$> o .:? "volumes"
    return GrootManifest{..}

-- Exceptions

data ManifestException =
  ManifestParseError ManifestParseError
  deriving (Eq, Show, Typeable)

instance Exception ManifestException

data ManifestParseError = ManifestParseError' FilePath Text
  deriving (Eq, Show, Typeable)

instance Exception ManifestParseError

manifestParseError :: FilePath -> Text -> SomeException
manifestParseError file reason =
  toException . ManifestParseError $ ManifestParseError' file reason

class AsManifestException t where
  _ManifestException :: Prism' t ManifestException
  {-# MINIMAL _ManifestException #-}

  _ManifestParseError :: Prism' t ManifestParseError
  _ManifestParseError = _ManifestException . _ManifestParseError

instance AsManifestException SomeException where
  _ManifestException = exception

instance AsManifestException ManifestException where
  _ManifestException = id

  _ManifestParseError = prism ManifestParseError $ \case
    ManifestParseError x -> Right x

-- Defaults

defaultManifestFilePath :: FilePath
defaultManifestFilePath = "./groot-manifest.yml"

-- Operations

handleManifestParseError :: MonadConsole m => ManifestParseError -> m ()
handleManifestParseError (ManifestParseError' file reason) =
  putError $ "Could not parse manifest file: " <> (styled blueStyle $ T.pack file) <> "\n"
    <> (styled yellowStyle reason)

loadManifest :: (MonadIO m, MonadConsole m, MonadThrow m) => Maybe FilePath -> m GrootManifest
loadManifest maybeFile = do
  file   <- pure $ maybe defaultManifestFilePath id maybeFile
  parsed <- liftIO $ decodeFileEither file
  case parsed of
    Left err       -> throwM $ manifestParseError file (fromString $ prettyPrintParseException err)
    Right manifest -> return manifest
