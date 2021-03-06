{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Groot.Manifest
     ( PortELBLink(..)
     -- Port Mapping
     , PortMapping
     , pmContainerPort
     , pmElbLink
     , pmHostPort
     , pmProtocol
     -- Volume
     , Volume
     , vName
     , vSourcePath
     -- MountPoint
     , MountPoint
     , mpReadOnly
     , mpTargetPath
     , mpVolume
     -- Memory
     , AssignedMemory
     , _AssignedMemory
     , ReservedMemory
     , _ReservedMemory
     , Memory
     -- Container
     , Container
     , cCommand
     , cCpu
     , cDnsSearch
     , cEntryPoint
     , cEnvironment
     , cEssential
     , cExtraHosts
     , cHostname
     , cImage
     , cLabels
     , cLinks
     , cLogConfig
     , cMemory
     , cMountPoints
     , cName
     , cPortMappings
     , cPriviledged
     , cUser
     , cWorkDir
     -- Deployment Strategy
     , DeploymentStrategy (..)
     , defaultDeploymentStrategy
     -- Deployment Constraints
     , DeploymentConstraint (..)
     -- Service Network
     , ServiceNetwork (..)
     -- Service Deployment
     , ServiceDeployment
     , NamedServiceDeployment
     , sdContainers
     , sdDeploymentConstraints
     , sdDeploymentStrategy
     , sdDesiredCount
     , sdExecutionRole
     , sdNetwork
     , sdPlacementStrategy
     , sdServiceRole
     , sdTaskRole
     -- Image Filter
     , ImageFilterSpec
     , imageFilters
     , defaultImageFilterSpec
     -- Instance Group
     , InstanceGroupCapacity
     , instanceGroupCapacity
     , igcDesired
     , igcMaximum
     , igcMinimum
     , InstanceGroup
     , instanceGroup
     , igCapacity
     , igImage
     , igInstanceType
     -- Manifest
     , GrootManifest
     , gmInstanceGroups
     , gmServices
     , gmVolumes
     -- Utilities
     , defaultManifestFilePath
     , loadManifest
     , introspectManifest
     ) where

import           Control.Applicative
import           Control.Lens              hiding ((.=))
import           Control.Monad
import           Control.Monad.Catch       hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.Aeson.Types          as JSON
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.These
import           Data.Typeable
import           Data.Yaml                 (decodeFileEither,
                                            prettyPrintParseException)
import           GHC.Generics
import qualified Network.AWS.AutoScaling   as AS
import qualified Network.AWS.EC2           as EC2
import qualified Network.AWS.ECS           as ECS

import           Groot.Console
import           Groot.Core
import           Groot.Exception
import           Groot.Internal.Data.JSON
import           Groot.Internal.Data.Text
import           Groot.Internal.Util
import           Groot.Types

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
    DeployDistinctInstance
  | DeployOnExpr Text
  deriving (Eq, Show)

instance FromJSON DeploymentConstraint where
  parseJSON x = (deployOnDistinct x) <|> (deployOnExpr x)
    where deployOnDistinct (JSON.String str) =
            if str == "distinct-instance"
              then pure DeployDistinctInstance
              else mzero
          deployOnDistinct _ = mzero

          deployOnExpr = withObject "deployment constaint" $ \o ->
            DeployOnExpr <$> o .: "expr"

data ServiceNetwork =
    NoNetwork
  | BridgeNetwork
  | HostNetwork
  | AWSNetwork { _snSubnets :: [Text], _snSecurityGroups :: [Text], _snAssignPublicIP :: Bool }
  deriving (Eq, Show, Generic)

awsNetworkCfg :: JSON.Value -> JSON.Parser ServiceNetwork
awsNetworkCfg = withObject "network config" $ \o -> do
  _snSubnets        <- o .: "subnets"
  _snSecurityGroups <- maybe [] id <$> o .:? "security-groups"
  _snAssignPublicIP <- maybe False id <$> o .:? "assign-public-ip"
  return $ AWSNetwork{..}

instance FromJSON ServiceNetwork where
  parseJSON = withObject "service network" $ \o -> do
    networkMode <- (o .: "mode") :: JSON.Parser Text
    case networkMode of
      "none"   -> return NoNetwork
      "bridge" -> return BridgeNetwork
      "host"   -> return HostNetwork
      "awsvpc" -> awsNetworkCfg =<< o .: "config"
      _        -> fail $ "Invalid network mode: " ++ (T.unpack networkMode)

data ServiceDeployment = ServiceDeployment
  { _sdTaskRole              :: Maybe Text
  , _sdServiceRole           :: Maybe Text
  , _sdExecutionRole         :: Maybe Text
  , _sdDesiredCount          :: Int
  , _sdDeploymentStrategy    :: DeploymentStrategy
  , _sdContainers            :: [Container]
  , _sdNetwork               :: Maybe ServiceNetwork
  , _sdDeploymentConstraints :: [DeploymentConstraint]
  , _sdPlacementStrategy     :: [ECS.PlacementStrategy]
  } deriving (Eq, Show, Generic)

makeLenses ''ServiceDeployment

type NamedServiceDeployment = (Text, ServiceDeployment)

instance FromJSON ServiceDeployment where
  parseJSON = withObject "service deployment" $ \o -> do
    _sdTaskRole              <- o .:? "task-role"
    _sdServiceRole           <- o .:? "service-role"
    _sdExecutionRole         <- o .:? "execution-role"
    _sdDesiredCount          <- maybe 1 id <$> o .:? "desired-count"
    _sdDeploymentStrategy    <- maybe defaultDeploymentStrategy id <$> o .:? "deployment-strategy"
    _sdContainers            <- o .: "containers"
    _sdNetwork               <- o .:? "network"
    _sdDeploymentConstraints <- maybe [] id <$> o .:? "deployment-constraints"
    _sdPlacementStrategy     <- maybe [] id <$> o .:? "placement-strategy"
    return ServiceDeployment{..}

newtype ImageFilterSpec = ImageFilterSpec { imageFilters :: NonEmpty ImageFilterPart }
  deriving (Eq, Show, Generic)

defaultImageFilterSpec :: ImageFilterSpec
defaultImageFilterSpec = ImageFilterSpec $ NEL.fromList
                       [ IFPName "*-amazon-ecs-optimized"
                       , IFPOwnerAlias "amazon"
                       , IFPArchitecture EC2.X86_64
                       , IFPRootDeviceType EC2.EBS
                       , IFPVirtualizationType EC2.HVM
                       , IFPImageState EC2.ISAvailable
                       ]

parseImageFilterPart :: FromText a => String -> Text -> (a -> ImageFilterPart) -> JSON.Object -> JSON.Parser (Maybe ImageFilterPart)
parseImageFilterPart errMsg field f o = runMaybeT $ fmap f (MaybeT $ (parseFromText (\i -> errMsg ++ ' ':i)) =<< o .:? field)

instance FromJSON ImageFilterSpec where
  parseJSON = withObject "image filter" $ \o -> do
    name               <- runMaybeT $ IFPName <$> (MaybeT $ o .:? "name")
    virtualizationType <- parseImageFilterPart "Invalid virtualization type:" "virtualization-type" IFPVirtualizationType o
    ownerAlias         <- runMaybeT $ IFPOwnerAlias <$> (MaybeT $ o .:? "owner-alias")
    architecture       <- parseImageFilterPart "Invalid architecture:" "architecture" IFPArchitecture o
    rootDeviceType     <- parseImageFilterPart "Invalid device type:" "root-device-type" IFPRootDeviceType o
    imageSt            <- parseImageFilterPart "Invalid image state:" "state" IFPImageState o

    filterParts        <- pure $ catMaybes
                        [ name
                        , virtualizationType
                        , ownerAlias
                        , architecture
                        , rootDeviceType
                        , imageSt
                        ]
    case (NEL.nonEmpty filterParts) of
      Just x  -> return $ ImageFilterSpec x
      Nothing -> fail "Must have at least one filter element."

instance ToJSON ImageFilterSpec where
  toJSON (ImageFilterSpec filtrs) = JSON.Object . Map.fromList $ encodeFilterPart <$> (NEL.toList filtrs)
    where encodeFilterPart :: ImageFilterPart -> (Text, JSON.Value)
          encodeFilterPart (IFPName x)               = ("name", toJSON x)
          encodeFilterPart (IFPVirtualizationType x) = ("virtualization-type", toJSON . toText $ x)
          encodeFilterPart (IFPOwnerAlias x)         = ("owner-alias", toJSON x)
          encodeFilterPart (IFPArchitecture x)       = ("architecture", toJSON . toText $ x)
          encodeFilterPart (IFPRootDeviceType x)     = ("root-device-type", toJSON . toText $ x)
          encodeFilterPart (IFPImageState x)         = ("state", toJSON . toText $ x)

data InstanceGroupCapacity = InstanceGroupCapacity
  { _igcMinimum :: Int
  , _igcMaximum :: Int
  , _igcDesired :: Maybe Int
  } deriving (Eq, Show, Generic)

instanceGroupCapacity :: Int -> Int -> Maybe Int -> InstanceGroupCapacity
instanceGroupCapacity = InstanceGroupCapacity

makeLenses ''InstanceGroupCapacity

instance FromJSON InstanceGroupCapacity where
  parseJSON = withObject "instance group capacity" $ \o -> do
    _igcMinimum <- o .: "min"
    _igcMaximum <- o .: "max"
    _igcDesired <- o .: "desired"
    return InstanceGroupCapacity{..}

instance ToJSON InstanceGroupCapacity where
  toJSON igc = JSON.object
    [ "min" .= (igc ^. igcMinimum)
    , "max" .= (igc ^. igcMaximum)
    , "desired" .= (igc ^. igcDesired)
    ]

data InstanceGroup = InstanceGroup
  { _igInstanceType :: InstanceType
  , _igImage        :: Either Ami ImageFilterSpec
  , _igCapacity     :: InstanceGroupCapacity
  } deriving (Eq, Show, Generic)

instanceGroup :: InstanceType -> Either Ami ImageFilterSpec -> InstanceGroupCapacity -> InstanceGroup
instanceGroup = InstanceGroup

makeLenses ''InstanceGroup

instance FromJSON InstanceGroup where
  parseJSON = withObject "instance group" $ \o -> do
    _igInstanceType <- InstanceType <$> o .: "instance-type"

    _igImage <- do
      imageAmi    <- o .:? "image"
      imageFilter <- o .:? "image-filter"
      case (imageAmi, imageFilter) of
        (Nothing, Nothing)  -> fail $ "You must provide one of 'image' or 'image-filter'."
        (Just ami, Nothing) -> pure $ Left ami
        (Nothing, Just f)   -> pure $ Right f
        _                   -> fail $ "Must provide only one of 'image' or 'image-filter', but not both."

    _igCapacity <- o .: "capacity"
    return InstanceGroup{..}

instance ToJSON InstanceGroup where
  toJSON ig = JSON.object
    [ "instance-type" .= (toText $ ig ^. igInstanceType)
    , case (ig ^. igImage) of
      Left ami     -> "image" .= (toText ami)
      Right filtrs -> "image-filter" .= filtrs
    , "capacity" .= (ig ^. igCapacity)
    ]

data GrootManifest = GrootManifest
  { _gmInstanceGroups :: HashMap Text InstanceGroup
  , _gmServices       :: HashMap Text ServiceDeployment
  , _gmVolumes        :: HashMap Text Volume
  } deriving (Eq, Show, Generic)

makeLenses ''GrootManifest

instance FromJSON GrootManifest where
  parseJSON = withObject "manifest" $ \o -> do
    _gmInstanceGroups <- maybe Map.empty id <$> o .:? "instance-groups"
    _gmServices       <- maybe Map.empty id <$> o .:? "services"
    _gmVolumes        <- (Map.map head) <$> maybe Map.empty (groupByKey $ view vName) <$> o .:? "volumes"
    return GrootManifest{..}

instance ToJSON GrootManifest where
  toJSON gm = JSON.object
         [ "instance-groups" .= (gm ^. gmInstanceGroups)
         ]

-- Defaults

defaultManifestFilePath :: FilePath
defaultManifestFilePath = "./groot-manifest.yml"

-- Operations

loadManifest :: (MonadIO m, MonadThrow m) => FilePath -> m GrootManifest
loadManifest file = do
  parsed <- liftIO $ decodeFileEither file
  case parsed of
    Left err       -> throwM $ manifestParseError file (fromString $ prettyPrintParseException err)
    Right manifest -> return manifest

introspectManifest :: ClusterRef -> GrootIO GrootManifest
introspectManifest clusterRef = do
  groups <- obtainInstanceGroups
  return $ GrootManifest { _gmInstanceGroups = groups, _gmServices = Map.empty, _gmVolumes = Map.empty }
  where obtainInstanceGroups = useResource . awsResource . runConduit $ yield clusterRef
          .| fetchAutoScalingGroups
          .| instanceGroupMapSink

        instanceGroupMapSink =
          let insertM m k v = (\x -> Map.insert k x m) <$> v
          in CL.foldM (\m (g, c) -> insertM m (g ^. AS.asgAutoScalingGroupName) (mkInstanceGroup g c)) Map.empty

        mkInstanceGroup :: MonadThrow m => AS.AutoScalingGroup -> AS.LaunchConfiguration -> m InstanceGroup
        mkInstanceGroup asg cfg = do
          ami <- parseImageId $ cfg ^. AS.lcImageId
          return $ instanceGroup
            (InstanceType $ cfg ^. AS.lcInstanceType)
            (Left ami)
            (instanceGroupCapacity (asg ^. AS.asgMinSize) (asg ^. AS.asgMaxSize) (Just $ asg ^. AS.asgDesiredCapacity))
          where parseImageId :: MonadThrow m => Text -> m Ami
                parseImageId imageId = either (\_ -> throwM $ invalidInstanceImageId imageId) pure $ fromText imageId
