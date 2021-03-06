{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Groot.Types
Description : Type definitions for Groot
Copyright   : A. Alonso Dominguez (c) 2017, http://github.com/alonsodomin
License     : Apache 2.0
Maintainer  : A. Alonso Dominguez <alonso.domin (λ) google>
Stability   : experimental
Portability : portable

This is a module providing global type definitions needed in Groot
-}
module Groot.Types
     ( ServiceId (..)
     , AccountId (..)
     , Arn (..)
     , arnAccountId
     , arnRegion
     , arnResourcePath
     , arnServiceId
     , viewArn
     , Ami (..)
     , ImageFilterPart(..)
     , ImageFilter
     , imageHasName
     , imageVirtualizationType
     , imageOwnerAlias
     , imageArchitecture
     , imageRootDeviceType
     , imageState
     , InstanceType(..)
     -- Auth
     , SerialNumber
     , AuthToken (..)
     , RoleArn
     , RoleArnPath (..)
     , MFADeviceArn
     , MFADeviceArnPath (..)
     , MFACredentials
     , mfaCredentials
     , mfaCredsDevice
     , mfaCredsToken
     -- Cluster
     , ClusterName (..)
     , ClusterArnPath (..)
     , ClusterArn
     , arnClusterName
     , ClusterRef(..)
     , ClusterStatus(..)
     , ClusterFilter
     , isActiveCluster
     , isInactiveCluster
     , clusterHasNameOrArn
     -- Container Instance
     , ContainerInstanceId (..)
     , ContainerInstanceArn
     , arnContainerInstanceId
     , ContainerInstanceRef(..)
     , canUpdateContainerAgent
     -- EC2 Instance
     , EC2InstanceId (..)
     -- Container Service
     , ServiceName (..)
     , ContainerServiceArn
     , arnContainerServiceName
     , ContainerServiceRef(..)
     , ContainerServiceCoords(..)
     , ContainerServiceStatus(..)
     , ContainerServiceFilter
     , isActiveContainerService
     , isInactiveContainerService
     , isContainerService
     -- Task
     , TaskId (..)
     , TaskArn
     , arnTaskId
     , TaskRef(..)
     , TaskStatus(..)
     -- Task Definition
     , TaskDefRef(..)
     , TaskFamily (..)
     , TaskDefId (..)
     , tdiTaskFamily
     , tdiTaskRevision
     , nextTaskDefId
     , TaskDefArn
     , arnTaskDefId
     , arnTaskDefFamily
     , arnTaskDefRevision
     , TaskDefStatus(..)
     , TaskDefFilterPart(..)
     , TaskDefFilter
     -- Resources
     , ResourceType(..)
     , allResourceTypes
     , ResourceUsage(..)
     , mkResourceUsage
     , ruType
     , ruAllocated
     , ruFree
     , ruFreeRatio
     , ruUsed
     , ruUsedRatio
     , ResourceSummary
     ) where

import           Control.Lens
import           Control.Monad              (join)
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Data
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import           Data.Monoid                hiding ((<>))
import           Data.Ratio
import           Data.Semigroup             (Semigroup, (<>))
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           GHC.Generics               hiding (to)
import           Network.AWS                hiding (IAM, InstanceType)
import qualified Network.AWS.EC2            as EC2
import qualified Network.AWS.ECS            as ECS
import           Numeric
import           Prelude                    hiding (takeWhile)
import           Text.PrettyPrint.Tabulate  (CellValueFormatter)

import           Groot.Internal.Data.Filter
import           Groot.Internal.Data.JSON
import           Groot.Internal.Data.Text

-- | An AWS service identifier, typically used in AWS ARNs
data ServiceId =
    AutoScaling
  | ECS
  | EC2
  | IAM
  | STS
  deriving (Eq, Show, Generic, Enum, Bounded, Data)

instance FromText ServiceId where
  parser = takeLowerText >>= \case
    "autoscaling" -> pure AutoScaling
    "ecs"         -> pure ECS
    "ec2"         -> pure EC2
    "iam"         -> pure IAM
    "sts"         -> pure STS
    e             ->
      fromTextError $ "Failure parsing service id from " <> e

instance ToText ServiceId where
  toText AutoScaling = "autoscaling"
  toText ECS         = "ecs"
  toText EC2         = "ec2"
  toText IAM         = "iam"
  toText STS         = "sts"

-- | An AWS account identifier
newtype AccountId = AccountId Text
  deriving (Eq, Show, Data, Generic)

instance IsString AccountId where
  fromString = AccountId . T.pack

instance ToText AccountId where
  toText (AccountId s) = s

-- | An AWS Resource Name (ARN for short) used to uniquely identify
-- a given resource
data Arn a = Arn
  { _arnServiceId    :: ServiceId
  , _arnRegion       :: Maybe Region
  , _arnAccountId    :: AccountId
  , _arnResourcePath :: a
  } deriving (Eq, Show, Data, Generic)

-- |Obtain the 'ServiceId' from a given 'Arn'
arnServiceId :: forall a. Getter (Arn a) ServiceId
arnServiceId = to _arnServiceId

-- |Obtain the AWS region from a given 'Arn'
arnRegion :: forall a. Getter (Arn a) (Maybe Region)
arnRegion = to _arnRegion

-- |Obtain the 'AccountId' from a given 'Arn'
arnAccountId :: forall a. Getter (Arn a) AccountId
arnAccountId = to _arnAccountId

-- |Obtain the resource path from a given 'Arn'
arnResourcePath :: forall a. Getter (Arn a) a
arnResourcePath = to _arnResourcePath

instance FromText a => FromText (Arn a) where
  parser = do
    "arn:aws:"
    serviceId <- subparser =<< takeTill (== ':')
    char ':'
    region    <- option Nothing (fmap Just $ subparser =<< takeTill (== ':'))
    char ':'
    account   <- takeWhile (/= ':')
    char ':'
    path <- parser
    return $ Arn serviceId region (AccountId account) path

instance ToText a => ToText (Arn a) where
  toText (Arn service region account path) = T.concat [
      "arn:aws:"
    , toText service
    , ":"
    , maybe T.empty toText region
    , ":"
    , toText account
    , ":"
    , toText path
    ]

-- |Parse an ARN given a getter an a resource from where to obtain the text form
viewArn :: forall a b. FromText b => Getting (First Text) a Text -> a -> Maybe (Arn b)
viewArn l item = join $ either (\_ -> Nothing) Just <$> fromText <$> item ^? l

-- | An AWS Machine Image, used to uniquely identify a given
-- image for an specific instance
newtype Ami = Ami Text
  deriving (Eq, Show, Data, Generic)

instance FromText Ami where
  parser = do
    "ami-"
    ident <- takeText
    return $ Ami ident

instance ToText Ami where
  toText (Ami ident) = T.append "ami-" ident

instance FromJSON Ami where
  parseJSON = withText "ami" (parseFromText' (\i -> "Invalid AMI: " ++ i))

instance ToJSON Ami where
  toJSON = toJSON . toText

newtype InstanceType = InstanceType Text
  deriving (Eq, Show, Generic, Data)

instance ToText InstanceType where
  toText (InstanceType x) = x

instance IsString InstanceType where
  fromString = InstanceType . T.pack

data ImageFilterPart =
    IFPName Text
  | IFPVirtualizationType EC2.VirtualizationType
  | IFPOwnerAlias Text
  | IFPArchitecture EC2.ArchitectureValues
  | IFPRootDeviceType EC2.DeviceType
  | IFPImageState EC2.ImageState
  deriving (Eq, Show, Generic, Data)

type ImageFilter = Filter ImageFilterPart

imageHasName :: Text -> ImageFilter
imageHasName = toFilter . IFPName

imageVirtualizationType :: EC2.VirtualizationType -> ImageFilter
imageVirtualizationType = toFilter . IFPVirtualizationType

imageOwnerAlias :: Text -> ImageFilter
imageOwnerAlias = toFilter . IFPOwnerAlias

imageArchitecture :: EC2.ArchitectureValues -> ImageFilter
imageArchitecture = toFilter . IFPArchitecture

imageRootDeviceType :: EC2.DeviceType -> ImageFilter
imageRootDeviceType = toFilter . IFPRootDeviceType

imageState :: EC2.ImageState -> ImageFilter
imageState = toFilter . IFPImageState

instance IsFilter ImageFilterPart where
  type FilterItem ImageFilterPart = EC2.Image

  matches (IFPName name) img =
    maybe False (== name) $ img ^. EC2.iName
  matches (IFPVirtualizationType virtType) img =
    (img ^. EC2.iVirtualizationType) == virtType
  matches (IFPOwnerAlias ownerAlias)       img =
    maybe False (== ownerAlias) $ img ^. EC2.iImageOwnerAlias
  matches (IFPArchitecture arch) img =
    (img ^. EC2.iArchitecture) == arch
  matches (IFPRootDeviceType rdt) img =
    (img ^. EC2.iRootDeviceType) == rdt
  matches (IFPImageState state) img =
    (img ^. EC2.iState) == state

-- Auth

newtype SerialNumber = SerialNumber Text
  deriving (Eq, Show, Data, Generic)

instance IsString SerialNumber where
  fromString = SerialNumber . T.pack

instance FromText SerialNumber where
  parser = SerialNumber <$> takeText

instance ToText SerialNumber where
  toText (SerialNumber txt) = txt

newtype AuthToken = AuthToken Text
  deriving (Eq, Data, Generic)

instance IsString AuthToken where
  fromString = AuthToken . T.pack

instance ToText AuthToken where
  toText (AuthToken value) = value

roleArnPreffix :: Text
roleArnPreffix = "role/"

newtype RoleArnPath = RoleArnPath Text
  deriving (Eq, Show, Data, Generic)

instance FromText RoleArnPath where
  parser = do
    string roleArnPreffix
    roleName <- parser
    return $ RoleArnPath roleName

instance ToText RoleArnPath where
  toText (RoleArnPath roleName) =
    T.append roleArnPreffix roleName

type RoleArn = Arn RoleArnPath

mfaDevicePathPrefix :: Text
mfaDevicePathPrefix = "mfa/"

newtype MFADeviceArnPath = MFADeviceArnPath Text
  deriving (Eq, Show, Data, Generic)

instance FromText MFADeviceArnPath where
  parser = do
    string mfaDevicePathPrefix
    userName <- parser
    return $ MFADeviceArnPath userName

instance ToText MFADeviceArnPath where
  toText (MFADeviceArnPath userName) =
    T.append mfaDevicePathPrefix userName

type MFADeviceArn = Arn MFADeviceArnPath

data MFACredentials = MFACredentials MFADeviceArn AuthToken
  deriving (Eq, Data, Generic)

mfaCredentials :: MFADeviceArn -> AuthToken -> MFACredentials
mfaCredentials = MFACredentials

mfaCredsDevice :: Getter MFACredentials MFADeviceArn
mfaCredsDevice = to (\(MFACredentials arn _) -> arn)

mfaCredsToken :: Getter MFACredentials AuthToken
mfaCredsToken = to (\(MFACredentials _ token) -> token)

-- Cluster

-- |Type describing the cluster name
newtype ClusterName = ClusterName Text
  deriving (Eq, Show, Data, Generic)

instance IsString ClusterName where
  fromString = ClusterName . T.pack

instance FromText ClusterName where
  parser = ClusterName <$> takeText

instance ToText ClusterName where
  toText (ClusterName x) = x

-- |Type describing the ARN path for ECS clusters
newtype ClusterArnPath = ClusterArnPath ClusterName
  deriving (Eq, Show, Data, Generic)

-- |Obtains the cluster name from a given Cluster ARN path
capClusterName :: Getter ClusterArnPath ClusterName
capClusterName = to (\(ClusterArnPath name) -> name)

-- |Type describing cluster ARNs
type ClusterArn = Arn ClusterArnPath

-- |Obtains the cluster name from a cluster ARN
arnClusterName :: Getter ClusterArn ClusterName
arnClusterName = arnResourcePath . capClusterName

capPreffix :: Text
capPreffix = "cluster/"

instance FromText ClusterArnPath where
  parser = do
    string capPreffix
    clusterName <- parser
    return $ ClusterArnPath clusterName

instance ToText ClusterArnPath where
  toText (ClusterArnPath clusterName) =
    T.append capPreffix $ toText clusterName

-- |Type used across the code to make references to clusters
newtype ClusterRef = ClusterRef Text
  deriving (Eq, Show, Data, Generic)

instance IsString ClusterRef where
  fromString = ClusterRef . T.pack

instance ToText ClusterRef where
  toText (ClusterRef txt) = txt

-- |Different statuses available for clusters
data ClusterStatus =
    CSActive
  | CSInactive
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Data, Generic)

instance ToText ClusterStatus where
  toText CSActive   = "ACTIVE"
  toText CSInactive = "INACTIVE"

-- |Filter definition for clusters
data ClusterFilterPart =
    CFRef ClusterRef
  | CFStatus ClusterStatus
  deriving (Eq, Show)

type ClusterFilter = Filter ClusterFilterPart

-- |Cluster filter predicates based on status
isActiveCluster, isInactiveCluster :: ClusterFilter
isActiveCluster   = toFilter $ CFStatus CSActive
isInactiveCluster = toFilter $ CFStatus CSInactive

-- |Cluster filter based on name or ARN
clusterHasNameOrArn :: Text -> ClusterFilter
clusterHasNameOrArn = toFilter . CFRef . ClusterRef

instance IsFilter ClusterFilterPart where
  type FilterItem ClusterFilterPart = ECS.Cluster

  matches (CFRef (ClusterRef txt)) cluster =
       maybe False (== txt) (cluster ^. ECS.cClusterName)
    || maybe False (== txt) (cluster ^. ECS.cClusterARN)
  matches (CFStatus CSActive) cluster =
    maybe False (== (T.pack "ACTIVE")) $ cluster ^. ECS.cStatus
  matches (CFStatus CSInactive) cluster =
    maybe False (== (T.pack "INACTIVE")) $ cluster ^. ECS.cStatus

-- Container Instance

-- |Type describing a Container Instance Identifier
newtype ContainerInstanceId = ContainerInstanceId UUID
  deriving (Eq, Show)

instance FromText ContainerInstanceId where
  parser = ContainerInstanceId <$> uuid

instance ToText ContainerInstanceId where
  toText (ContainerInstanceId s) = UUID.toText s

-- |Type describing a Container Instance ARN path
newtype ContainerInstanceArnPath = ContainerInstanceArnPath ContainerInstanceId
  deriving (Eq, Show)

-- |Obtain a Container Instance Identifier from a Container Instance ARN
ciapContainerInstanceId :: Getter ContainerInstanceArnPath ContainerInstanceId
ciapContainerInstanceId = to (\(ContainerInstanceArnPath x) -> x)

-- |Type describing a Container Instance ARN
type ContainerInstanceArn = Arn ContainerInstanceArnPath

-- |Obtain a Container Instance Identifier from a Container Instance ARN
arnContainerInstanceId :: Getter ContainerInstanceArn ContainerInstanceId
arnContainerInstanceId = arnResourcePath . ciapContainerInstanceId

ciapPreffix :: Text
ciapPreffix = "container-instance/"

instance FromText ContainerInstanceArnPath where
  parser = do
    string ciapPreffix
    instanceId <- parser
    return $ ContainerInstanceArnPath instanceId

instance ToText ContainerInstanceArnPath where
  toText (ContainerInstanceArnPath instanceId) =
    T.append ciapPreffix $ toText instanceId

-- |Type used across the code to make references to container instances
newtype ContainerInstanceRef = ContainerInstanceRef Text
  deriving (Eq, Show, Data, Read, Generic)

instance ToText ContainerInstanceRef where
  toText (ContainerInstanceRef s) = s

instance IsString ContainerInstanceRef where
  fromString = ContainerInstanceRef . T.pack

data ContainerInstanceFilterPart =
  CIFAgentStatus ECS.AgentUpdateStatus
  deriving (Eq, Show)

type ContainerInstanceFilter = Filter ContainerInstanceFilterPart

canUpdateContainerAgent :: ContainerInstanceFilter
canUpdateContainerAgent = (toFilter $ CIFAgentStatus ECS.AUSFailed) ||| (toFilter $ CIFAgentStatus ECS.AUSUpdated)

instance IsFilter ContainerInstanceFilterPart where
  type FilterItem ContainerInstanceFilterPart = ECS.ContainerInstance

  matches (CIFAgentStatus status) inst =
    maybe True (== status) $ inst ^. ECS.ciAgentUpdateStatus

-- EC2 Instance

newtype EC2InstanceId = EC2InstanceId Text
  deriving (Eq, Show)

instance ToText EC2InstanceId where
  toText (EC2InstanceId x) = x

-- Container Service

-- |Type describing a service name
newtype ServiceName = ServiceName Text
  deriving (Eq, Show)

instance FromText ServiceName where
  parser = ServiceName <$> takeText

instance ToText ServiceName where
  toText (ServiceName s) = s

-- |Type describing a service ARN path
newtype ContainerServiceArnPath = ContainerServiceArnPath ServiceName
  deriving (Eq, Show)

-- |Obtain a service name from a service ARN path
csapContainerServiceName :: Getter ContainerServiceArnPath ServiceName
csapContainerServiceName = to (\(ContainerServiceArnPath x) -> x)

-- |Type describing a service ARN
type ContainerServiceArn = Arn ContainerServiceArnPath

-- |Obtain a service name from a service ARN
arnContainerServiceName :: Getter ContainerServiceArn ServiceName
arnContainerServiceName = arnResourcePath . csapContainerServiceName

csapPreffix :: Text
csapPreffix = "service/"

instance FromText ContainerServiceArnPath where
  parser = do
    string csapPreffix
    serviceName <- parser
    return $ ContainerServiceArnPath serviceName

instance ToText ContainerServiceArnPath where
  toText (ContainerServiceArnPath serviceName) =
    T.append csapPreffix $ toText serviceName

-- |Type used across the code to make references to services
newtype ContainerServiceRef = ContainerServiceRef Text
  deriving (Eq, Show, Generic, Data, Read)

instance IsString ContainerServiceRef where
  fromString = ContainerServiceRef . T.pack

instance ToText ContainerServiceRef where
  toText (ContainerServiceRef s) = s

-- |Service coordinates are compoound of a service reference and a cluster reference.
-- They uniquely identify a service
data ContainerServiceCoords = ContainerServiceCoords ContainerServiceRef ClusterRef
  deriving (Eq, Show, Generic, Data)

-- |Service statuses
data ContainerServiceStatus =
    CSSActive
  | CSSInactive
  deriving (Eq, Show, Ord, Enum, Bounded, Data, Generic)

-- |Service filters
data ContainerServiceFilterPart =
    CSFRef ContainerServiceRef
  | CSFStatus ContainerServiceStatus
  deriving (Eq, Show)

type ContainerServiceFilter = Filter ContainerServiceFilterPart

-- |Service filter predicate based on service status
isActiveContainerService, isInactiveContainerService :: ContainerServiceFilter
isActiveContainerService   = toFilter $ CSFStatus CSSActive
isInactiveContainerService = toFilter $ CSFStatus CSSInactive

-- |Service filter preficate based on service name or ARN
isContainerService :: ContainerServiceRef -> ContainerServiceFilter
isContainerService = toFilter . CSFRef

instance IsFilter ContainerServiceFilterPart where
  type FilterItem ContainerServiceFilterPart = ECS.ContainerService

  matches (CSFStatus CSSActive) serv =
    maybe False (== (T.pack "ACTIVE")) (serv ^. ECS.csStatus)
  matches (CSFStatus CSSInactive) serv =
    maybe False (== (T.pack "INACTIVE")) (serv ^. ECS.csStatus)

  matches (CSFRef (ContainerServiceRef ref)) serv =
       maybe False (== ref) (serv ^. ECS.csServiceName)
    || maybe False (== ref) (serv ^. ECS.csServiceARN)

-- Task

-- |Type describing a task identifier
newtype TaskId = TaskId UUID
  deriving (Eq, Show)

instance FromText TaskId where
  parser = TaskId <$> uuid

instance ToText TaskId where
  toText (TaskId s) = UUID.toText s

-- |Type describing a task ARN path
newtype TaskArnPath = TaskArnPath TaskId
  deriving (Eq, Show)

-- |Obtain a task identifier from a task ARN
tapTaskId :: Getter TaskArnPath TaskId
tapTaskId = to (\(TaskArnPath x) -> x)

-- |Type describing a task ARN
type TaskArn = Arn TaskArnPath

-- |Obtain a task identifier from a task ARN
arnTaskId :: Getter TaskArn TaskId
arnTaskId = arnResourcePath . tapTaskId

tapPreffix :: Text
tapPreffix = "task/"

instance FromText TaskArnPath where
  parser = do
    string tapPreffix
    taskId <- parser
    return $ TaskArnPath taskId

instance ToText TaskArnPath where
  toText (TaskArnPath taskId) =
    T.append tapPreffix $ toText taskId

-- |Type used across the code to make references to tasks
newtype TaskRef = TaskRef Text
  deriving (Eq, Show, Generic, Data, Read)

instance IsString TaskRef where
  fromString = TaskRef . T.pack

instance ToText TaskRef where
  toText (TaskRef t) = t

-- |Task statuses
data TaskStatus =
    TSRunning
  | TSStopped
  | TSPending
  deriving (Eq, Show, Ord, Enum, Bounded, Data, Generic, Read)

-- Task Definition

-- |Type used across the code to make references to task definitions
newtype TaskDefRef = TaskDefRef Text
  deriving (Eq, Generic, Data, Show, Read)

instance IsString TaskDefRef where
  fromString = TaskDefRef . T.pack

instance ToText TaskDefRef where
  toText (TaskDefRef t) = t

-- |Type describing a task definition family
newtype TaskFamily = TaskFamily Text
  deriving (Eq, Show, Data, Generic)

instance IsString TaskFamily where
  fromString = TaskFamily . T.pack

instance FromText TaskFamily where
  parser = TaskFamily <$> takeText

instance ToText TaskFamily where
  toText (TaskFamily s) = s

-- |A Task Definition Identifier, compound of a task family and a revision number
data TaskDefId = TaskDefId
  { _tdiTaskFamily   :: TaskFamily
  , _tdiTaskRevision :: Int
  } deriving (Eq, Show, Data, Generic)

makeLenses ''TaskDefId

-- |Increments the revision number of a given task definition
nextTaskDefId :: TaskDefId -> TaskDefId
nextTaskDefId (TaskDefId family revision) = TaskDefId family (revision + 1)

instance FromText TaskDefId where
  parser = do
    family   <- subparser =<< takeTill (== ':')
    char ':'
    revision <- parser
    return $ TaskDefId family revision

instance ToText TaskDefId where
  toText (TaskDefId family revision) =
    T.concat [toText family, ":", toText revision]

-- |Type describing the task definition ARN path
newtype TaskDefArnPath = TaskDefArnPath TaskDefId
  deriving (Eq, Show, Data, Generic)

-- |Obtain a task definition identifier from a task definition ARN path
tdapTaskDefId :: Getter TaskDefArnPath TaskDefId
tdapTaskDefId = to (\(TaskDefArnPath x) -> x)

-- |Type describing a task definition ARN
type TaskDefArn = Arn TaskDefArnPath

-- |Obtain a task definition identifier from a task definition ARN
arnTaskDefId :: Getter TaskDefArn TaskDefId
arnTaskDefId = arnResourcePath . tdapTaskDefId

-- |Obtain a task definition family from a task definition ARN
arnTaskDefFamily :: Getter TaskDefArn TaskFamily
arnTaskDefFamily = arnTaskDefId . tdiTaskFamily

-- |Obtain a task definition revision number from a task definition ARN
arnTaskDefRevision :: Getter TaskDefArn Int
arnTaskDefRevision = arnTaskDefId . tdiTaskRevision

tdapPreffix :: Text
tdapPreffix = "task-definition/"

instance FromText TaskDefArnPath where
  parser = do
    string tdapPreffix
    taskDefId <- parser
    return $ TaskDefArnPath taskDefId

instance ToText TaskDefArnPath where
  toText (TaskDefArnPath taskDefId) =
    T.append tdapPreffix $ toText taskDefId

-- |Task definition statuses
data TaskDefStatus =
    TDSActive
  | TDSInactive
  deriving (Eq, Show, Ord, Enum, Bounded, Read, Generic, Data)

-- |Task definition filters
data TaskDefFilterPart =
    TDFFamily TaskFamily
  | TDFStatus TaskDefStatus
  deriving (Eq, Show)

type TaskDefFilter = Filter TaskDefFilterPart

instance IsFilter TaskDefFilterPart where
  type FilterItem TaskDefFilterPart = ECS.TaskDefinition

  matches (TDFFamily (TaskFamily family)) taskDef =
    maybe False (== family) $ taskDef ^. ECS.tdFamily
  matches (TDFStatus TDSActive) taskDef =
    maybe False (== ECS.TDSActive) $ taskDef ^. ECS.tdStatus
  matches (TDFStatus TDSInactive) taskDef =
    maybe False (== ECS.TDSInactive) $ taskDef ^. ECS.tdStatus

data ResourceType =
    Memory
  | CPU
  deriving (Eq, Show, Enum, Bounded, Generic, Data)

instance Hashable ResourceType

allResourceTypes :: [ResourceType]
allResourceTypes = enumFromTo minBound maxBound

data ResourceUsage = ResourceUsage
  { _ruType      :: ResourceType
  , _ruUsed      :: Int
  , _ruAllocated :: Int
  } deriving (Eq, Generic, Data)

instance Semigroup ResourceUsage where
  (ResourceUsage CPU leftX leftY) <> (ResourceUsage CPU rightX rightY) = ResourceUsage CPU (leftX + rightX) (leftY + rightY)
  (ResourceUsage Memory leftX leftY) <> (ResourceUsage Memory rightX rightY) = ResourceUsage Memory (leftX + rightX) (leftY + rightY)

mkResourceUsage :: ResourceType -> (Int, Int) -> ResourceUsage
mkResourceUsage resType (used, alloc) = ResourceUsage resType used alloc

ruType :: Getter ResourceUsage ResourceType
ruType = to _ruType

ruAllocated :: Getter ResourceUsage Int
ruAllocated = to _ruAllocated

ruFree :: Getter ResourceUsage Int
ruFree = to $ \x -> (_ruAllocated x) - (_ruUsed x)

ruFreeRatio :: Getter ResourceUsage Rational
ruFreeRatio = to $ \x -> (toInteger $ (_ruAllocated x) - (_ruUsed x)) % (toInteger $ _ruAllocated x)

ruUsed :: Getter ResourceUsage Int
ruUsed = to _ruUsed

ruUsedRatio :: Getter ResourceUsage Rational
ruUsedRatio = to $ \x -> (toInteger $ _ruUsed x) % (toInteger $ _ruAllocated x)

instance Show ResourceUsage where
  show summ = concat [show available, "/", show allocated, " ", units, " ", percent]
    where units = case (summ ^. ruType) of
            Memory -> "mb"
            CPU    -> "units"
          available = summ ^. ruUsed
          allocated = summ ^. ruAllocated
          percent =
            let pvalue = (fromIntegral available) / (fromIntegral allocated) * 100.0 :: Float
            in concat ["(", showFFloat (Just 1) pvalue "", " %)"]

instance CellValueFormatter ResourceUsage

type ResourceSummary = HashMap ResourceType ResourceUsage
