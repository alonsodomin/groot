{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
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
     , TaskDefFilter(..)
     ) where

import           Control.Lens
import           Control.Monad        (join)
import           Data.Attoparsec.Text
import           Data.Data
import           Data.Monoid
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import           GHC.Generics         hiding (to)
import           Network.AWS
import qualified Network.AWS.ECS      as ECS
import           Prelude              hiding (takeWhile)

import           Groot.Data.Filter
import           Groot.Data.Text

-- | An AWS service identifier, typically used in AWS ARNs
data ServiceId =
    AutoScaling
  | ECS
  | EC2
  deriving (Eq, Show, Generic, Enum, Bounded, Data)

instance FromText ServiceId where
  parser = takeLowerText >>= \case
    "autoscaling" -> pure AutoScaling
    "ecs"         -> pure ECS
    "ec2"         -> pure EC2
    e             ->
      fromTextError $ "Failure parsing service id from " <> e

instance ToText ServiceId where
  toText AutoScaling = "autoscaling"
  toText ECS         = "ecs"
  toText EC2         = "ec2"

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
  , _arnRegion       :: Region
  , _arnAccountId    :: AccountId
  , _arnResourcePath :: a
  } deriving (Eq, Show, Data, Generic)

-- |Obtain the 'ServiceId' from a given 'Arn'
arnServiceId :: forall a. Getter (Arn a) ServiceId
arnServiceId = to _arnServiceId

-- |Obtain the AWS region from a given 'Arn'
arnRegion :: forall a. Getter (Arn a) Region
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
    region    <- subparser =<< takeTill (== ':')
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
    , toText region
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
data ClusterFilter =
    CFRef ClusterRef
  | CFStatus ClusterStatus
  deriving (Eq, Show)

-- |Cluster filter predicates based on status
isActiveCluster, isInactiveCluster :: ClusterFilter
isActiveCluster   = CFStatus CSActive
isInactiveCluster = CFStatus CSInactive

-- |Cluster filter based on name or ARN
clusterHasNameOrArn :: Text -> ClusterFilter
clusterHasNameOrArn = CFRef . ClusterRef

instance Filter ClusterFilter where
  type FilterItem ClusterFilter = ECS.Cluster

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

-- EC2 Instance

newtype EC2InstanceId = EC2InstanceId Text
  deriving (Eq, Show)

instance ToText EC2InstanceId where
  toText (EC2InstanceId id) = id

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
data ContainerServiceFilter =
    CSFRef ContainerServiceRef
  | CSFStatus ContainerServiceStatus
  deriving (Eq, Show)

-- |Service filter predicate based on service status
isActiveContainerService, isInactiveContainerService :: ContainerServiceFilter
isActiveContainerService   = CSFStatus CSSActive
isInactiveContainerService = CSFStatus CSSInactive

-- |Service filter preficate based on service name or ARN
isContainerService :: ContainerServiceRef -> ContainerServiceFilter
isContainerService = CSFRef

instance Filter ContainerServiceFilter where
  type FilterItem ContainerServiceFilter = ECS.ContainerService

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
data TaskDefFilter =
    TDFFamily TaskFamily
  | TDFStatus TaskDefStatus
  deriving (Eq, Show)

instance Filter TaskDefFilter where
  type FilterItem TaskDefFilter = ECS.TaskDefinition

  matches (TDFFamily (TaskFamily family)) taskDef =
    maybe False (== family) $ taskDef ^. ECS.tdFamily
  matches (TDFStatus TDSActive) taskDef =
    maybe False (== ECS.TDSActive) $ taskDef ^. ECS.tdStatus
  matches (TDFStatus TDSInactive) taskDef =
    maybe False (== ECS.TDSInactive) $ taskDef ^. ECS.tdStatus
