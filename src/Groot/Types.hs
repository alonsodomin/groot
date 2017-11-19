{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Groot.Types
     ( ServiceId (..)
     , AccountId (..)
     , Arn (..)
     , arnAccountId
     , arnRegion
     , arnResourcePath
     , arnServiceId
     , Ami (..)
     -- Cluster
     , ClusterArnPath (..)
     , ClusterArn
     , arnClusterName
     , Cluster (..)
     , cClusterName
     , cClusterArn
     -- Container Instance
     , ContainerInstanceArn
     , arnContainerInstanceId
     -- Container Service
     , ContainerServiceArn
     , arnContainerServiceName
     -- Task
     , TaskArn
     , arnTaskId
     -- Task Definition
     , TaskDefArn
     , arnTaskDefId
     ) where

import           Prelude          hiding (takeWhile)
import           Control.Lens
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.String
import           Data.UUID        (UUID)
import qualified Data.UUID        as UUID
import           GHC.Generics     hiding (to)
import           Groot.Data.Text
import           Network.AWS

-- | An AWS service identifier, typically used in AWS ARNs
data ServiceId =
    AutoScaling
  | ECS
  | EC2
  deriving (Eq, Show, Generic, Enum, Bounded)

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
  deriving (Eq, Show)

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
  } deriving (Eq, Show)

arnServiceId :: forall a. Getter (Arn a) ServiceId
arnServiceId = to _arnServiceId

arnRegion :: forall a. Getter (Arn a) Region
arnRegion = to _arnRegion

arnAccountId :: forall a. Getter (Arn a) AccountId
arnAccountId = to _arnAccountId

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

-- | An AWS Machine Image, used to uniquely identify a given
-- image for an specific instance
newtype Ami = Ami Text
  deriving (Eq, Show)

instance FromText Ami where
  parser = do
    "ami-"
    ident <- takeText
    return $ Ami ident

instance ToText Ami where
  toText (Ami ident) = T.append "ami-" ident

-- Cluster

newtype ClusterArnPath = ClusterArnPath Text
  deriving (Eq, Show)

capClusterName :: Getter ClusterArnPath Text
capClusterName = to (\(ClusterArnPath name) -> name)

type ClusterArn = Arn ClusterArnPath

arnClusterName :: Getter ClusterArn Text
arnClusterName = arnResourcePath . capClusterName

capPreffix :: Text
capPreffix = "cluster/"

instance FromText ClusterArnPath where
  parser = do
    string capPreffix
    clusterName <- takeText
    return $ ClusterArnPath clusterName

instance ToText ClusterArnPath where
  toText (ClusterArnPath clusterName) =
    T.append capPreffix clusterName

data Cluster = Cluster
  { _cClusterArn :: Maybe ClusterArn
  } deriving (Eq, Show)

cClusterArn :: Getter Cluster (Maybe ClusterArn)
cClusterArn = to _cClusterArn

cClusterName :: Getting (First Text) Cluster Text
cClusterName = cClusterArn . _Just . arnClusterName

-- Container Instance

newtype ContainerInstanceArnPath = ContainerInstanceArnPath UUID
  deriving (Eq, Show)

ciapContainerInstanceId :: Getter ContainerInstanceArnPath UUID
ciapContainerInstanceId = to (\(ContainerInstanceArnPath x) -> x)

type ContainerInstanceArn = Arn ContainerInstanceArnPath

arnContainerInstanceId :: Getter ContainerInstanceArn UUID
arnContainerInstanceId = arnResourcePath . ciapContainerInstanceId

ciapPreffix :: Text
ciapPreffix = "container-instance/"

instance FromText ContainerInstanceArnPath where
  parser = do
    string ciapPreffix
    instanceId <- uuid
    return $ ContainerInstanceArnPath instanceId

instance ToText ContainerInstanceArnPath where
  toText (ContainerInstanceArnPath instanceId) =
    T.append ciapPreffix $ UUID.toText instanceId

-- Container Service

newtype ContainerServiceArnPath = ContainerServiceArnPath Text
  deriving (Eq, Show)

csapContainerServiceName :: Getter ContainerServiceArnPath Text
csapContainerServiceName = to (\(ContainerServiceArnPath x) -> x)

type ContainerServiceArn = Arn ContainerServiceArnPath

arnContainerServiceName :: Getter ContainerServiceArn Text
arnContainerServiceName = arnResourcePath . csapContainerServiceName

csapPreffix :: Text
csapPreffix = "service/"

instance FromText ContainerServiceArnPath where
  parser = do
    string csapPreffix
    serviceName <- takeText
    return $ ContainerServiceArnPath serviceName

instance ToText ContainerServiceArnPath where
  toText (ContainerServiceArnPath serviceName) =
    T.append csapPreffix serviceName

-- Task

newtype TaskArnPath = TaskArnPath UUID
  deriving (Eq, Show)

tapTaskId :: Getter TaskArnPath UUID
tapTaskId = to (\(TaskArnPath x) -> x)

type TaskArn = Arn TaskArnPath

arnTaskId :: Getter TaskArn UUID
arnTaskId = arnResourcePath . tapTaskId

tapPreffix :: Text
tapPreffix = "task/"

instance FromText TaskArnPath where
  parser = do
    string tapPreffix
    taskId <- uuid
    return $ TaskArnPath taskId

instance ToText TaskArnPath where
  toText (TaskArnPath taskId) =
    T.append tapPreffix $ UUID.toText taskId

-- Task Definition

newtype TaskDefArnPath = TaskDefArnPath Text
  deriving (Eq, Show)

tdapTaskDefId :: Getter TaskDefArnPath Text
tdapTaskDefId = to (\(TaskDefArnPath x) -> x)

type TaskDefArn = Arn TaskDefArnPath

arnTaskDefId :: Getter TaskDefArn Text
arnTaskDefId = arnResourcePath . tdapTaskDefId

tdapPreffix :: Text
tdapPreffix = "task-definition/"

instance FromText TaskDefArnPath where
  parser = do
    string tdapPreffix
    taskDefId <- takeText
    return $ TaskDefArnPath taskDefId

instance ToText TaskDefArnPath where
  toText (TaskDefArnPath taskDefId) = T.append tdapPreffix taskDefId