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
     , viewArn
     , asArn
     , Ami (..)
     -- Cluster
     , ClusterName (..)
     , ClusterArnPath (..)
     , ClusterArn
     , arnClusterName
     , Cluster (..)
     , cClusterName
     , cClusterArn
     -- Container Instance
     , ContainerInstanceId (..)
     , ContainerInstanceArn
     , arnContainerInstanceId
     -- Container Service
     , ServiceName (..)
     , ContainerServiceArn
     , arnContainerServiceName
     -- Task
     , TaskId (..)
     , TaskArn
     , arnTaskId
     -- Task Definition
     , TaskFamily (..)
     , TaskDefId (..)
     , tdiTaskFamily
     , tdiTaskRevision
     , TaskDefArn
     , arnTaskDefId
     , arnTaskDefFamily
     , arnTaskDefRevision
     ) where

import           Prelude          hiding (takeWhile)
import           Control.Monad    (join)
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

viewArn :: forall a b. FromText b => Getting (First Text) a Text -> a -> Maybe (Arn b)
viewArn l item = join $ either (\_ -> Nothing) Just <$> fromText <$> item ^? l

asArn :: forall a. FromText a => Getting (First (Arn a)) Text (Arn a)
asArn = to (\txt -> either (\_ -> Nothing) Just $ fromText txt) . _Just

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

newtype ClusterName = ClusterName Text
  deriving (Eq, Show)

instance FromText ClusterName where
  parser = ClusterName <$> takeText

instance ToText ClusterName where
  toText (ClusterName x) = x

newtype ClusterArnPath = ClusterArnPath ClusterName
  deriving (Eq, Show)

capClusterName :: Getter ClusterArnPath ClusterName
capClusterName = to (\(ClusterArnPath name) -> name)

type ClusterArn = Arn ClusterArnPath

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

data Cluster = Cluster
  { _cClusterArn :: Maybe ClusterArn
  } deriving (Eq, Show)

cClusterArn :: Getter Cluster (Maybe ClusterArn)
cClusterArn = to _cClusterArn

cClusterName :: Getting (First Text) Cluster ClusterName
cClusterName = cClusterArn . _Just . arnClusterName

-- Container Instance

newtype ContainerInstanceId = ContainerInstanceId UUID
  deriving (Eq, Show)

instance FromText ContainerInstanceId where
  parser = ContainerInstanceId <$> uuid

instance ToText ContainerInstanceId where
  toText (ContainerInstanceId s) = UUID.toText s

newtype ContainerInstanceArnPath = ContainerInstanceArnPath ContainerInstanceId
  deriving (Eq, Show)

ciapContainerInstanceId :: Getter ContainerInstanceArnPath ContainerInstanceId
ciapContainerInstanceId = to (\(ContainerInstanceArnPath x) -> x)

type ContainerInstanceArn = Arn ContainerInstanceArnPath

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

-- Container Service

newtype ServiceName = ServiceName Text
  deriving (Eq, Show)

instance FromText ServiceName where
  parser = ServiceName <$> takeText

instance ToText ServiceName where
  toText (ServiceName s) = s

newtype ContainerServiceArnPath = ContainerServiceArnPath ServiceName
  deriving (Eq, Show)

csapContainerServiceName :: Getter ContainerServiceArnPath ServiceName
csapContainerServiceName = to (\(ContainerServiceArnPath x) -> x)

type ContainerServiceArn = Arn ContainerServiceArnPath

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

-- Task

newtype TaskId = TaskId UUID
  deriving (Eq, Show)

instance FromText TaskId where
  parser = TaskId <$> uuid

instance ToText TaskId where
  toText (TaskId s) = UUID.toText s

newtype TaskArnPath = TaskArnPath TaskId
  deriving (Eq, Show)

tapTaskId :: Getter TaskArnPath TaskId
tapTaskId = to (\(TaskArnPath x) -> x)

type TaskArn = Arn TaskArnPath

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

-- Task Definition

newtype TaskFamily = TaskFamily Text
  deriving (Eq, Show)

instance FromText TaskFamily where
  parser = TaskFamily <$> takeText

instance ToText TaskFamily where
  toText (TaskFamily s) = s

data TaskDefId = TaskDefId
  { _tdiTaskFamily   :: TaskFamily
  , _tdiTaskRevision :: Int
  } deriving (Eq, Show)

makeLenses ''TaskDefId

instance FromText TaskDefId where
  parser = do
    family   <- subparser =<< takeTill (== ':')
    char ':'
    revision <- parser
    return $ TaskDefId family revision

instance ToText TaskDefId where
  toText (TaskDefId family revision) =
    T.concat [toText family, ":", toText revision]

newtype TaskDefArnPath = TaskDefArnPath TaskDefId
  deriving (Eq, Show)

tdapTaskDefId :: Getter TaskDefArnPath TaskDefId
tdapTaskDefId = to (\(TaskDefArnPath x) -> x)

type TaskDefArn = Arn TaskDefArnPath

arnTaskDefId :: Getter TaskDefArn TaskDefId
arnTaskDefId = arnResourcePath . tdapTaskDefId

arnTaskDefFamily :: Getter TaskDefArn TaskFamily
arnTaskDefFamily = arnTaskDefId . tdiTaskFamily

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