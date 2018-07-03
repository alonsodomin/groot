{-# LANGUAGE TemplateHaskell #-}

module Groot.Cluster
     ( InstancesByGroup
     , ibgGroup
     , ibgInstances
     , Cluster
     , clName
     , clInstanceGroups
     , introspectCluster
     ) where

import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Text               (Text)
import qualified Network.AWS.AutoScaling as AS
import qualified Network.AWS.EC2         as EC2

import           Groot.Core
import           Groot.Manifest
import           Groot.Types

data InstancesByGroup = InstancesByGroup
  { _ibgGroup     :: InstanceGroup
  , _ibgInstances :: [AS.Instance]
  } deriving (Eq, Show)

makeLenses ''InstancesByGroup

instancesByGroup :: AS.AutoScalingGroup -> AS.LaunchConfiguration -> InstanceGroup
instancesByGroup asg cfg = instanceGroup
    (InstanceType $ cfg ^. AS.lcInstanceType)
    (Right $ defaultImageFilterSpec)
    (instanceGroupCapacity (asg ^. AS.asgMinSize) (asg ^. AS.asgMaxSize) (Just $ asg ^. AS.asgDesiredCapacity))

data Cluster = Cluster
  { _clName           :: ClusterRef
  , _clInstanceGroups :: HashMap Text InstanceGroup
  } deriving (Eq, Show)

makeLenses ''Cluster

introspectCluster :: ClusterRef -> GrootIO Cluster
introspectCluster clusterRef = do
  groups <- runGrootResource . awsResource . runConduit $ yield clusterRef .| fetchAutoScalingGroups .| instanceGroupMapSink
  return $ Cluster clusterRef groups
  where instanceGroupMapSink = CL.fold (\m (g, c) -> Map.insert (g ^. AS.asgAutoScalingGroupName) (instancesByGroup g c) m) Map.empty
