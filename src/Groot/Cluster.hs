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
import           Control.Monad.Trans.State.Lazy
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.Text                      (Text)
import           Network.AWS
import qualified Network.AWS.AutoScaling        as AS
import qualified Network.AWS.EC2                as EC2

import           Groot.Core
import           Groot.Manifest
import           Groot.Types

data InstancesByGroup = InstancesByGroup
  { _ibgGroup     :: InstanceGroup
  , _ibgInstances :: [AS.Instance]
  } deriving (Eq, Show)

makeLenses ''InstancesByGroup

instancesByGroup :: AS.AutoScalingGroup -> InstancesByGroup
instancesByGroup asg = InstancesByGroup
  { _ibgGroup = instanceGroup
    EC2.T2_Medium
    (Right $ defaultImageFilterSpec)
    (instanceGroupCapacity (asg ^. AS.asgMinSize) (asg ^. AS.asgMaxSize) (Just $ asg ^. AS.asgDesiredCapacity))
  , _ibgInstances = asg ^. AS.asgInstances}

data Cluster = Cluster
  { _clName           :: ClusterRef
  , _clInstanceGroups :: HashMap Text InstancesByGroup
  } deriving (Eq, Show)

makeLenses ''Cluster

introspectCluster :: ClusterRef -> GrootIO Cluster
introspectCluster clusterRef = do
  groups <- runGrootResource . awsResource . runConduit $ yield clusterRef .| findAutoScalingGroups .| instanceGroupMapSink
  return $ Cluster clusterRef groups
  where instanceGroupMapSink :: MonadAWS m => ConduitT AS.AutoScalingGroup Void m (HashMap Text InstancesByGroup)
        instanceGroupMapSink = CL.fold (\m group -> Map.insert (group ^. AS.asgAutoScalingGroupName) (instancesByGroup group) m) Map.empty
