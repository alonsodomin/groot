module Groot.Cluster where

import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Network.AWS.EC2     as EC2

import           Groot.Core
import           Groot.Manifest
import           Groot.Types

data InstancesByGroup = InstancesByGroup
  { _ibgGroup     :: InstanceGroup
  , _ibgInstances :: [EC2.Instance]
  } deriving (Eq, Show)

data Cluster = Cluster
  { _cName           :: ClusterRef
  , _cInstanceGroups :: HashMap Text InstancesByGroup
  } deriving (Eq, Show)

introspectCluster :: ClusterRef -> GrootIO Cluster
introspectCluster = undefined
