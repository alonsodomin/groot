{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Groot.Data.Cluster where

import Control.Lens
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Groot.Data.Base
import Network.AWS.Data.Text
import qualified Network.AWS.ECS as ECS

newtype ClusterRef = ClusterRef Text
  deriving (Eq, Show, Generic, Data, Read)

mkClusterRef :: String -> ClusterRef
mkClusterRef = ClusterRef . T.pack

instance ToText ClusterRef where
  toText (ClusterRef ref) = ref

data ClusterStatus =
    ClusterActive
  | ClusterInactive
  deriving (Eq, Ord, Read, Generic, Data)

instance Show ClusterStatus where
  show ClusterActive   = "Active"
  show ClusterInactive = "Inactive"

data ClusterFilter =
  ClusterStatusFilter ClusterStatus
  deriving (Eq, Show)

instance FilterPredicate ClusterFilter where
  type CanBeFilteredBy ClusterFilter = ECS.Cluster

  matches (ClusterStatusFilter ClusterActive) cluster =
    maybe False (== (T.pack "ACTIVE")) (cluster ^. ECS.cStatus)
  matches (ClusterStatusFilter ClusterInactive) cluster =
    maybe False (== (T.pack "INACTIVE")) (cluster ^. ECS.cStatus)
