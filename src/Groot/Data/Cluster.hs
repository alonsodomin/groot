{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Groot.Data.Cluster where

import Control.Lens
import Data.Data
import Data.Text
import GHC.Generics
import Groot.Data.Base
import qualified Network.AWS.ECS as ECS

newtype ClusterARN = ClusterARN Text
  deriving (Eq, Generic, Data)

instance Show ClusterARN where
  show (ClusterARN arn) = unpack arn

newtype ClusterId = ClusterId Text
  deriving (Eq, Generic, Data)

instance Show ClusterId where
  show (ClusterId id) = unpack id

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
    maybe False (== (pack "ACTIVE")) (cluster ^. ECS.cStatus)
  matches (ClusterStatusFilter ClusterInactive) cluster =
    maybe False (== (pack "INACTIVE")) (cluster ^. ECS.cStatus)
