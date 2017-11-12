{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies       #-}

module Groot.Data.Service where

import Control.Lens
import Data.Data
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Groot.Data.Base
import Groot.Data.Cluster (ClusterRef)
import Network.AWS.Data.Text
import qualified Network.AWS.ECS as ECS

newtype ServiceRef = ServiceRef Text
  deriving (Eq, Show, Generic, Data, Read)

instance IsString ServiceRef where
  fromString = ServiceRef . T.pack

instance ToText ServiceRef where
  toText (ServiceRef s) = s

data ServiceCoords = ServiceCoords ServiceRef ClusterRef
  deriving (Eq, Show, Generic, Data, Read)

data ServiceStatus =
    ServiceActive
  | ServiceInactive
  deriving (Eq, Show, Ord, Generic, Data, Read)

data ServiceFilter =
    ServiceStatusFilter ServiceStatus
  | ServiceRefFilter ServiceRef
  deriving (Eq, Show, Generic, Data, Read)

isActiveService :: ServiceFilter
isActiveService = ServiceStatusFilter ServiceActive

instance FilterPredicate ServiceFilter where
  type CanBeFilteredBy ServiceFilter = ECS.ContainerService

  matches (ServiceStatusFilter ServiceActive) serv =
    maybe False (== (T.pack "ACTIVE")) (serv ^. ECS.csStatus)
  matches (ServiceStatusFilter ServiceInactive) serv =
    maybe False (== (T.pack "INACTIVE")) (serv ^. ECS.csStatus)

  matches (ServiceRefFilter (ServiceRef ref)) serv =
       maybe False (== ref) (serv ^. ECS.csServiceName)
    || maybe False (== ref) (serv ^. ECS.csServiceARN)