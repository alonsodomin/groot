{-# LANGUAGE TypeFamilies       #-}

module Groot.Data.Service where

import Control.Lens
import Data.Text
import Groot.Data.Base
import qualified Network.AWS.ECS as ECS

data ServiceStatus =
    ServiceActive
  | ServiceInactive
  deriving (Eq, Show)

data ServiceFilter =
  ServiceStatusFilter ServiceStatus
  deriving (Eq, Show)

instance FilterPredicate ServiceFilter where
  type CanBeFilteredBy ServiceFilter = ECS.ContainerService

  matches (ServiceStatusFilter ServiceActive) serv =
    maybe False (== (pack "ACTIVE")) (serv ^. ECS.csStatus)
  matches (ServiceStatusFilter ServiceInactive) serv =
    maybe False (== (pack "INACTIVE")) (serv ^. ECS.csStatus)