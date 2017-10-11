{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Groot.Data.Instance where

import Data.Aeson
import Data.Data
import Data.Text
import GHC.Generics

newtype AMI = AMI Text
  deriving (Eq, Show, Data, Read, Generic)

instance FromJSON AMI

newtype InstanceARN = InstanceARN Text
  deriving (Eq, Data, Generic)

instance Show InstanceARN where
  show (InstanceARN arn) = unpack arn

newtype InstanceId = InstanceId Text
  deriving (Eq, Data, Generic)

instance Show InstanceId where
  show (InstanceId x) = unpack x

data InstanceStatus =
    InstanceActive
  | InstanceInactive
  deriving (Eq, Ord, Generic, Data)

instance Show InstanceStatus where
  show InstanceActive   = "Active"
  show InstanceInactive = "Inactive"