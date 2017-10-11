{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Groot.Data.TaskDef where

import Data.Data
import Data.Text
import GHC.Generics
import Groot.Data.Base
import qualified Network.AWS.ECS as ECS

newtype TaskDefARN = TaskDefARN Text
  deriving (Eq, Generic, Data)

instance Show TaskDefARN where
  show (TaskDefARN arn) = unpack arn

newtype TaskFamily = TaskFamily Text
  deriving (Eq, Generic, Data)

instance Show TaskFamily where
  show (TaskFamily x) = unpack x

data TaskDefStatus =
    TaskActive
  | TaskInactive
  deriving (Eq, Ord, Read, Generic, Data)

instance Show TaskDefStatus where
  show TaskActive   = "Active"
  show TaskInactive = "Inactive"

data TaskDefFilter =
    FamilyFilter TaskFamily
  | StatusFilter TaskDefStatus
