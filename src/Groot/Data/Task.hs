{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}

module Groot.Data.Task where

import Control.Lens
import Data.Data
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Groot.Data.Base
import Network.AWS.Data.Text
import qualified Network.AWS.ECS as ECS

newtype TaskRef = TaskRef Text
  deriving (Eq, Show, Generic, Data, Read)

instance IsString TaskRef where
  fromString = TaskRef . T.pack

instance ToText TaskRef where
  toText (TaskRef t) = t

data TaskStatus =
    Running
  | Pending
  | Stopped
  deriving (Eq, Show, Ord, Read, Generic, Data)

instance ToText TaskStatus where
  toText Running = "RUNNING"
  toText Pending = "PENDING"
  toText Stopped = "STOPPED"

data TaskFilter =
  TaskStatusFilter TaskStatus
  deriving (Eq, Show, Read, Generic, Data)

taskStatusIs :: TaskStatus -> TaskFilter
taskStatusIs = TaskStatusFilter

instance FilterPredicate TaskFilter where
  type CanBeFilteredBy TaskFilter = ECS.Task

  matches (TaskStatusFilter st) task =
    maybe False (== (toText st)) $ task ^. ECS.tLastStatus
