{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Groot.Data.TaskDef where

import           Data.Data
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics
import           Network.AWS.Data.Text

newtype TaskDefRef = TaskDefRef Text
  deriving (Eq, Generic, Data, Show, Read)

instance IsString TaskDefRef where
  fromString = TaskDefRef . T.pack

instance ToText TaskDefRef where
  toText (TaskDefRef t) = t

newtype TaskFamily = TaskFamily Text
  deriving (Eq, Generic, Data, Show, Read)

instance IsString TaskFamily where
  fromString = TaskFamily . T.pack

instance ToText TaskFamily where
  toText (TaskFamily f) = f

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
