{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Groot.Data.Task where

import Data.Data
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.AWS.Data.Text

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
