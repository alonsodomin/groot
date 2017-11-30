{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Groot.Data.Instance where

import           Data.Aeson
import           Data.Data
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics
import           Network.AWS.Data.Text

newtype InstanceRef = InstanceRef Text
  deriving (Eq, Show, Data, Read, Generic)

instance ToText InstanceRef where
  toText (InstanceRef s) = s

instance IsString InstanceRef where
  fromString = InstanceRef . T.pack

data InstanceStatus =
    InstanceActive
  | InstanceInactive
  deriving (Eq, Ord, Generic, Data)

instance Show InstanceStatus where
  show InstanceActive   = "Active"
  show InstanceInactive = "Inactive"
