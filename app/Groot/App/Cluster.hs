{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Groot.App.Cluster where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.List (drop)
import Data.Text hiding (drop, take, toLower)
import GHC.Generics

import Groot.Data (AMI)

newtype InstanceType = InstanceType Text
  deriving (Eq, Show, Generic)

instance FromJSON InstanceType

data InstanceGroup = InstanceGroup
  { _igName :: Text
  , _igInstanceType :: InstanceType
  , _igAmi :: AMI
  , _igDesiredInstances :: Int
  , _igMinInstances :: Int
  , _igMaxInstances :: Int
  } deriving (Eq, Show, Generic)

makeLenses ''InstanceGroup

instance FromJSON InstanceGroup where
  parseJSON =
    let uncapitalizeFirstChar :: String -> String
        uncapitalizeFirstChar "" = ""
        uncapitalizeFirstChar (x:xs) = (toLower x) : xs
    in genericParseJSON defaultOptions {
         fieldLabelModifier = uncapitalizeFirstChar . drop 3 }

