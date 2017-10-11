{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.Data.Task where

import Data.Data
import Data.Text
import GHC.Generics

newtype TaskARN = TaskARN Text
  deriving (Eq, Generic, Data)

instance Show TaskARN where
  show (TaskARN arn) = unpack arn

data TaskStatus =
    Running
  | Pending
  | Stopped
  deriving (Eq, Show, Ord, Read, Generic, Data)
