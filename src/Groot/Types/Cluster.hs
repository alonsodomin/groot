{-# LANGUAGE OverloadedStrings  #-}

module Groot.Types.Cluster where

import Data.Text (Text)
import qualified Data.Text as T
import Groot.Data.Text
import Groot.Types.Base

newtype ClusterArnPath = ClusterArnPath Text
  deriving (Eq, Show)

type ClusterArn = Arn ClusterArnPath

instance FromText ClusterArnPath where
  parser = do
    "cluster/"
    clusterName <- takeText
    return $ ClusterArnPath clusterName

instance ToText ClusterArnPath where
  toText (ClusterArnPath clusterName) =
    T.append "cluster/" clusterName