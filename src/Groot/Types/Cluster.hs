{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Groot.Types.Cluster
     (
       ClusterArnPath (..)
     , ClusterArn
     , Cluster
     , cClusterName
     , cClusterArn
     ) where

import           Control.Lens
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Groot.Data.Text
import           Groot.Types.Base

newtype ClusterArnPath = ClusterArnPath Text
  deriving (Eq, Show)

capClusterName :: Lens' ClusterArnPath Text
capClusterName = lens (\(ClusterArnPath name) -> name) (\_ a -> ClusterArnPath a)

type ClusterArn = Arn ClusterArnPath

arnClusterName :: Lens' ClusterArn Text
arnClusterName = arnResourcePath . capClusterName

instance FromText ClusterArnPath where
  parser = do
    "cluster/"
    clusterName <- takeText
    return $ ClusterArnPath clusterName

instance ToText ClusterArnPath where
  toText (ClusterArnPath clusterName) =
    T.append "cluster/" clusterName

data Cluster = Cluster
  { _cClusterArn :: Maybe ClusterArn
  } deriving (Eq, Show)

makeLenses ''Cluster

cClusterName :: Traversal' Cluster Text
cClusterName = cClusterArn . _Just . arnClusterName