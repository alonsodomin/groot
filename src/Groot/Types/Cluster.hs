{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Groot.Types.Cluster
     (
       ClusterArnPath (..)
     , ClusterArn
     , arnClusterName
     , Cluster (..)
     , cClusterName
     , cClusterArn
     ) where

import           Control.Lens
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Groot.Data.Text
import           Groot.Types.Base

newtype ClusterArnPath = ClusterArnPath Text
  deriving (Eq, Show)

capClusterName :: Getter ClusterArnPath Text
capClusterName = to (\(ClusterArnPath name) -> name)

type ClusterArn = Arn ClusterArnPath

arnClusterName :: Getter ClusterArn Text
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

cClusterArn :: Getter Cluster (Maybe ClusterArn)
cClusterArn = to _cClusterArn

cClusterName :: Getting (First Text) Cluster Text
cClusterName = cClusterArn . _Just . arnClusterName