{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Cluster where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch    hiding (Handler)
import           Data.Typeable

import           Groot.Types

data ClusterException =
    ClusterNotFound ClusterNotFound
  | InvalidClusterStatus InvalidClusterStatus
  deriving (Eq, Show, Typeable)

instance Exception ClusterException

data ClusterNotFound =
  ClusterNotFound' ClusterRef
  deriving (Eq, Show, Typeable)

instance Exception ClusterNotFound

clusterNotFound :: ClusterRef -> SomeException
clusterNotFound = toException . ClusterNotFound . ClusterNotFound'

data InvalidClusterStatus =
  InvalidClusterStatus' ClusterRef ClusterStatus (Maybe ClusterStatus)
  deriving (Eq, Show, Typeable)

instance Exception InvalidClusterStatus

invalidClusterStatus :: ClusterRef -> ClusterStatus -> Maybe ClusterStatus -> SomeException
invalidClusterStatus clusterRef currentSt desiredSt =
  toException . InvalidClusterStatus $ InvalidClusterStatus' clusterRef currentSt desiredSt

class AsClusterException t where
  _ClusterException :: Prism' t ClusterException
  {-# MINIMAL _ClusterException #-}

  _ClusterNotFound :: Prism' t ClusterNotFound
  _ClusterNotFound = _ClusterException . _ClusterNotFound

  _InvalidClusterStatus :: Prism' t InvalidClusterStatus
  _InvalidClusterStatus = _ClusterException . _InvalidClusterStatus

instance AsClusterException SomeException where
  _ClusterException = exception

instance AsClusterException ClusterException where
  _ClusterException = id

  _ClusterNotFound = prism ClusterNotFound $ \case
    ClusterNotFound e -> Right e
    x                 -> Left x

  _InvalidClusterStatus = prism InvalidClusterStatus $ \case
    InvalidClusterStatus e -> Right e
    x                      -> Left x
