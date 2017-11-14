{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Cluster where

import Control.Exception.Lens
import Control.Monad.Catch hiding (Handler)
import Control.Lens
import Data.Typeable
import Groot.Data

data ClusterException =
  ClusterNotFound ClusterNotFound
  deriving (Eq, Show, Typeable)

instance Exception ClusterException

data ClusterNotFound =
  ClusterNotFound' ClusterRef
  deriving (Eq, Show, Typeable)

instance Exception ClusterNotFound

clusterNotFound :: ClusterRef -> SomeException
clusterNotFound = toException . ClusterNotFound . ClusterNotFound'

class AsClusterException t where
  _ClusterException :: Prism' t ClusterException
  {-# MINIMAL _ClusterException #-}

  _ClusterNotFound :: Prism' t ClusterNotFound
  _ClusterNotFound = _ClusterException . _ClusterNotFound

instance AsClusterException SomeException where
  _ClusterException = exception

instance AsClusterException ClusterException where
  _ClusterException = id

  _ClusterNotFound = prism ClusterNotFound $ \case
    ClusterNotFound e -> Right e