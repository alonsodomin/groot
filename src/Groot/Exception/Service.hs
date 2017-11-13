{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Service where

import Control.Exception.Lens
import Control.Monad.Catch hiding (Handler)
import Control.Lens
import Data.Typeable
import Groot.Data

data ServiceException =
    ServiceNotFound ServiceNotFound
  | AmbiguousServiceName AmbiguousServiceName
  | InactiveService InactiveService
  deriving (Eq, Show, Typeable)

instance Exception ServiceException

data ServiceNotFound =
  ServiceNotFound' ServiceRef (Maybe ClusterRef)
  deriving (Eq, Show, Typeable)

serviceNotFound :: ServiceRef -> Maybe ClusterRef -> SomeException
serviceNotFound serviceName clusterRef =
  toException . ServiceNotFound $ ServiceNotFound' serviceName clusterRef

instance Exception ServiceNotFound

data AmbiguousServiceName =
  AmbiguousServiceName' ServiceRef [ClusterRef]
  deriving (Eq, Typeable, Show)

ambiguousServiceName :: ServiceRef -> [ClusterRef] -> SomeException
ambiguousServiceName serviceName clusters =
  toException . AmbiguousServiceName $ AmbiguousServiceName' serviceName clusters

instance Exception AmbiguousServiceName

data InactiveService =
  InactiveService' ServiceRef ClusterRef
  deriving (Eq, Typeable, Show)

instance Exception InactiveService

inactiveService :: ServiceRef -> ClusterRef -> SomeException
inactiveService serviceRef clusterRef =
  toException . InactiveService $ InactiveService' serviceRef clusterRef

class AsServiceException t where
  _ServiceException :: Prism' t ServiceException
  {-# MINIMAL _ServiceException #-}

  _ServiceNotFound :: Prism' t ServiceNotFound
  _ServiceNotFound = _ServiceException . _ServiceNotFound

  _AmbiguousServiceName :: Prism' t AmbiguousServiceName
  _AmbiguousServiceName = _ServiceException . _AmbiguousServiceName

  _InactiveService :: Prism' t InactiveService
  _InactiveService = _ServiceException . _InactiveService

instance AsServiceException SomeException where
  _ServiceException = exception

instance AsServiceException ServiceException where
  _ServiceException = id

  _ServiceNotFound = prism ServiceNotFound $ \case
    ServiceNotFound e -> Right e
    x                 -> Left x

  _AmbiguousServiceName = prism AmbiguousServiceName $ \case
    AmbiguousServiceName e -> Right e
    x                      -> Left x

  _InactiveService = prism InactiveService $ \case
    InactiveService e -> Right e
    x                 -> Left x