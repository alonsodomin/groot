{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Service where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch    hiding (Handler)
import           Data.Text              (Text)
import           Data.Typeable
import           Groot.Types

data ServiceException =
    ServiceNotFound ServiceNotFound
  | AmbiguousServiceName AmbiguousServiceName
  | InactiveService InactiveService
  | FailedServiceDeployment FailedServiceDeployment
  | UndefinedService UndefinedService
  deriving (Eq, Show, Typeable)

instance Exception ServiceException

data ServiceNotFound =
  ServiceNotFound' ContainerServiceRef (Maybe ClusterRef)
  deriving (Eq, Show, Typeable)

serviceNotFound :: ContainerServiceRef -> Maybe ClusterRef -> SomeException
serviceNotFound serviceName clusterRef =
  toException . ServiceNotFound $ ServiceNotFound' serviceName clusterRef

instance Exception ServiceNotFound

data AmbiguousServiceName =
  AmbiguousServiceName' ContainerServiceRef [ClusterRef]
  deriving (Eq, Typeable, Show)

ambiguousServiceName :: ContainerServiceRef -> [ClusterRef] -> SomeException
ambiguousServiceName serviceName clusters =
  toException . AmbiguousServiceName $ AmbiguousServiceName' serviceName clusters

instance Exception AmbiguousServiceName

data InactiveService =
  InactiveService' ContainerServiceRef ClusterRef
  deriving (Eq, Typeable, Show)

instance Exception InactiveService

inactiveService :: ContainerServiceRef -> ClusterRef -> SomeException
inactiveService serviceRef clusterRef =
  toException . InactiveService $ InactiveService' serviceRef clusterRef

data FailedServiceDeployment =
  FailedServiceDeployment' ContainerServiceRef ClusterRef
  deriving (Eq, Typeable, Show)

instance Exception FailedServiceDeployment

failedServiceDeployment :: ContainerServiceRef -> ClusterRef -> SomeException
failedServiceDeployment serviceRef clusterRef =
  toException . FailedServiceDeployment $ FailedServiceDeployment' serviceRef clusterRef

data UndefinedService =
  UndefinedService' Text
  deriving (Eq, Typeable, Show)

instance Exception UndefinedService

undefinedService :: Text -> SomeException
undefinedService = toException . UndefinedService . UndefinedService'

class AsServiceException t where
  _ServiceException :: Prism' t ServiceException
  {-# MINIMAL _ServiceException #-}

  _ServiceNotFound :: Prism' t ServiceNotFound
  _ServiceNotFound = _ServiceException . _ServiceNotFound

  _AmbiguousServiceName :: Prism' t AmbiguousServiceName
  _AmbiguousServiceName = _ServiceException . _AmbiguousServiceName

  _InactiveService :: Prism' t InactiveService
  _InactiveService = _ServiceException . _InactiveService

  _FailedServiceDeployment :: Prism' t FailedServiceDeployment
  _FailedServiceDeployment = _ServiceException . _FailedServiceDeployment

  _UndefinedService :: Prism' t UndefinedService
  _UndefinedService = _ServiceException . _UndefinedService

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

  _FailedServiceDeployment = prism FailedServiceDeployment $ \case
    FailedServiceDeployment e -> Right e
    x                         -> Left x

  _UndefinedService = prism UndefinedService $ \case
    UndefinedService e -> Right e
    x                  -> Left x
