{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Instance where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch    hiding (Handler)
import           Data.Typeable
import           Groot.Types

data InstanceException =
    InstanceNotFound InstanceNotFound
  | DrainingInstance DrainingInstance
  deriving (Eq, Show, Typeable)

instance Exception InstanceException

data InstanceNotFound =
  InstanceNotFound' ContainerInstanceRef (Maybe ClusterRef)
  deriving (Eq, Show, Typeable)

instance Exception InstanceNotFound

instanceNotFound :: ContainerInstanceRef -> Maybe ClusterRef -> SomeException
instanceNotFound instanceRef clusterRef =
  toException . InstanceNotFound $ InstanceNotFound' instanceRef clusterRef

data DrainingInstance = DrainingInstance' ContainerInstanceRef
  deriving (Eq, Show, Typeable)

instance Exception DrainingInstance

drainingInstance :: ContainerInstanceRef -> SomeException
drainingInstance = toException . DrainingInstance . DrainingInstance'

class AsInstanceException t where
  _InstanceException :: Prism' t InstanceException
  {-# MINIMAL _InstanceException #-}

  _InstanceNotFound :: Prism' t InstanceNotFound
  _InstanceNotFound = _InstanceException . _InstanceNotFound

  _DrainingInstance :: Prism' t DrainingInstance
  _DrainingInstance = _InstanceException . _DrainingInstance

instance AsInstanceException SomeException where
  _InstanceException = exception

instance AsInstanceException InstanceException where
  _InstanceException = id

  _InstanceNotFound = prism InstanceNotFound $ \case
    InstanceNotFound e -> Right e
    x                  -> Left x

  _DrainingInstance = prism DrainingInstance $ \case
    DrainingInstance e -> Right e
    x                  -> Left x
