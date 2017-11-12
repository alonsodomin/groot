{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Groot.Exception where

import Control.Exception.Lens
import Control.Lens
import Control.Monad.Catch hiding (Handler)
import Data.Typeable
import Groot.Data

data GrootError =
    ClusterNotFound ClusterNotFound
  | InstanceNotFound InstanceNotFound
  | TaskNotFound TaskNotFound
  | ServiceNotFound ServiceNotFound
  | AmbiguousServiceName AmbiguousServiceName
  deriving (Eq, Typeable, Show)

instance Exception GrootError

data ClusterNotFound = ClusterNotFound' ClusterRef
  deriving (Eq, Typeable, Show)

clusterNotFound :: ClusterRef -> SomeException
clusterNotFound = toException . ClusterNotFound . ClusterNotFound'

instance Exception ClusterNotFound

-- class AsClusterNotFoundException t where
--   _ClusterNotFound :: Prism' t ClusterNotFound

-- instance AsClusterNotFoundException ClusterNotFound where
--   _ClusterNotFound = id

-- instance AsClusterNotFoundException SomeException where
--   _ClusterNotFound = exception

-- | Instance Exceptions

data InstanceNotFound = InstanceNotFound' InstanceRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

instanceNotFound :: InstanceRef -> Maybe ClusterRef -> SomeException
instanceNotFound instanceRef clusterRef =
  toException . InstanceNotFound $ InstanceNotFound' instanceRef clusterRef

instance Exception InstanceNotFound

data TaskNotFound = TaskNotFound' TaskRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

taskNotFound :: TaskRef -> Maybe ClusterRef -> SomeException
taskNotFound taskRef clusterRef =
  toException . TaskNotFound $ TaskNotFound' taskRef clusterRef

instance Exception TaskNotFound

data ServiceNotFound = ServiceNotFound' ServiceRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

serviceNotFound :: ServiceRef -> Maybe ClusterRef -> SomeException
serviceNotFound serviceName clusterRef =
  toException . ServiceNotFound $ ServiceNotFound' serviceName clusterRef

instance Exception ServiceNotFound

data AmbiguousServiceName = AmbiguousServiceName' ServiceRef [ClusterRef]
  deriving (Eq, Typeable, Show)

ambiguousServiceName :: ServiceRef -> [ClusterRef] -> SomeException
ambiguousServiceName serviceName clusters =
  toException . AmbiguousServiceName $ AmbiguousServiceName' serviceName clusters

instance Exception AmbiguousServiceName

-- Prisms

class AsGrootError t where
  _GrootError :: Prism' t GrootError
  {-# MINIMAL _GrootError #-}

  _ClusterNotFound :: Prism' t ClusterNotFound
  _ClusterNotFound = _GrootError . _ClusterNotFound

  _ServiceNotFound :: Prism' t ServiceNotFound
  _ServiceNotFound = _GrootError . _ServiceNotFound

  _AmbiguousServiceName :: Prism' t AmbiguousServiceName
  _AmbiguousServiceName = _GrootError . _AmbiguousServiceName

instance AsGrootError SomeException where
  _GrootError = exception

instance AsGrootError GrootError where
  _GrootError = id

  _ClusterNotFound = prism ClusterNotFound $ \case
    ClusterNotFound e -> Right e
    x                 -> Left x

  _ServiceNotFound = prism ServiceNotFound $ \case
    ServiceNotFound e -> Right e
    x                 -> Left x

  _AmbiguousServiceName = prism AmbiguousServiceName $ \case
    AmbiguousServiceName e -> Right e
    x                      -> Left x

-- Prisms

-- _ClusterNotFound :: Prism' GrootError ClusterNotFound
-- _ClusterNotFound = prism' ClusterNotFound fromException

-- _InstanceNotFound :: Prism' GrootError InstanceNotFound
-- _InstanceNotFound = prism' InstanceNotFound fromException

-- _TaskNotFound :: Prism' GrootError TaskNotFound
-- _TaskNotFound = prism' TaskNotFound fromException

-- _ServiceNotFound :: Prism' GrootError ServiceNotFound
-- _ServiceNotFound = prism' ServiceNotFound fromException

-- _AmbiguousServiceName :: Prism' GrootError AmbiguousServiceName
-- _AmbiguousServiceName = prism' AmbiguousServiceName fromException

-- Instances

-- instance Show GrootError where
--   show (ClusterNotFound (ClusterNotFound' (ClusterRef ref))) =
--     "Could not find cluster '" ++ (T.unpack ref) ++ "'"
--   show (InstanceNotFound (InstanceNotFound' (InstanceRef ref) clusterRef)) =
--     "Could not find instance '" ++ (T.unpack ref) ++ "'" ++
--     maybe "" (\x -> " in cluster " ++ (T.unpack . toText $ x)) clusterRef
--   show (ServiceNotFound (ServiceNotFound' serviceName clusterRef)) =
--     "Could not find service '" ++ (T.unpack serviceName) ++ "'" ++
--     maybe "" (\x -> " in cluster " ++ (T.unpack . toText $ x)) clusterRef
--   show (AmbiguousServiceName (AmbiguousServiceName' serviceName clusters)) =
--     let stringifyClusters = intercalate "\n - " $ map (T.unpack . toText) clusters
--     in "Service name '" ++
--        (T.unpack serviceName) ++ "' is ambiguous. It was found in the following clusters:\n" ++
--        stringifyClusters