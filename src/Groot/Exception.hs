{-# LANGUAGE LambdaCase #-}

module Groot.Exception where

import Control.Lens
import Control.Monad.Catch
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Groot.Data
import Network.AWS.Data.Text

data GrootError =
    ClusterNotFound ClusterNotFound
  | InstanceNotFound InstanceNotFound
  | ServiceNotFound ServiceNotFound
  | AmbiguousServiceName AmbiguousServiceName
  deriving (Eq, Typeable)

data ClusterNotFound = ClusterNotFound' ClusterRef
  deriving (Eq, Typeable)
data InstanceNotFound = InstanceNotFound' InstanceRef (Maybe ClusterRef)
  deriving (Eq, Typeable)
data ServiceNotFound = ServiceNotFound' Text (Maybe ClusterRef)
  deriving (Eq, Typeable)
data AmbiguousServiceName = AmbiguousServiceName' Text [ClusterRef]
  deriving (Eq, Typeable)

-- Smart constructors

clusterNotFound :: ClusterRef -> GrootError
clusterNotFound = ClusterNotFound . ClusterNotFound'

instanceNotFound :: InstanceRef -> Maybe ClusterRef -> GrootError
instanceNotFound instanceRef clusterRef =
  InstanceNotFound (InstanceNotFound' instanceRef clusterRef)

serviceNotFound :: Text -> Maybe ClusterRef -> GrootError
serviceNotFound serviceName clusterRef =
  ServiceNotFound (ServiceNotFound' serviceName clusterRef)

ambiguousServiceName :: Text -> [ClusterRef] -> GrootError
ambiguousServiceName serviceName clusters =
  AmbiguousServiceName (AmbiguousServiceName' serviceName clusters)

-- Prisms

_ClusterNotFound :: Prism' GrootError ClusterNotFound
_ClusterNotFound = prism ClusterNotFound $ \case
  ClusterNotFound e -> Right e
  x                 -> Left x

_InstanceNotFound :: Prism' GrootError InstanceNotFound
_InstanceNotFound = prism InstanceNotFound $ \case
  InstanceNotFound e -> Right e
  x                  -> Left x

_ServiceNotFound :: Prism' GrootError ServiceNotFound
_ServiceNotFound = prism ServiceNotFound $ \case
  ServiceNotFound e -> Right e
  x                 -> Left x

_AmbiguousServiceName :: Prism' GrootError AmbiguousServiceName
_AmbiguousServiceName = prism AmbiguousServiceName $ \case
  AmbiguousServiceName e -> Right e
  x                      -> Left x

-- Instances

instance Show GrootError where
  show (ClusterNotFound (ClusterNotFound' (ClusterRef ref))) =
    "Could not find cluster '" ++ (T.unpack ref) ++ "'"
  show (InstanceNotFound (InstanceNotFound' (InstanceRef ref) clusterRef)) =
    "Could not find instance '" ++ (T.unpack ref) ++ "'" ++
    maybe "" (\x -> " in cluster " ++ (T.unpack . toText $ x)) clusterRef
  show (ServiceNotFound (ServiceNotFound' serviceName clusterRef)) =
    "Could not find service '" ++ (T.unpack serviceName) ++ "'" ++
    maybe "" (\x -> " in cluster " ++ (T.unpack . toText $ x)) clusterRef
  show (AmbiguousServiceName (AmbiguousServiceName' serviceName clusters)) =
    let stringifyClusters = intercalate "\n - " $ map (T.unpack . toText) clusters
    in "Service name '" ++
       (T.unpack serviceName) ++ "' is ambiguous. It was found in the following clusters:\n" ++
       stringifyClusters

instance Exception GrootError