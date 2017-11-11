{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Groot.Exception where

import Control.Exception.Lens
import Control.Lens
import Control.Monad.Catch hiding (Handler)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Groot.Data
import Network.AWS.Data.Text

data GrootError = forall e. Exception e => GrootError e

instance Show GrootError where
  show (GrootError e) = show e

instance Exception GrootError

grootErrorToException :: Exception e => e -> SomeException
grootErrorToException = toException . GrootError

grootErrorFromException :: Exception e => SomeException -> Maybe e
grootErrorFromException x = do
  GrootError e <- fromException x
  cast e

  --   ClusterNotFound ClusterNotFound
  -- | InstanceNotFound InstanceNotFound
  -- | TaskNotFound TaskNotFound
  -- | ServiceNotFound ServiceNotFound
  -- | AmbiguousServiceName AmbiguousServiceName
  -- deriving (Eq, Typeable, Show)

data ClusterNotFound = ClusterNotFound ClusterRef
  deriving (Eq, Typeable, Show)

clusterNotFound :: ClusterRef -> SomeException
clusterNotFound = toException . GrootError . ClusterNotFound

instance Exception ClusterNotFound

class AsClusterNotFoundException t where
  _ClusterNotFound :: Prism' t ClusterNotFound

instance AsClusterNotFoundException ClusterNotFound where
  _ClusterNotFound = id

instance AsClusterNotFoundException SomeException where
  _ClusterNotFound = exception

-- | Instance Exceptions

data InstanceNotFound = InstanceNotFound InstanceRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

instanceNotFound :: InstanceRef -> Maybe ClusterRef -> SomeException
instanceNotFound instanceRef clusterRef =
  toException . GrootError $ InstanceNotFound instanceRef clusterRef

instance Exception InstanceNotFound

data TaskNotFound = TaskNotFound TaskRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

taskNotFound :: TaskRef -> Maybe ClusterRef -> SomeException
taskNotFound taskRef clusterRef =
  toException . GrootError $ TaskNotFound taskRef clusterRef

instance Exception TaskNotFound

data ServiceNotFound = ServiceNotFound ServiceRef (Maybe ClusterRef)
  deriving (Eq, Typeable, Show)

serviceNotFound :: ServiceRef -> Maybe ClusterRef -> SomeException
serviceNotFound serviceName clusterRef =
  toException . GrootError $ ServiceNotFound serviceName clusterRef

instance Exception ServiceNotFound

data AmbiguousServiceName = AmbiguousServiceName Text [ClusterRef]
  deriving (Eq, Typeable, Show)

ambiguousServiceName :: Text -> [ClusterRef] -> SomeException
ambiguousServiceName serviceName clusters =
  toException . GrootError $ AmbiguousServiceName serviceName clusters

instance Exception AmbiguousServiceName

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