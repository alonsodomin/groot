{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Groot.Exception
     ( 
       GrootError(..)
     , _GrootError
     , module Groot.Exception.Cluster
     , module Groot.Exception.Instance
     , module Groot.Exception.Task
     , module Groot.Exception.Service
     ) where

import Control.Exception.Lens
import Control.Lens
import Control.Monad.Catch hiding (Handler)
import Data.Typeable
import Groot.Data
import Groot.Exception.Cluster
import Groot.Exception.Instance
import Groot.Exception.Task
import Groot.Exception.Service

data GrootError =
    ClusterException ClusterException
  | InstanceException InstanceException
  | TaskException TaskException
  | ServiceException ServiceException
  deriving (Eq, Typeable, Show)

instance Exception GrootError

class AsGrootError t where
  _GrootError :: Prism' t GrootError
  {-# MINIMAL _GrootError #-}

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