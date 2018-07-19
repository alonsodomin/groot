{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Groot.Compose.Service.Free where

import           Control.Monad
import Control.Monad.Identity
import           Control.Monad.Trans.Free
import           Control.Monad.Free.TH
import           Data.Text             (Text)

import           Groot.Manifest
import           Groot.Types

data ServiceComposeOp next =
    RegisterTask NamedServiceDeployment (TaskDefId -> next)
  | ServiceExists Text ClusterRef (Bool -> next)
  | CreateService NamedServiceDeployment ClusterRef TaskDefId next
  | UpdateService NamedServiceDeployment ClusterRef TaskDefId next
  | RemoveService NamedServiceDeployment ClusterRef next
  | VerifyActiveCluster ClusterRef next
  deriving Functor

makeFree ''ServiceComposeOp

type ServiceComposeT m = FreeT ServiceComposeOp m
type ServiceComposeM = ServiceComposeT IO

deployService :: Monad m => ClusterRef -> NamedServiceDeployment -> ServiceComposeT m ()
deployService clusterRef service = do
  verifyActiveCluster clusterRef
  taskDefId <- registerTask service
  exists    <- serviceExists (fst service) clusterRef
  if exists
  then updateService service clusterRef taskDefId
  else createService service clusterRef taskDefId

deployServices :: (Monad m, Traversable f) => ClusterRef -> f NamedServiceDeployment -> ServiceComposeT m ()
deployServices clusterRef = void . traverse (\serv -> deployService clusterRef serv)
{-# INLINE deployServices #-}

deleteServices :: (Monad m, Traversable f) => ClusterRef -> f NamedServiceDeployment -> ServiceComposeT m ()
deleteServices clusterRef = void . traverse (\serv -> removeService serv clusterRef)
{-# INLINE deleteServices #-}

replaceService :: ClusterRef -> NamedServiceDeployment -> ServiceComposeM ()
replaceService clusterRef service = do
  removeService service clusterRef
  deployService clusterRef service

replaceServices :: Traversable f => ClusterRef -> f NamedServiceDeployment -> ServiceComposeM ()
replaceServices clusterRef = void . traverse (replaceService clusterRef)
{-# INLINE replaceServices #-}
