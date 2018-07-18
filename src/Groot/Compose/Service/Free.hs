{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Groot.Compose.Service.Free where

import           Control.Monad
import           Control.Monad.Free
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

type ServiceComposeM = Free ServiceComposeOp

deployService :: ClusterRef -> NamedServiceDeployment -> ServiceComposeM ()
deployService clusterRef service = do
  verifyActiveCluster clusterRef
  taskDefId <- registerTask service
  exists    <- serviceExists (fst service) clusterRef
  if exists
  then updateService service clusterRef taskDefId
  else createService service clusterRef taskDefId

deployServices :: Traversable f => ClusterRef -> f NamedServiceDeployment -> ServiceComposeM ()
deployServices clusterRef = void . traverse (deployService clusterRef)
{-# INLINE deployServices #-}

deleteServices :: Traversable f => ClusterRef -> f NamedServiceDeployment -> ServiceComposeM ()
deleteServices clusterRef = void . traverse (\serv -> removeService serv clusterRef)
{-# INLINE deleteServices #-}

replaceService :: ClusterRef -> NamedServiceDeployment -> ServiceComposeM ()
replaceService clusterRef service = do
  removeService service clusterRef
  deployService clusterRef service

replaceServices :: Traversable f => ClusterRef -> f NamedServiceDeployment -> ServiceComposeM ()
replaceServices clusterRef = void . traverse (replaceService clusterRef)
{-# INLINE replaceServices #-}