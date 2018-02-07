{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Groot.Compose.Service.Free where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Text                   (Text)

import           Groot.Compose.Service.Model
import           Groot.Types

data ServiceComposeOp next =
    RegisterTask NamedServiceDeployment (TaskDefId -> next)
  | ServiceExists Text ClusterRef (Bool -> next)
  | CreateService NamedServiceDeployment ClusterRef TaskDefId next
  | UpdateService NamedServiceDeployment ClusterRef TaskDefId next
  | VerifyActiveCluster ClusterRef next
  deriving Functor

makeFree ''ServiceComposeOp

type ServiceComposeM = Free ServiceComposeOp

deployService :: ClusterRef -> NamedServiceDeployment -> ServiceComposeM ()
deployService clusterRef service = do
  taskDefId <- registerTask service
  exists    <- serviceExists (fst service) clusterRef
  if exists
  then updateService service clusterRef taskDefId
  else createService service clusterRef taskDefId

deployServices :: Traversable f => ClusterRef -> f NamedServiceDeployment -> ServiceComposeM ()
deployServices clusterRef = void . traverse (\serv -> deployService clusterRef serv)
{-# INLINE deployServices #-}
