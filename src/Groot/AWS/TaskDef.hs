module Groot.AWS.TaskDef where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Network.AWS
import qualified Network.AWS.ECS           as ECS

import           Groot.Types

taskDefId :: ECS.TaskDefinition -> Maybe TaskDefId
taskDefId taskDef = TaskDefId <$> (TaskFamily <$> taskDef ^. ECS.tdFamily) <*> (taskDef ^. ECS.tdRevision)

getTaskDef :: MonadAWS m => TaskDefRef -> MaybeT m ECS.TaskDefinition
getTaskDef (TaskDefRef arn) = MaybeT $ do
  res <- send $ ECS.describeTaskDefinition arn
  return $ res ^. ECS.desrsTaskDefinition

taskDefFromTask :: MonadAWS m => ECS.Task -> MaybeT m ECS.TaskDefinition
taskDefFromTask tsk = do
  arn <- MaybeT . return $ TaskDefRef <$> tsk ^. ECS.tTaskDefinitionARN
  getTaskDef arn

fetchTaskDefs :: (MonadAWS m, Foldable f) => f TaskDefFilter -> Source m ECS.TaskDefinition
fetchTaskDefs filters =
  let tds :: TaskDefStatus -> ECS.TaskDefinitionStatus
      tds TDSActive   = ECS.TDSActive
      tds TDSInactive = ECS.TDSInactive

      withFilter :: TaskDefFilter -> ECS.ListTaskDefinitions -> ECS.ListTaskDefinitions
      withFilter (TDFFamily (TaskFamily f)) = ECS.ltdFamilyPrefix ?~ f
      withFilter (TDFStatus s)              = ECS.ltdStatus ?~ (tds s)

  in paginate (foldr withFilter ECS.listTaskDefinitions filters)
     =$= CL.concatMap (view ECS.ltdrsTaskDefinitionARNs)
     =$= CL.map TaskDefRef
     =$= CL.mapMaybeM (\x -> runMaybeT (getTaskDef x))
