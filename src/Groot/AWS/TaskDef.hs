module Groot.AWS.TaskDef where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Groot.Data
import           Network.AWS
import qualified Network.AWS.ECS           as ECS

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
      tds TaskActive   = ECS.TDSActive
      tds TaskInactive = ECS.TDSInactive

      withFilter :: TaskDefFilter -> ECS.ListTaskDefinitions -> ECS.ListTaskDefinitions
      withFilter (FamilyFilter (TaskFamily f)) = ECS.ltdFamilyPrefix ?~ f
      withFilter (StatusFilter s)              = ECS.ltdStatus ?~ (tds s)

  in paginate (foldr withFilter ECS.listTaskDefinitions filters)
     =$= CL.concatMap (view ECS.ltdrsTaskDefinitionARNs)
     =$= CL.map TaskDefRef
     =$= CL.mapMaybeM (\x -> runMaybeT (getTaskDef x))
