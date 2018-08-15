{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.AWS
     ( module Groot.Internal.AWS.Cluster
     , module Groot.Internal.AWS.Images
     , module Groot.Internal.AWS.Instance
     , module Groot.Internal.AWS.Service
     , module Groot.Internal.AWS.Task
     , module Groot.Internal.AWS.TaskDef
     , module Groot.Internal.AWS.AutoScaling
     -- Resource summaries
     , instanceResourceSummary
     , clusterResourceSummary
     -- Error handlers
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.List
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
import           Network.AWS
import qualified Network.AWS.ECS                as ECS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           Groot.Console
import           Groot.Internal.AWS.AutoScaling
import           Groot.Internal.AWS.Cluster
import           Groot.Internal.AWS.Images
import           Groot.Internal.AWS.Instance
import           Groot.Internal.AWS.Service
import           Groot.Internal.AWS.Task
import           Groot.Internal.AWS.TaskDef
import           Groot.Internal.Data.Text
import           Groot.Types

-- Resource Summaries

instanceResourceSummary :: ResourceType -> ECS.ContainerInstance -> Maybe ResourceSummary
instanceResourceSummary resType inst = (ResourceSummary resType) <$> rUsed <*> rTotal
  where rTotal     = findResource $ inst ^. ECS.ciRegisteredResources
        rRemaining = findResource $ inst ^. ECS.ciRemainingResources
        rUsed      = liftA2 (-) rTotal rRemaining
    
        findResource :: [ECS.Resource] -> Maybe Int
        findResource rs = (view ECS.rIntegerValue) =<< find (\x -> maybe False (== resName) $ x ^. ECS.rName) rs

        resName = case resType of
          Memory -> "MEMORY"
          CPU    -> "CPU"

clusterResourceSummary :: MonadAWS m => ResourceType -> ECS.Cluster -> m ResourceSummary
clusterResourceSummary resType cluster = maybe (return emptySummary) id (summarizeResources <$> clusterRef)
  where summarizeResources :: MonadAWS m => ClusterRef -> m ResourceSummary
        summarizeResources cref = runConduit $ fetchInstances cref
          .| CL.mapMaybe (instanceResourceSummary resType)
          .| CL.fold sumSummaries emptySummary
    
        clusterRef = clusterName cluster
        sumSummaries (ResourceSummary _ leftPart leftTotal) (ResourceSummary _ rightPart rightTotal) =
          ResourceSummary resType (leftPart + rightPart) (leftTotal + rightTotal)
        emptySummary = ResourceSummary resType 0 0

-- AWS Error handlers

handleHttpException :: MonadConsole m => HttpException -> m ()
handleHttpException (InvalidUrlException url reason) =
  putError $ "Url " <> (styled yellowStyle $ toText url) <> " is invalid due to: " <> (styled redStyle $ toText reason)
handleHttpException (HttpExceptionRequest req _) =
  putError $ "Could not communicate with " <> (styled yellowStyle $ toText . host $ req) <> "."

handleServiceError :: MonadConsole m => ServiceError -> m ()
handleServiceError err =
  let servName  = toText $ err ^. serviceAbbrev
      statusMsg = toText . statusMessage $ err ^. serviceStatus
      message   = maybe "" toText $ err ^. serviceMessage
      styledSt  = styled yellowStyle (T.concat [servName, " ", statusMsg])
  in putError $ styledSt <+> (styleless message)
