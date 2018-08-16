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
     , instanceResourceUsage
     , clusterResourceUsage
     , instanceResourceSummary
     , clusterResourceSummary
     -- Error handlers
     , handleHttpException
     , handleServiceError
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.List
import           Data.Maybe
import           Data.Monoid
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
import           Groot.Internal.Util
import           Groot.Types

-- Resource Summaries

resourceSummary' :: Monad m => (ResourceType -> m (Maybe ResourceUsage)) -> m ResourceSummary
resourceSummary' f = (toHashMap (view ruType)) . catMaybes <$> traverse f [Memory, CPU]

instanceResourceUsage' :: ResourceType -> ECS.ContainerInstance -> Maybe (Int, Int)
instanceResourceUsage' resType inst = liftA2 (,) rUsed rTotal
  where rTotal     = findResource $ inst ^. ECS.ciRegisteredResources
        rRemaining = findResource $ inst ^. ECS.ciRemainingResources
        rUsed      = liftA2 (-) rTotal rRemaining

        findResource :: [ECS.Resource] -> Maybe Int
        findResource rs = (view ECS.rIntegerValue) =<< find (\x -> maybe False (== resName) $ x ^. ECS.rName) rs

        resName = case resType of
          Memory -> "MEMORY"
          CPU    -> "CPU"

instanceResourceUsage :: ResourceType -> ECS.ContainerInstance -> Maybe ResourceUsage
instanceResourceUsage resType inst = (mkResourceUsage  resType) <$> instanceResourceUsage' resType inst

instanceResourceSummary :: ECS.ContainerInstance -> ResourceSummary
instanceResourceSummary inst = runIdentity $ resourceSummary' (\x -> Identity $ (flip instanceResourceUsage) inst x)

clusterResourceUsage :: MonadAWS m => ResourceType -> ECS.Cluster -> m (Maybe ResourceUsage)
clusterResourceUsage resType cluster =
  runMaybeT $ (fmap (mkResourceUsage resType)) . fmap (mapPair getSum) <$> MaybeT $ sequence (summarizeResources <$> clusterRef)
  where clusterRef = clusterName cluster
        summarizeResources cref = runConduit $ fetchInstances cref
          .| CL.mapMaybe (instanceResourceUsage' resType)
          .| CL.map (mapPair Sum)
          .| CL.fold ((<>)) mempty

clusterResourceSummary :: MonadAWS m => ECS.Cluster -> m ResourceSummary
clusterResourceSummary cluster = resourceSummary' $ (flip clusterResourceUsage) cluster

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
