{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.AWS
     ( module Groot.Internal.AWS.Auth
     , module Groot.Internal.AWS.Cluster
     , module Groot.Internal.AWS.Images
     , module Groot.Internal.AWS.Instance
     , module Groot.Internal.AWS.Service
     , module Groot.Internal.AWS.Task
     , module Groot.Internal.AWS.TaskDef
     , module Groot.Internal.AWS.AutoScaling
     -- Resource summaries
     , instanceResourceUsage
     , instanceResourceSummary
     , clusterResourceSummary
     ) where

import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.Foldable
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                      as T
import           Network.AWS
import qualified Network.AWS.ECS                as ECS

import           Groot.Console
import           Groot.Internal.AWS.Auth
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
resourceSummary' f = (hashMapWith (view ruType)) . catMaybes <$> traverse f allResourceTypes

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
instanceResourceUsage resType inst = (mkResourceUsage resType) <$> instanceResourceUsage' resType inst

instanceResourceSummary :: ECS.ContainerInstance -> ResourceSummary
instanceResourceSummary inst = runIdentity $ resourceSummary' (\x -> Identity $ (flip instanceResourceUsage) inst x)

clusterResourceSummary :: (Foldable f, MonadAWS m) => f ResourceType -> ECS.Cluster -> m ResourceSummary
clusterResourceSummary rs cluster = maybe (return Map.empty) id $ (summarizeResources rs) <$> clusterRef
  where clusterRef = ClusterRef <$> cluster ^. ECS.cClusterName
        
        summarizeResources resTypes cref = fmap (Map.mapWithKey (\k v -> mkResourceUsage k $ mapPair getSum v))
           $ runConduit $ CL.sourceList (toList resTypes)
          .| (awaitForever $ \rt -> toProducer $ resourceConduit rt cref)
          .| hashMapWithC' fst snd

        resourceConduit rt cref = fetchInstances cref
          .| CL.mapMaybe (instanceResourceUsage' rt)
          .| CL.map (mapPair Sum)
          .| CL.map ((,) rt)

