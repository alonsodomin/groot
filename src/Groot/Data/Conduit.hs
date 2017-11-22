{-# LANGUAGE RankNTypes #-}

module Groot.Data.Conduit where

import Control.Monad.Trans (lift)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Foldable       (toList)
import Data.List           (sortOn)

-- | Merge multiple sorted sources into one sorted producer.
mergeSources :: (Ord a, Foldable f, Monad m) => f (Source m a) -> Producer m a
mergeSources = mergeSourcesOn id

-- | Merge multiple sorted sources into one sorted producer using specified sorting key.
mergeSourcesOn :: (Ord b, Foldable f, Monad m) => (a -> b) -> f (Source m a) -> Producer m a
mergeSourcesOn key = mergeResumable . fmap newResumableSource . toList
  where
    mergeResumable sources = do
        prefetchedSources <- lift $ traverse ($$++ await) sources
        go [(a, s) | (s, Just a) <- prefetchedSources]

    go [] = pure ()
    go sources = do
        let (a, src1) : sources1 = sortOn (key . fst) sources
        yield a
        (src2, mb) <- lift $ src1 $$++ await
        let sources2 = case mb of
                Nothing -> sources1
                Just b  -> (b, src2) : sources1
        go sources2

mergeSourcesOn' :: (Ord b, Traversable t, Monad m) => (a -> b) -> t (Source m a) -> Source m a
mergeSourcesOn' key sources = sequenceSources sources =$= CL.map (\xs -> sortOn key $ toList xs) =$= CL.concat