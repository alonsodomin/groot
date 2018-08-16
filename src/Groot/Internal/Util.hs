module Groot.Internal.Util where

import           Control.Arrow       ((***))
import           Control.Monad       (join)
import           Data.Conduit
import qualified Data.Conduit.List   as CL
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Semigroup

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = join (***)

hashMapWith' :: (Hashable k, Eq k, Semigroup b, Functor f, Foldable f) => (a -> k) -> (a -> b) -> f a -> HashMap k b
hashMapWith' f g xs = foldr (Map.unionWith (<>)) Map.empty $ fmap (\x -> Map.singleton (f x) (g x)) xs

hashMapWith :: (Hashable k, Eq k, Semigroup a, Functor f, Foldable f) => (a -> k) -> f a -> HashMap k a
hashMapWith f xs = hashMapWith' f id xs

hashMapWithC' :: (Monad m, Hashable k, Eq k, Semigroup b) => (a -> k) -> (a -> b) -> ConduitT a o m (HashMap k b)
hashMapWithC' f g = awaitForever (\x -> toProducer . yield $ Map.singleton (f x) (g x))
  .| CL.fold (Map.unionWith (<>)) Map.empty

hashMapWithC :: (Monad m, Hashable k, Eq k, Semigroup a) => (a -> k) -> ConduitT a o m (HashMap k a)
hashMapWithC f = hashMapWithC' f id

groupByKey :: (Hashable k, Eq k) => (a -> k) -> [a] -> HashMap k [a]
groupByKey f xs = hashMapWith (\x -> f $ head x) $ (\x -> [x]) <$> xs
