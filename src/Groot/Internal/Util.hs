module Groot.Internal.Util where

import           Control.Arrow       ((***))
import           Control.Monad       (join)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = join (***)

toHashMap :: (Hashable k, Eq k) => (a -> k) -> [a] -> HashMap k a
toHashMap f xs = Map.fromList $ (\x -> (f x, x)) <$> xs
