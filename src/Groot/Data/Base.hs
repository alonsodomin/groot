{-# LANGUAGE TypeFamilies #-}

module Groot.Data.Base where

import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as CL

data FilterOp a =
    Or a a
  | And a a
  | Not a
  deriving (Eq, Show)

class FilterPredicate a where
  type CanBeFilteredBy a :: *

  matches :: a -> CanBeFilteredBy a -> Bool

  (|||) :: a -> a -> FilterOp a
  (|||) x y = Or x y

  (&&&) :: a -> a -> FilterOp a
  (&&&) x y = And x y

  notP :: a -> FilterOp a
  notP x = Not x

instance FilterPredicate a => FilterPredicate (FilterOp a) where
  type CanBeFilteredBy (FilterOp a) = CanBeFilteredBy a

  matches (Or x y)  res = (matches x res) || (matches y res)
  matches (And x y) res = (matches x res) && (matches y res)
  matches (Not x)   res = not (matches x res)

filterOnM :: (MonadPlus m, FilterPredicate p)
          => (a -> CanBeFilteredBy p)
          -> p                        -- The actual predicate
          -> m a                      -- The item to which to apply the filter, wrapped in a Monad
          -> m a                      -- The result after applying the predicate, wrapped in the same Monad
filterOnM f p = mfilter (\x -> matches p $ f x)

filterM :: (MonadPlus m, FilterPredicate p)
        => p
        -> m (CanBeFilteredBy p)
        -> m (CanBeFilteredBy p)
filterM = filterOnM id

filterC :: (Monad m, FilterPredicate p)
        => p
        -> Conduit (CanBeFilteredBy p) m (CanBeFilteredBy p)
filterC = filterOnC id

filterOnC :: (Monad m, FilterPredicate p)
          => (a -> CanBeFilteredBy p)
          -> p
          -> Conduit a m a
filterOnC f p = CL.filter (\x -> matches p $ f x)