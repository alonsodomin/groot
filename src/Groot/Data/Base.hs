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

filterM :: (MonadPlus m, FilterPredicate p)
        => p                     -- The actual predicate
        -> m (CanBeFilteredBy p) -- The item to which to apply the filter, wrapped in a Monad
        -> m (CanBeFilteredBy p) -- The result after applying the predicate, wrapped in the same Monad
filterM p = mfilter (matches p)

filterC :: (Monad m, FilterPredicate p)
        => p
        -> Conduit (CanBeFilteredBy p) m (CanBeFilteredBy p)
filterC p = CL.filter (matches p)