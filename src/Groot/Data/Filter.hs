{-# LANGUAGE TypeFamilies #-}

module Groot.Data.Filter where

import           Control.Monad
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Functor.Identity

data FilterOp a =
    Or a a
  | And a a
  | Not a
  deriving (Eq, Show)

class Filter a where
  type FilterItem a :: *

  matches :: a -> FilterItem a -> Bool

  maybeMatchesM :: Monad m => a -> m (FilterItem a) -> m (Maybe (FilterItem a))
  maybeMatchesM p me = do
    e <- me
    return $ if (matches p e) then (Just e) else Nothing

  maybeMatches :: a -> FilterItem a -> Maybe (FilterItem a)
  maybeMatches p e = runIdentity $ maybeMatchesM p (Identity e)

  (|||) :: a -> a -> FilterOp a
  (|||) x y = Or x y

  (&&&) :: a -> a -> FilterOp a
  (&&&) x y = And x y

  notP :: a -> FilterOp a
  notP x = Not x

instance Filter a => Filter (FilterOp a) where
  type FilterItem (FilterOp a) = FilterItem a

  matches (Or x y)  res = (matches x res) || (matches y res)
  matches (And x y) res = (matches x res) && (matches y res)
  matches (Not x)   res = not (matches x res)

filterOnM :: (MonadPlus m, Filter p)
          => (a -> FilterItem p)
          -> p                        -- The actual predicate
          -> m a                      -- The item to which to apply the filter, wrapped in a Monad
          -> m a                      -- The result after applying the predicate, wrapped in the same Monad
filterOnM f p = mfilter (\x -> matches p $ f x)

filterM :: (MonadPlus m, Filter p)
        => p
        -> m (FilterItem p)
        -> m (FilterItem p)
filterM = filterOnM id

filterC :: (Monad m, Filter p)
        => p
        -> ConduitT (FilterItem p) (FilterItem p) m ()
filterC = filterOnC id

filterOnC :: (Monad m, Filter p)
          => (a -> FilterItem p)
          -> p
          -> ConduitT a a m ()
filterOnC f p = CL.filter (\x -> matches p $ f x)
