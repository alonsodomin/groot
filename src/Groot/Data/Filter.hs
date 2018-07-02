{-# LANGUAGE TypeFamilies #-}

module Groot.Data.Filter
     ( Filter(..)
     , toFilter
     , IsFilter(..)
     , filterM
     , filterOnM
     , filterC
     , filterOnC
     , (&&&), (|||), (!!!)
     ) where

import           Control.Monad         hiding (filterM)
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Functor.Identity

class IsFilter a where
  type FilterItem a :: *

  matches :: a -> FilterItem a -> Bool

  maybeMatchesM :: Monad m => a -> m (FilterItem a) -> m (Maybe (FilterItem a))
  maybeMatchesM p me = do
    e <- me
    return $ if (matches p e) then (Just e) else Nothing

  maybeMatches :: a -> FilterItem a -> Maybe (FilterItem a)
  maybeMatches p e = runIdentity $ maybeMatchesM p (Identity e)

(|||) :: IsFilter a => Filter a -> Filter a -> Filter a
(|||) x y = Or x y

(&&&) :: IsFilter a => Filter a -> Filter a -> Filter a
(&&&) x y = And x y

(!!!) :: IsFilter a => Filter a -> Filter a
(!!!) x = Not x

data Filter a =
    Single a
  | Or (Filter a) (Filter a)
  | And (Filter a) (Filter a)
  | Not (Filter a)
  deriving (Eq, Show)

toFilter :: IsFilter a => a -> Filter a
toFilter = Single

instance Functor Filter where
  fmap f (Single x) = Single (f x)
  fmap f (Or x y)   = Or (fmap f x) (fmap f y)
  fmap f (And x y)  = And (fmap f x) (fmap f y)
  fmap f (Not x)    = Not (fmap f x)

instance IsFilter a => IsFilter (Filter a) where
  type FilterItem (Filter a) = FilterItem a

  matches (Single x) res = matches x res
  matches (Or x y)   res = (matches x res) || (matches y res)
  matches (And x y)  res = (matches x res) && (matches y res)
  matches (Not x)    res = not (matches x res)

filterOnM :: (MonadPlus m, IsFilter p)
          => (a -> FilterItem p)
          -> p                        -- The actual predicate
          -> m a                      -- The item to which to apply the filter, wrapped in a Monad
          -> m a                      -- The result after applying the predicate, wrapped in the same Monad
filterOnM f p = mfilter (\x -> matches p $ f x)
{-# INLINE filterOnM #-}

filterM :: (MonadPlus m, IsFilter p)
        => p
        -> m (FilterItem p)
        -> m (FilterItem p)
filterM = filterOnM id
{-# INLINE filterM #-}

filterC :: (Monad m, IsFilter p)
        => p
        -> ConduitT (FilterItem p) (FilterItem p) m ()
filterC = filterOnC id
{-# INLINE filterC #-}

filterOnC :: (Monad m, IsFilter p)
          => (a -> FilterItem p)
          -> p
          -> ConduitT a a m ()
filterOnC f p = CL.filter (\x -> matches p $ f x)
{-# INLINE filterOnC #-}
