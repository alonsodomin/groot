{-# LANGUAGE FlexibleInstances #-}

module Groot.Data.Text.Display 
     ( Display (..)
     ) where

import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

class Display a where
  display :: MonadIO m => a -> m ()

instance Display Char where
  display = liftIO . print
  {-# INLINE display #-}

instance Display Text where
  display = liftIO . T.putStr
  {-# INLINE display #-}

instance (Foldable f, Display a) => Display (f a) where
  display xs = forM_ xs display
  {-# INLINE display #-}

instance Display String where
  display = display . T.pack
  {-# INLINE display #-}
