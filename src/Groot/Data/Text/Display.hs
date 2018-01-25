{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Groot.Data.Text.Display
     ( Display (..)
     ) where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Network.AWS.Data.Text

class ToText a => Display a where
  display :: MonadIO m => a -> m ()

  displayLn :: MonadIO m => a -> m ()
  displayLn d = do
    display d
    liftIO $ T.putStrLn ""

instance Display Char where
  display = liftIO . print
  {-# INLINE display #-}

instance Display Text where
  display = liftIO . T.putStr
  {-# INLINE display #-}

instance Display String where
  display = display . T.pack
  {-# INLINE display #-}
