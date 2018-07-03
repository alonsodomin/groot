{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.Display
     ( Display (..)
     ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.ANSI

import           Groot.Internal.Data.Text

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

instance Display StyledText where
  display (TextSpan style txt) = liftIO $ do
    setSGR style
    T.putStr txt
  display (TextBlock xs) =
    forM_ xs display
