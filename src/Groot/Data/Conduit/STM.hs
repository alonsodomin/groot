module Groot.Data.Conduit.STM where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Conduit

-- | Creates a Conduit Source from an async channel
chanSource :: MonadIO m
           => chan
           -> (chan -> STM (Maybe a))
           -> (chan -> IO ())
           -> Source m a
chanSource ch reader closer = loop
  where loop = do
          a <- liftIO . atomically $ reader ch
          case a of
            Just x  -> yieldOr x close >> loop
            Nothing -> return ()

        close = liftIO $ closer ch
