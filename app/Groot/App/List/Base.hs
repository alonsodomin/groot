{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Groot.App.List.Base where

import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Tabulate

-- pprintSink :: (Traversable t, SummaryAttr a) => t a -> Sink [AttrResource a] IO ()
-- pprintSink attrs = transPipe (\x -> evalStateT x False) $ pprintSink' attrs

-- pprintSink' :: (Traversable t, SummaryAttr a) => t a -> Sink [AttrResource a] (StateT Bool IO) ()
-- pprintSink' attrs = do
--   mitems <- await
--   case mitems of
--     Nothing    -> return ()
--     Just items -> do
--       hasHeaders <- get
--       if (not hasHeaders)
--       then do
--         hdrs <- state (\_ -> (ppheaders attrs, True))
--         rows <- return $ pprintResources attrs items
--         liftIO . PP.putDoc $ hdrs <$$> rows        
--       else liftIO . PP.putDoc $ pprintResources attrs items
      --pprintSink attrs

class HasSummary a b where
  summarize :: a -> Maybe b

printTable' :: Tabulate a => Text -> [a] -> IO ()
printTable' txt []       = putStrLn $ T.unpack txt
printTable' _   xs@(_:_) = ppTable xs