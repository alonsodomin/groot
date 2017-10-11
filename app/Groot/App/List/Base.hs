{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.App.List.Base where

import Data.Text
import Text.PrettyPrint.Tabulate

class HasSummary a b where
  summarize :: a -> Maybe b

printTable' :: Tabulate a => Text -> [a] -> IO ()
printTable' txt []       = putStrLn $ unpack txt
printTable' _   xs@(_:_) = ppTable xs