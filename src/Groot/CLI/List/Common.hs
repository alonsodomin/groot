{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.CLI.List.Common where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Groot.Data.Text
import           Text.PrettyPrint.Tabulate

class HasSummary a b where
  summarize :: a -> Maybe b

asString :: ToText a => a -> String
asString = T.unpack . toText

printTable' :: Tabulate a => Text -> [a] -> IO ()
printTable' txt []       = putStrLn $ T.unpack txt
printTable' _   xs@(_:_) = ppTable xs
