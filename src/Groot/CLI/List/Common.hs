{-# LANGUAGE MultiParamTypeClasses #-}

module Groot.CLI.List.Common where

import qualified Data.Text       as T
import           Groot.Data.Text

class HasSummary a b where
  summarize :: a -> Maybe b

asString :: ToText a => a -> String
asString = T.unpack . toText
