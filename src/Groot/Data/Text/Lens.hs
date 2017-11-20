{-# LANGUAGE RankNTypes #-}

module Groot.Data.Text.Lens where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Groot.Data.Text

orEmpty :: ToText a => Getter (Maybe a) Text
orEmpty = to (\x -> maybe T.empty toText x)