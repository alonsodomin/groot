{-# LANGUAGE OverloadedStrings #-}

module Groot.Data.Text
     (
       module Data.Attoparsec.Text
     , module Network.AWS.Data.Text
     , uuid
     , subparser
     ) where

import           Data.Attoparsec.Text
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID
import           Network.AWS.Data.Text hiding (takeText)

uuid :: Parser UUID
uuid = do
  input <- takeText
  case (UUID.fromText input) of
    Nothing -> fromTextError $ T.append "Invalid UUID: " input
    Just x  -> return x

subparser :: FromText a => Text -> Parser a
subparser input = either (fromTextError . T.pack) return $ parseOnly parser input
