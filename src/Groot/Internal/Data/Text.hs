{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Groot.Data.Text
Description : Generic text manipulation module
Copyright   : A. Alonso Dominguez (c) 2017, http://github.com/alonsodomin
License     : Apache 2.0
Maintainer  : A. Alonso Dominguez <alonso.domin (λ) google>
Stability   : experimental
Portability : portable

This module aggregates the main text manipulation functionality needed
in Groot to operate.
-}
module Groot.Internal.Data.Text
     (
       module Network.AWS.Data.Text
     , module Groot.Internal.Data.Text.Styled
     , uuid
     , subparser
     ) where

import           Data.Attoparsec.Text
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.UUID                       (UUID)
import qualified Data.UUID                       as UUID
import           Network.AWS.Data.Text           hiding (takeText)

import           Groot.Internal.Data.Text.Styled

-- |Text parser for 'UUID's
uuid :: Parser UUID
uuid = do
  input <- takeText
  case (UUID.fromText input) of
    Nothing -> fromTextError $ T.append "Invalid UUID: " input
    Just x  -> return x

-- |Utility to compose text parsers
subparser :: FromText a => Text -> Parser a
subparser input = either (fromTextError . T.pack) return $ parseOnly parser input
