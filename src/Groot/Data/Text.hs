module Groot.Data.Text
     ( 
       module Data.Attoparsec.Text
     , module Network.AWS.Data.Text
     , subparser
     ) where

import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text             as T
import           Network.AWS.Data.Text hiding (takeText)

subparser :: Parser a -> Text -> Parser a
subparser m input = either (fromTextError . T.pack) return $ parseOnly m input