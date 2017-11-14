{-# LANGUAGE OverloadedStrings  #-}

module Groot.AWS.Base where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS
import Network.AWS.Data.Text hiding (takeText)

subparser :: Parser a -> Text -> Parser a
subparser m input = either (fromTextError . T.pack) return $ parseOnly m input

data ARN = ARN
  { _arnServiceId    :: Text
  , _arnRegion       :: Region
  , _arnAccountId    :: Text
  , _arnResourcePath :: Text
  } deriving (Eq, Show)

instance FromText ARN where
  parser = do
    string "arn:aws:"
    serviceId <- takeWhile (/= ':')
    char ':'
    region <- subparser parser =<< takeTill (== ':')
    char ':'
    account <- takeWhile (/= ':')
    char ':'
    path <- takeText
    return $ ARN serviceId region account path

instance ToText ARN where
  toText (ARN service region account path) =
    T.concat ["arn:aws:", service, ":", (toText region), ":", account, ":", path]