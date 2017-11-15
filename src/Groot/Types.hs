{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Groot.Types 
     ( ServiceId (..)
     , AccountId (..)
     , ARN (..)
     , arnAccountId
     , arnRegion
     , arnResourcePath
     , arnServiceId
     , AMI
     ) where

import           Prelude               hiding (takeWhile)
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Semigroup
import           Network.AWS
import           Network.AWS.Data.Text hiding (takeText)

subparser :: Parser a -> Text -> Parser a
subparser m input = either (fromTextError . T.pack) return $ parseOnly m input

-- | An AWS service identifier, typically used in AWS ARNs
data ServiceId =
    AutoScaling
  | ECS
  | EC2
  deriving (Eq, Show)

instance FromText ServiceId where
  parser = takeLowerText >>= \case
    "autoscaling" -> pure AutoScaling
    "ecs"         -> pure ECS
    "ec2"         -> pure EC2
    e             ->
      fromTextError $ "Failure parsing service id from " <> e

instance ToText ServiceId where
  toText AutoScaling = "autoscaling"
  toText ECS         = "ecs"
  toText EC2         = "ec2"

-- | An AWS account identifier
newtype AccountId = AccountId Text
  deriving (Eq, Show)

instance ToText AccountId where
  toText (AccountId s) = s

-- | An AWS Resource Name (ARN for short) used to uniquely identify 
-- a given resource
data ARN = ARN
  { _arnServiceId    :: ServiceId
  , _arnRegion       :: Region
  , _arnAccountId    :: AccountId
  , _arnResourcePath :: Text
  } deriving (Eq, Show)

makeLenses ''ARN

instance FromText ARN where
  parser = do
    "arn:aws:"
    serviceId <- subparser parser =<< takeTill (== ':')
    char ':'
    region <- subparser parser =<< takeTill (== ':')
    char ':'
    account <- takeWhile (/= ':')
    char ':'
    path <- takeText
    return $ ARN serviceId region (AccountId account) path

instance ToText ARN where
  toText (ARN service region account path) = T.concat [
      "arn:aws:"
    , (toText service)
    , ":"
    , (toText region)
    , ":"
    , (toText account)
    , ":"
    , path
    ]

-- | An AWS Machine Image, used to uniquely identify a given
-- image for an specific instance
newtype AMI = AMI Text
  deriving (Eq, Show)

instance FromText AMI where
  parser = do
    "ami-"
    ident <- takeText
    return $ AMI ident

instance ToText AMI where
  toText (AMI ident) = ident