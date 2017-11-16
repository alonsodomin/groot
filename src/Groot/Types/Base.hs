{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Groot.Types.Base 
     ( ServiceId (..)
     , AccountId (..)
     , Arn (..)
     , arnAccountId
     , arnRegion
     , arnResourcePath
     , arnServiceId
     , Ami (..)
     ) where

import           Prelude               hiding (takeWhile)
import           Control.Lens
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Semigroup
import           Data.String
import           GHC.Generics
import           Groot.Data.Text
import           Network.AWS

-- | An AWS service identifier, typically used in AWS ARNs
data ServiceId =
    AutoScaling
  | ECS
  | EC2
  deriving (Eq, Show, Generic, Enum, Bounded)

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

instance IsString AccountId where
  fromString = AccountId . T.pack

instance ToText AccountId where
  toText (AccountId s) = s

-- | An AWS Resource Name (ARN for short) used to uniquely identify 
-- a given resource
data Arn a = Arn
  { _arnServiceId    :: ServiceId
  , _arnRegion       :: Region
  , _arnAccountId    :: AccountId
  , _arnResourcePath :: a
  } deriving (Eq, Show)

makeLenses ''Arn

instance FromText a => FromText (Arn a) where
  parser = do
    "arn:aws:"
    serviceId <- subparser parser =<< takeTill (== ':')
    char ':'
    region <- subparser parser =<< takeTill (== ':')
    char ':'
    account <- takeWhile (/= ':')
    char ':'
    path <- parser
    return $ Arn serviceId region (AccountId account) path

instance ToText a => ToText (Arn a) where
  toText (Arn service region account path) = T.concat [
      "arn:aws:"
    , toText service
    , ":"
    , toText region
    , ":"
    , toText account
    , ":"
    , toText path
    ]

-- | An AWS Machine Image, used to uniquely identify a given
-- image for an specific instance
newtype Ami = Ami Text
  deriving (Eq, Show)

instance FromText Ami where
  parser = do
    "ami-"
    ident <- takeText
    return $ Ami ident

instance ToText Ami where
  toText (Ami ident) = T.append "ami-" ident