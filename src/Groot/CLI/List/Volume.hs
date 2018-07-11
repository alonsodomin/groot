{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Groot.CLI.List.Volume
     ( ListVolumeOpts
     , listVolumeOpts
     , printVolumesSummary
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Data
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Network.AWS
import qualified Network.AWS.ECS           as ECS
import           Options.Applicative
import           Text.PrettyPrint.Tabulate (Tabulate, printTable)
import qualified Text.PrettyPrint.Tabulate as Tabs

import           Groot.CLI.Common
import           Groot.CLI.List.Common
import           Groot.Console
import           Groot.Core
import           Groot.Types

data ListVolumeOpts = ListVolumeOpts
  { _cluserRef :: Maybe ClusterRef
  } deriving (Eq, Show)

listVolumeOpts :: Parser ListVolumeOpts
listVolumeOpts = ListVolumeOpts <$> (optional clusterOpt)

data VolumeSummary = VolumeSummary
  { name   :: String
  , source :: String
  } deriving (Eq, Show, Generic, Data)

instance HasSummary ECS.Volume VolumeSummary where
  summarize volume = VolumeSummary <$> vName <*> vSource
    where vName   = T.unpack <$> volume ^. ECS.vName
          vSource = T.unpack <$> (volume ^. ECS.vHost >>= (view ECS.hvpSourcePath))

summarizeVolumes :: AWS [VolumeSummary]
summarizeVolumes = sourceToList $ fetchTaskDefs [] .| CL.concatMap (view ECS.tdVolumes) .| CL.mapMaybe summarize

printVolumesSummary :: ListVolumeOpts -> GrootIO ()
printVolumesSummary _ = runGrootResource $ do
  desc <- awsResource $ summarizeVolumes
  case desc of
    [] -> putWarn ("No volumes found" :: Text)
    xs -> liftIO $ printTable xs
