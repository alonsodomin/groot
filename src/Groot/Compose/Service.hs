{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Groot.Compose.Service
     ( ServiceComposeM
     , ServiceComposeCfg(..)
     , RunMode(..)
     , deployService
     , deployServices
     , deleteServices
     , interpretServiceComposeM
     ) where

import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Groot.Compose.Service.AWS    (awsServiceCompose)
import           Groot.Compose.Service.DryRun
import           Groot.Compose.Service.Free
import           Groot.Console
import           Groot.Core
import           Groot.Manifest
import           Groot.Types

data RunMode = DryRun | Unattended
  deriving (Eq, Show, Enum, Ord)

_DryRun :: Prism' RunMode Bool
_DryRun = prism (const DryRun) $ \case
  DryRun -> Right True
  x      -> Left x

_Unattended :: Prism' RunMode Bool
_Unattended = prism (const Unattended) $ \case
  Unattended -> Right True
  x          -> Left x

data ServiceComposeCfg = ServiceComposeCfg
  { composeManifest :: GrootManifest
  , composeCluster  :: ClusterRef
  , composeServices :: [NamedServiceDeployment]
  , composeRunMode  :: Maybe RunMode
  } deriving (Eq, Show)

interpretServiceComposeM :: Text
                         -> ServiceComposeM ()
                         -> ServiceComposeCfg
                         -> GrootM IO ()
interpretServiceComposeM userMsg action cfg =
  let shouldConfirm   = maybe True (isn't _Unattended) (composeRunMode cfg)
      isDryRun        = maybe False (\x -> not $ isn't _DryRun x) (composeRunMode cfg)
      confirmMsg srvs = userMsg <> "\n"
                     <> (T.intercalate "\n" $ T.append "   - " . fst <$> srvs)
                     <> ".\nDo you want to continue? "
      interpret       = if isDryRun then dryRunServiceCompose else awsServiceCompose
      performFor srvs =
        if shouldConfirm
        then askUserToContinue (confirmMsg srvs) $ interpret (composeManifest cfg) action
        else interpret (composeManifest cfg) action
  in hoist runResourceT $ performFor (composeServices cfg)
