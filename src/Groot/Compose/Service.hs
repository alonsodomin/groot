{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Groot.Compose.Service
     ( ServiceComposeM
     , ServiceComposeCfg(..)
     , RunFlag(..)
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

data RunFlag = DryRun | Unattended
  deriving (Eq, Show, Enum, Ord)

_DryRun :: Prism' RunFlag Bool
_DryRun = prism (const DryRun) $ \case
  DryRun -> Right True
  x      -> Left x

_Unattended :: Prism' RunFlag Bool
_Unattended = prism (const Unattended) $ \case
  Unattended -> Right True
  x          -> Left x

data ServiceComposeCfg = ServiceComposeCfg
  { composeManifest :: GrootManifest
  , composeCluster  :: ClusterRef
  , composeServices :: [NamedServiceDeployment]
  , composeRunFlags :: [RunFlag]
  } deriving (Eq, Show)

interpretServiceComposeM :: Text
                         -> ServiceComposeM ()
                         -> ServiceComposeCfg
                         -> GrootM IO ()
interpretServiceComposeM userMsg action cfg =
  let hasRunFlag flag = flag `elem` (composeRunFlags cfg)
      shouldConfirm   = not $ hasRunFlag Unattended
      isDryRun        = hasRunFlag DryRun
      confirmMsg srvs = userMsg <> "\n"
                     <> (T.intercalate "\n" $ T.append "   - " . fst <$> srvs)
                     <> ".\nDo you want to continue? "
      interpret       =
        let execute = if isDryRun then dryRunServiceCompose else awsServiceCompose
        in execute (composeManifest cfg) action
      performFor srvs =
        if shouldConfirm
        then askUserToContinue (confirmMsg srvs) $ interpret
        else interpret
  in hoist runResourceT $ performFor (composeServices cfg)
