{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Groot.CLI
Description : Command Line Interface for Groot
Copyright   : A. Alonso Dominguez (c) 2017, http://github.com/alonsodomin
License     : Apache 2.0
Maintainer  : A. Alonso Dominguez <alonso.domin (λ) google>
Stability   : experimental
Portability : portable

This is the main application module, application's 'main' function hooks into this
via the 'grootCli' function and applications interested on embedding some of Groot's
features can use some of the more fain grained functions exposed via this module.
-}
module Groot.CLI
     ( GrootOpts
     , GrootCmd
     , loadEnv
     , grootOpts
     , grootCommand
     , execGrootCmd
     ) where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Semigroup             ((<>))
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.AWS                hiding (Debug)
import qualified Network.AWS                as AWS
import           Options.Applicative
import           System.IO

import           Groot.CLI.Auth
import           Groot.CLI.Cluster
import           Groot.CLI.Common
import           Groot.CLI.Introspect
import           Groot.CLI.List
import           Groot.CLI.Service
import           Groot.Config
import           Groot.Console
import           Groot.Core
import           Groot.Exception
import           Groot.Internal.Data.Text
import           Groot.Manifest
import           Groot.Session
import           Groot.Types

-- |Top level Groot commands
data GrootCmd =
    ClusterCmd ClusterSubCmd
  | ListCmd ListSubCmd
  | ServiceCmd ServiceSubCmd
  | IntrospectCmd IntrospectOpts
  deriving (Eq, Show)

-- |Groot options for a given execution
data GrootOpts = GrootOpts
  { _grootCreds   :: Credentials
  , _grootRegion  :: Maybe Region
  , _grootSession :: Maybe SessionAuth
  , _grootDebug   :: Bool
  } deriving Eq

makeLenses ''GrootOpts

introspectCmd :: Parser GrootCmd
introspectCmd = IntrospectCmd <$> introspectOpts

grootCommand :: Parser GrootCmd
grootCommand = hsubparser
   ( command "ls"         (info (ListCmd    <$> listCmds)    (progDesc "List ECS resources"))
  <> command "cluster"    (info (ClusterCmd <$> clusterCmds) (progDesc "Perform cluster related operations"))
  <> command "service"    (info (ServiceCmd <$> serviceCmds) (progDesc "Perform service related operations"))
  <> command "introspect" (info introspectCmd                (progDesc "Introspect the manifest from a running cluster"))
  )

-- |Command line parser for all the possible Groot options
grootOpts :: Parser GrootOpts
grootOpts = GrootOpts
          <$> credsOpt
          <*> optional regionOpt
          <*> optional sessionAuthOpt
          <*> switch
            ( long "debug"
           <> help "Enable debug mode when running" )

-- | Generates the AWS Env from the command line options
loadEnv :: GrootOpts -> IO Env
loadEnv opts = do
  env <- newEnv $ opts ^. grootCreds
  assignRegion (opts ^. grootRegion) <$> (setupSession =<< setupLogger env)
    where assignRegion :: Maybe Region -> Env -> Env
          assignRegion maybeRegion env =
            maybe id (\x -> envRegion .~ x) maybeRegion env

          setupSession :: Env -> IO Env
          setupSession env =
            case opts ^. grootSession of
              Just auth -> startSession auth env
              Nothing   -> pure env

          setupLogger :: Env -> IO Env
          setupLogger env = if opts ^. grootDebug
            then do
              lgr <- newLogger AWS.Debug stdout
              return $ env & envLogger .~ lgr
            else return env

-- |Groot main entry point from the Command line, able to interpret
-- arguments and parameters passed to it
execGrootCmd :: GrootCmd -> GrootIO ()
execGrootCmd = handleExceptions_ . evalCmd
  where
    evalCmd :: GrootCmd -> GrootIO ()
    evalCmd (ClusterCmd opts)    = runClusterCmd opts
    evalCmd (ListCmd opts)       = runListCmd opts
    evalCmd (ServiceCmd opts)    = runServiceCmd opts
    evalCmd (IntrospectCmd opts) = runIntrospect opts
