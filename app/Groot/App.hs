{-# LANGUAGE OverloadedStrings #-}

module Groot.App 
     (
       groot
     ) where

import Control.Lens
import Data.Text (Text, unpack)
import Network.AWS
import Network.AWS.Data.Text
import Network.HTTP.Types.Status
import System.Directory

import Groot.App.Cli
import Groot.App.Compose
import Groot.App.Events
import Groot.App.List

defaultProfileName :: Text
defaultProfileName = "default"

defaultCredsFile :: IO FilePath
defaultCredsFile = (++ "/.aws/credentials") <$> getHomeDirectory

loadEnv :: CliOptions -> IO Env
loadEnv opts = do
  creds <- buildCreds $ awsCreds opts
  env   <- newEnv creds
  return $ assignRegion (awsRegion opts) env
    where buildCreds :: AwsCredentials -> IO Credentials
          buildCreds (AwsProfile profile file) = do
            profileName <- return $ maybe defaultProfileName id profile
            credsFile   <- maybe defaultCredsFile return file
            return $ FromFile profileName credsFile
          buildCreds (AwsKeys accessKey secretKey) =
            return $ FromKeys accessKey secretKey
      
          assignRegion :: Maybe Region -> Env -> Env
          assignRegion (Just r) = envRegion .~ r
          assignRegion _        = id

grootCmd :: Cmd -> Env -> IO ()
grootCmd (ComposeCmd opts) = runGrootCompose opts
grootCmd (ListCmd opts)    = runGrootList opts
grootCmd (EventsCmd opts)  = runGrootEvents opts

groot :: CliOptions -> IO ()
groot opts = catching _ServiceError exec handler
  where exec :: IO ()
        exec = loadEnv opts >>= grootCmd (cmd opts)

        handler :: ServiceError -> IO ()
        handler err =
          let servName  = unpack . toText $ err ^. serviceAbbrev
              statusMsg = show $ statusMessage $ err ^. serviceStatus
              message   = maybe "" (unpack . toText) $ err ^. serviceMessage
          in putStrLn $ servName ++ " Error (" ++ statusMsg ++ "): " ++ message
