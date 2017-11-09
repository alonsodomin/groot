{-# LANGUAGE OverloadedStrings #-}

module Groot.App 
     (
       groot
     ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Text (unpack)
import Network.AWS
import Network.AWS.Data.Text
import Network.HTTP.Types.Status

import Groot.App.Cli
import Groot.App.Config
import Groot.App.Compose
import Groot.App.Events
import Groot.App.List

loadEnv :: CliOptions -> IO Env
loadEnv opts = do
  configFile       <- defaultConfigFile
  (creds, profile) <- buildCreds $ awsCreds opts
  env              <- newEnv creds
  assignRegion (findRegion configFile profile) env
    where buildCreds :: AwsCredentials -> IO (Credentials, Maybe Text)
          buildCreds (AwsProfile profile file) = do
            profileName <- return $ maybe defaultAwsProfileName id profile
            credsFile   <- maybe defaultCredsFile return file
            return $ (FromFile profileName credsFile, Just profileName)
          buildCreds (AwsKeys accessKey secretKey) =
            return $ (FromKeys accessKey secretKey, Nothing)
      
          findRegion :: FilePath -> Maybe Text -> MaybeT IO Region
          findRegion confFile profile = regionFromOpts <|> regionFromConfig confFile profile
            where regionFromOpts = MaybeT $ return $ awsRegion opts

          assignRegion :: MaybeT IO Region -> Env -> IO Env
          assignRegion r env = do
            maybeRegion <- runMaybeT r
            return $ maybe id (\x -> envRegion .~ x) maybeRegion env

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
