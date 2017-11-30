{-# LANGUAGE OverloadedStrings #-}

module Groot.Config
       (
       -- Default config values
         defaultSectionName
       , defaultConfigFile
       , defaultCredsFile
       -- Utilities
       , regionFromConfig
       ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Ini
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.AWS                (Region (..))
import           Network.AWS.Data.Text
import           System.Directory

defaultSectionName :: Text
defaultSectionName = "default"

defaultConfigFile :: IO FilePath
defaultConfigFile = (++ "/.aws/config") <$> getHomeDirectory

defaultCredsFile :: IO FilePath
defaultCredsFile = (++ "/.aws/credentials") <$> getHomeDirectory

sectionNameFromProfile :: Text -> Text
sectionNameFromProfile s
  | s == defaultSectionName = s
  | otherwise               = T.append "profile " s

lookupRegionFromConfig :: Maybe Text -> Ini -> Either String Region
lookupRegionFromConfig profile config =
  fromText =<< lookupWithFallback profile "region" config
  where lookupWithFallback :: Maybe Text -> Text -> Ini -> Either String Text
        lookupWithFallback Nothing  key cfg =
          lookupValue defaultSectionName key cfg
        lookupWithFallback (Just x) key cfg =
              lookupValue (sectionNameFromProfile x) key cfg
          <|> lookupWithFallback Nothing key cfg

regionFromConfig :: MonadIO m => FilePath -> Maybe Text -> ExceptT String m Region
regionFromConfig filename profile = do
  ini <- ExceptT . liftIO $ readIniFile filename
  ExceptT . return $ lookupRegionFromConfig profile ini
