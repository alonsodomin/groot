{-# LANGUAGE LambdaCase #-}

module Groot.Exception.Manifest where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch    hiding (Handler)
import           Data.Text              (Text)
import           Data.Typeable
import           Groot.Types

data ManifestException =
  ManifestParseError ManifestParseError
  deriving (Eq, Show, Typeable)

instance Exception ManifestException

data ManifestParseError = ManifestParseError' FilePath Text
  deriving (Eq, Show, Typeable)

instance Exception ManifestParseError

manifestParseError :: FilePath -> Text -> SomeException
manifestParseError file reason =
  toException . ManifestParseError $ ManifestParseError' file reason

class AsManifestException t where
  _ManifestException :: Prism' t ManifestException
  {-# MINIMAL _ManifestException #-}

  _ManifestParseError :: Prism' t ManifestParseError
  _ManifestParseError = _ManifestException . _ManifestParseError

instance AsManifestException SomeException where
  _ManifestException = exception

instance AsManifestException ManifestException where
  _ManifestException = id

  _ManifestParseError = prism ManifestParseError $ \case
    ManifestParseError x -> Right x
