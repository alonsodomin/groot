module Groot.Internal.Data.JSON where

import           Control.Monad.Identity
import           Data.Aeson
import qualified Data.Aeson.Types         as JSON

import           Groot.Internal.Data.Text

parseFromText :: (Traversable f, FromText a) => (String -> String) -> f Text -> JSON.Parser (f a)
parseFromText f input = sequence $ (either (\x -> fail $ f x) pure) . fromText <$> input

parseFromText' :: FromText a => (String -> String) -> Text -> JSON.Parser a
parseFromText' f input = runIdentity <$> parseFromText f (Identity input)
