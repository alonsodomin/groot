{-# LANGUAGE OverloadedStrings  #-}

module Groot.Types.ContainerInstance where

import Data.Text (Text)
import qualified Data.Text as T
import Groot.Data.Text
import Groot.Types.Base

newtype ContainerInstanceArnPath = ContainerInstanceArnPath Text
  deriving (Eq, Show)

type ContainerInstanceArn = Arn ContainerInstanceArnPath

instance FromText ContainerInstanceArnPath where
  parser = do
    "container-instance/"
    instanceId <- takeText
    return $ ContainerInstanceArnPath instanceId

instance ToText ContainerInstanceArnPath where
  toText (ContainerInstanceArnPath instanceId) =
    T.append "container-instance/" instanceId