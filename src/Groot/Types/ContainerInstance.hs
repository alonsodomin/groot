{-# LANGUAGE OverloadedStrings  #-}

module Groot.Types.ContainerInstance
     (
       ContainerInstanceArn
     , arnContainerInstanceId
     ) where

import           Control.Lens
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Groot.Data.Text
import           Groot.Types.Base

newtype ContainerInstanceArnPath = ContainerInstanceArnPath Text
  deriving (Eq, Show)

ciapContainerInstanceId :: Getter ContainerInstanceArnPath Text
ciapContainerInstanceId = to (\(ContainerInstanceArnPath x) -> x)

type ContainerInstanceArn = Arn ContainerInstanceArnPath

arnContainerInstanceId :: Getter ContainerInstanceArn Text
arnContainerInstanceId = arnResourcePath . ciapContainerInstanceId

instance FromText ContainerInstanceArnPath where
  parser = do
    "container-instance/"
    instanceId <- takeText
    return $ ContainerInstanceArnPath instanceId

instance ToText ContainerInstanceArnPath where
  toText (ContainerInstanceArnPath instanceId) =
    T.append "container-instance/" instanceId