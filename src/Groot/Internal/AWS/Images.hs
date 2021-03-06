{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.AWS.Images
     ( findImage
     ) where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe
import           Network.AWS
import qualified Network.AWS.EC2           as EC2

import           Groot.Internal.Data.Text
import           Groot.Types

findImage :: MonadAWS m => Either Ami (NonEmpty ImageFilterPart) -> MaybeT m EC2.Image
findImage amiOrFilter = MaybeT $ do
  res <- send $ filterReq amiOrFilter
  return . listToMaybe $ res ^. EC2.diirsImages
  where filterReq (Left ami)    = EC2.deseImageIds .~ [toText ami] $ EC2.describeImages
        filterReq (Right filtr) = EC2.deseFilters .~ (NEL.toList $ encodeFilterPart <$> filtr) $ EC2.describeImages

        filter'' :: ToText a => Text -> a -> EC2.Filter
        filter'' name value = EC2.fValues .~ [toText value] $ EC2.filter' name

        encodeFilterPart (IFPName name)              = filter'' "name"                name
        encodeFilterPart (IFPVirtualizationType vt)  = filter'' "virtualization-type" vt
        encodeFilterPart (IFPOwnerAlias oa)          = filter'' "owner-alias"         oa
        encodeFilterPart (IFPArchitecture arch)      = filter'' "architecture"        arch
        encodeFilterPart (IFPRootDeviceType devType) = filter'' "root-device-type"    devType
        encodeFilterPart (IFPImageState state)       = filter'' "state"               state
