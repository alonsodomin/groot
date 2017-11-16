module Test.Groot.Types.Cluster where

import Data.Text.Arbitrary
import Groot.Types.Base
import Groot.Types.Cluster
import Test.Groot.Types.Base
import Test.QuickCheck

instance Arbitrary ClusterArnPath where
  arbitrary = ClusterArnPath <$> arbitrary