module Test.Groot.Types.Cluster
     ( describeClusterTypes
     ) where

import Data.Text.Arbitrary
import Groot.Types.Base
import Groot.Types.Cluster
import Test.Groot.Types.Base
import Test.QuickCheck

instance Arbitrary ClusterArnPath where
  arbitrary = ClusterArnPath <$> arbitrary

quickCheckClusterArn :: IO ()
quickCheckClusterArn = quickCheck $ \x -> arnParsePreservation (x :: ClusterArn)

describeClusterTypes :: IO ()
describeClusterTypes = quickCheckClusterArn