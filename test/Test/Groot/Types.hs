module Test.Groot.Types
     ( describeTypes
     ) where

import Test.Groot.Types.Base
import Test.Groot.Types.Cluster

describeTypes :: IO ()
describeTypes = describeBaseTypes *> describeClusterTypes