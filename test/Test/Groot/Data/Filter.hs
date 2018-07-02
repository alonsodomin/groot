module Test.Groot.Data.Filter where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Groot.Data.Filter

instance Arbitrary a => Arbitrary (Filter a) where
  arbitrary = frequency [(10, arbitrarySingle), (4, arbitraryAnd), (4, arbitraryOr), (2, arbitraryNot)]
    where arbitrarySingle = Single <$> arbitrary
          arbitraryAnd    = And    <$> arbitrary <*> arbitrary
          arbitraryOr     = Or     <$> arbitrary <*> arbitrary
          arbitraryNot    = Not    <$> arbitrary

instance Eq a => EqProp (Filter a) where
  (=-=) = eq

describeFilterInstances :: IO ()
describeFilterInstances = do
    let filtr = undefined :: Filter (Int, Int, Int)
    quickBatch $ functor filtr
