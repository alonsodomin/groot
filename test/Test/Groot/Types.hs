{-# LANGUAGE OverloadedStrings #-}

module Test.Groot.Types
     ( describeTypes
     ) where

import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text                as T
import           Data.Text.Arbitrary
import           Network.AWS.Types
import           Test.Hspec
import           Test.QuickCheck

import           Groot.Internal.Data.Text
import           Groot.Types

-- Arbitrary instances

instance Arbitrary ServiceId where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccountId where
  arbitrary = AccountId . T.pack <$> listOf1 (elements ['0'..'9'])

instance Arbitrary Region where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Arn a) where
  arbitrary = Arn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

arnParsePreservation :: (FromText a, ToText a, Eq a) => Arn a -> Bool
arnParsePreservation arn = parseOnly parser (toText arn) == Right arn

-- DummyArnPath to verify basic ARN parsing

newtype DummyArnPath = DummyArnPath Text
  deriving (Eq, Show)

instance FromText DummyArnPath where
  parser = DummyArnPath <$> takeText

instance ToText DummyArnPath where
  toText (DummyArnPath path) = path

type DummyArn = Arn DummyArnPath

instance Arbitrary DummyArnPath where
  arbitrary = DummyArnPath <$> arbitrary

quickCheckDummyArn =
  quickCheck $ \x -> arnParsePreservation (x :: DummyArn)

describeARN :: IO ()
describeARN = hspec $ do
  describe "ARN" $ do
    it "parses a valid ARN" $ do
      let validArn = "arn:aws:ecs:eu-west-1:340721489904:container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      let expectedARN = Arn ECS (Just Ireland) (AccountId "340721489904") (DummyArnPath "container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988")
      parseOnly parser validArn `shouldBe` (Right expectedARN)
    it "parses arns without region" $ do
      let givenArn = "arn:aws:iam::340721489904:role/arole"
      let expected = Arn IAM Nothing (AccountId "340721489904") (RoleArnPath "arole")
      parseOnly parser givenArn `shouldBe` (Right expected)
    it "fails to parse non-ARNs" $ do
      let invalidArn = "this is not an ARN"
      parseOnly (parser :: Parser DummyArn) invalidArn `shouldSatisfy` isLeft
    it "renders as an URN" $ do
      let arn = Arn EC2 (Just Oregon) (AccountId "340721489904") (DummyArnPath "instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988")
      let expected = "arn:aws:ec2:us-west-2:340721489904:instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      showText arn `shouldBe` expected

describeAmi :: IO ()
describeAmi = hspec $ do
  describe "AMI" $ do
    it "parses an AMI string" $ do
      let validAMI = "ami-78328334"
      let expectedAMI = Ami "78328334"
      parseOnly parser validAMI `shouldBe` (Right expectedAMI)
    it "fails to parse non-AMIs" $ do
      let invalidAmi = "this is not an AMI"
      parseOnly (parser :: Parser Ami) invalidAmi `shouldSatisfy` isLeft
    it "renders AMI as a string" $ do
      let ami = Ami "0dj9393rj"
      let expected = "ami-0dj9393rj"
      showText ami `shouldBe` expected

describeBaseTypes :: IO ()
describeBaseTypes = describeARN *> describeAmi *> quickCheckDummyArn

instance Arbitrary ClusterName where
  arbitrary = ClusterName <$> arbitrary

instance Arbitrary ClusterArnPath where
  arbitrary = ClusterArnPath <$> arbitrary

quickCheckClusterArn :: IO ()
quickCheckClusterArn = quickCheck $ \x -> arnParsePreservation (x :: ClusterArn)

describeClusterTypes :: IO ()
describeClusterTypes = quickCheckClusterArn

describeTypes :: IO ()
describeTypes = describeARN *> describeAmi *> quickCheckDummyArn *> describeClusterTypes
