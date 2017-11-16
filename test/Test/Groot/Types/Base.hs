{-# LANGUAGE OverloadedStrings  #-}

module Test.Groot.Types.Base
     ( describeBaseTypes
     ) where

import Data.Either
import Data.Text.Arbitrary
import Groot.Data.Text
import Groot.Types
import Network.AWS.Types
import Test.Hspec
import Test.QuickCheck

-- Arbitrary instances

instance Arbitrary ServiceId where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccountId where
  arbitrary = AccountId <$> arbitrary

instance Arbitrary Region where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Arn a) where
  arbitrary = Arn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype DummyArnPath = DummyArnPath Text
  deriving (Eq, Show)

instance FromText DummyArnPath where
  parser = DummyArnPath <$> takeText

instance ToText DummyArnPath where
  toText (DummyArnPath path) = path

type DummyArn = Arn DummyArnPath

describeARN :: IO ()
describeARN = hspec $ do
  describe "ARN" $ do
    it "parses a valid ARN" $ do
      let validArn = "arn:aws:ecs:eu-west-1:340721489904:container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      let expectedARN = Arn ECS Ireland (AccountId "340721489904") (DummyArnPath "container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988")
      parseOnly parser validArn `shouldBe` (Right expectedARN)
    it "fails to parse non-ARNs" $ do
      let invalidArn = "this is not an ARN"
      parseOnly (parser :: Parser DummyArn) invalidArn `shouldSatisfy` isLeft
    it "renders as an URN" $ do
      let arn = Arn EC2 Oregon (AccountId "340721489904") (DummyArnPath "instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988")
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
describeBaseTypes = describeARN *> describeAmi