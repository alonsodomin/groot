{-# LANGUAGE OverloadedStrings  #-}

module Test.Groot.Types
     ( describeTypes
     ) where

import Data.Attoparsec.Text
import Data.Either
import Groot.Types
import Network.AWS
import Network.AWS.Data.Text
import Test.Hspec

describeARN :: IO ()
describeARN = hspec $ do
  describe "ARN" $ do
    it "parses a valid ARN" $ do
      let validArn = "arn:aws:ecs:eu-west-1:340721489904:container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      let expectedARN = Arn ECS Ireland (AccountId "340721489904") "container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      parseOnly parser validArn `shouldBe` (Right expectedARN)
    it "fails to parse non-ARNs" $ do
      let invalidArn = "this is not an ARN"
      parseOnly (parser :: Parser Arn) invalidArn `shouldSatisfy` isLeft
    it "renders as an URN" $ do
      let arn = Arn EC2 Oregon (AccountId "340721489904") "instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
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

describeTypes :: IO ()
describeTypes = describeARN *> describeAmi

