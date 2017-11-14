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
      let expectedARN = ARN "ecs" Ireland "340721489904" "container-instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      parseOnly parser validArn `shouldBe` (Right expectedARN)
    it "fails to parse non-ARNs" $ do
      let invalidArn = "this is not an ARN"
      parseOnly (parser :: Parser ARN) invalidArn `shouldSatisfy` isLeft
    it "renders as an URN" $ do
      let arn = ARN "ec2" Oregon "340721489904" "instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      let expected = "arn:aws:ec2:us-west-2:340721489904:instance/b7e184d6-5b5a-4fa8-8fd3-27bd2c4b0988"
      showText arn `shouldBe` expected

describeTypes :: IO ()
describeTypes = describeARN

