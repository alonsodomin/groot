{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Groot.Console
     ( describeConsole
     ) where

import           Control.Applicative
import           Control.Monad.RWS.Lazy
import           Data.Text                (Text)
import           Test.Hspec

import           Groot.Console
import           Groot.Internal.Data.Text

data ConsoleEvent =
    MessagePut Severity Text
  | UserAnswered Text (Maybe Text)
  deriving (Eq, Show)

type Msg = (Severity, Text)
type MockConsole = RWS (Maybe Text) [Msg] [ConsoleEvent]

instance {-# OVERLAPS #-} MonadConsole MockConsole where
  askUser prompt = do
    answer <- ask
    modify (\prev -> (UserAnswered (toText prompt) answer):prev)
    return answer
  putMessage sev msg = do
    let txt = toText msg
    modify (\prev -> (MessagePut sev txt):prev)
    tell [(sev, txt)]

writtenMessages :: MockConsole () -> [Msg]
writtenMessages console = snd $ evalRWS console Nothing []

evalConsole :: Maybe Text -> MockConsole a -> (a, [ConsoleEvent])
evalConsole input console =
  let (res, evts, _) = runRWS console input []
  in (res, reverse evts)

events :: Maybe Text -> MockConsole a -> [ConsoleEvent]
events input console = snd $ evalConsole input console

userAnswered :: Text -> MockConsole a -> Maybe Text
userAnswered answer console = grabAnswer $ events (Just answer) console
  where grabAnswer :: [ConsoleEvent] -> Maybe Text
        grabAnswer =
          let getAnswer (UserAnswered _ ans) = ans
              getAnswer _                    = Nothing
          in (foldl (<|>) Nothing) . (fmap getAnswer)

describeConsole :: IO ()
describeConsole = hspec $ do
  describe "putXXX" $ do
    it "is equivalent to a putMessage with the severity enum" $ do
      let txt = toText ("hello" :: String)
      let console = do
            putSuccess txt
            putInfo    txt
            putWarn    txt
            putError   txt
            putDebug   txt
      (writtenMessages console) `shouldBe` [
          (Success, txt)
        , (Info,    txt)
        , (Warn,    txt)
        , (Error,   txt)
        , (Debug,   txt)
        ]

  describe "askUser" $ do
    it "should yield an AskUser event" $ do
      let txt = toText ("hello" :: String)
      userAnswered "bye" (askUser txt) `shouldBe` (Just "bye")

  describe "askUserYN" $ do
    it "should interpret no answer as the default" $ do
      let prompt = toText ("say something" :: String)
      let falseExpectedPrompt = prompt <> " [yN] "
      let trueExpectedPrompt = prompt <> " [Yn] "
      evalConsole Nothing (askUserYN False prompt) `shouldBe` (False, [UserAnswered falseExpectedPrompt Nothing])
      evalConsole Nothing (askUserYN True  prompt) `shouldBe` (True,  [UserAnswered trueExpectedPrompt  Nothing])

    it "should translate a y answer to a true" $ do
      let prompt = toText ("foo" :: String)
      let expectedPrompt = prompt <> " [yN] "
      evalConsole (Just "Y") (askUserYN False prompt) `shouldBe` (True, [UserAnswered expectedPrompt (Just "Y")])
    it "should translate a n answer to a false" $ do
      let prompt = toText ("foo" :: String)
      let expectedPrompt = prompt <> " [Yn] "
      evalConsole (Just "N") (askUserYN True prompt) `shouldBe` (False, [UserAnswered expectedPrompt (Just "N")])

