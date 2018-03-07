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

import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import           Test.Hspec

import           Groot.Console
import           Groot.Data.Text

data ConsoleOp a =
    PutMessage Severity Text a
  | AskUser Text (Maybe Text -> a)
  deriving Functor

type ConsoleM = Free ConsoleOp

instance {-# OVERLAPS #-} MonadConsole ConsoleM where
  putMessage sev txt = liftF (PutMessage sev (toText txt) ())
  askUser txt        = liftF (AskUser (toText txt) id)

data ConsoleEvent =
    MessagePut Severity Text
  | UserAnswered Text (Maybe Text)
  deriving (Eq, Show)

type ConsoleState = ReaderT (Maybe Text) (State [ConsoleEvent])

runConsoleM :: Maybe Text -> ConsoleM a -> (a, [ConsoleEvent])
runConsoleM input console =
  runReader (accumEvents console) input
  where accumEvents :: ConsoleM a -> Reader (Maybe Text) (a, [ConsoleEvent])
        accumEvents console =
          mapReaderT (\st -> Identity $ runState st []) (interpretConsole console)

        interpretConsole :: ConsoleM a -> ConsoleState a
        interpretConsole = foldFree $ \msg -> do
          prev <- get
          case msg of
            PutMessage severity text next -> do
              let event = MessagePut severity text
              put (event:prev)
              return next
            AskUser txt fn -> do
              answer <- ask
              let event = UserAnswered txt answer
              put (event:prev)
              return (fn answer)

eventsAfterAnswer :: Text -> ConsoleM a -> [ConsoleEvent]
eventsAfterAnswer answer console = snd $ runConsoleM (Just answer) console

events :: ConsoleM a -> [ConsoleEvent]
events console = snd $ runConsoleM Nothing console

describePutMessage :: IO ()
describePutMessage = hspec $ do
  describe "putXXX" $ do
    it "is equivalent to a putMessage with the severity enum" $ do
      let txt = toText ("hello" :: String)
      events (putSuccess txt) `shouldBe` [MessagePut Success txt]
      events (putInfo    txt) `shouldBe` [MessagePut Info    txt]
      events (putWarn    txt) `shouldBe` [MessagePut Warn    txt]
      events (putError   txt) `shouldBe` [MessagePut Error   txt]
  describe "askUser" $ do
    it "should yield an AskUser event" $ do
      let txt = toText ("hello" :: String)
      eventsAfterAnswer "bye" (askUser txt) `shouldBe` [UserAnswered txt (Just "bye")]
  describe "askUserYN" $ do
    it "should interpret no answer as the default" $ do
      let prompt = toText ("say something" :: String)
      let falseExpectedPrompt = prompt <> " [yN] "
      let trueExpectedPrompt = prompt <> " [Yn] "
      runConsoleM Nothing (askUserYN False prompt) `shouldBe` (False, [UserAnswered falseExpectedPrompt Nothing])
      runConsoleM Nothing (askUserYN True  prompt) `shouldBe` (True,  [UserAnswered trueExpectedPrompt  Nothing])
    it "should translate a y answer to a true" $ do
      let prompt = toText ("foo" :: String)
      let expectedPrompt = prompt <> " [yN] "
      runConsoleM (Just "Y") (askUserYN False prompt) `shouldBe` (True, [UserAnswered expectedPrompt (Just "Y")])
    it "should translate a n answer to a false" $ do
      let prompt = toText ("foo" :: String)
      let expectedPrompt = prompt <> " [Yn] "
      runConsoleM (Just "N") (askUserYN True prompt) `shouldBe` (False, [UserAnswered expectedPrompt (Just "N")])

describeConsole :: IO ()
describeConsole = describePutMessage
