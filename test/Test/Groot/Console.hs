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
import           Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import           Data.Text           (Text)
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
  | UserAsked Text (Maybe Text)
  deriving (Eq, Show)

type ConsoleState = ReaderT (Maybe Text) (State [ConsoleEvent])

toConsoleEvents :: Maybe Text -> ConsoleM a -> [ConsoleEvent]
toConsoleEvents input console =
  runReader (accumEvents console) input
  where accumEvents :: ConsoleM a -> Reader (Maybe Text) [ConsoleEvent]
        accumEvents console =
          mapReaderT (\st -> Identity $ execState st []) (interpretConsole console)
    
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
              let event = UserAsked txt answer
              put (event:prev)
              return (fn answer)

describePutMessage :: IO ()
describePutMessage = hspec $ do
  describe "putXXX" $ do
    it "is equivalent to a putMessage with the severity enum" $ do
      let txt = toText ("hello" :: String)
      toConsoleEvents Nothing (putSuccess txt) `shouldBe` [MessagePut Success txt]
      toConsoleEvents Nothing (putInfo    txt) `shouldBe` [MessagePut Info    txt]
      toConsoleEvents Nothing (putWarn    txt) `shouldBe` [MessagePut Warn    txt]
      toConsoleEvents Nothing (putError   txt) `shouldBe` [MessagePut Error   txt]
  describe "askUser" $ do
    it "should yield an AskUser event" $ do
      let txt = toText ("hello" :: String)
      toConsoleEvents (Just "bye") (askUser txt) `shouldBe` [UserAsked txt (Just "bye")]

describeConsole :: IO ()
describeConsole = describePutMessage
