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

type ConsoleState = State [ConsoleEvent]

toConsoleEvents :: ConsoleM a -> [ConsoleEvent]
toConsoleEvents console =
  execState (interpretConsole console) []
  where interpretConsole = foldFree $ \msg -> do
          prev <- get
          case msg of
            PutMessage severity text next -> do
              let event = MessagePut severity text
              put (event:prev)
              return next
            AskUser text onAnswer -> undefined

describePutMessage :: IO ()
describePutMessage = hspec $ do
  describe "putXXX" $ do
    it "is equivalent to a putMessage with the severity enum" $ do
      let txt = toText ("hello" :: String)
      toConsoleEvents (putSuccess txt) `shouldBe` [MessagePut Success txt]
      toConsoleEvents (putInfo    txt) `shouldBe` [MessagePut Info    txt]
      toConsoleEvents (putWarn    txt) `shouldBe` [MessagePut Warn    txt]
      toConsoleEvents (putError   txt) `shouldBe` [MessagePut Error   txt]

describeConsole :: IO ()
describeConsole = describePutMessage
