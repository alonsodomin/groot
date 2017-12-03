{-# LANGUAGE OverloadedStrings #-}

module Groot.Data.Text.Styled where

import           Control.Monad.IO.Class
import           Data.Semigroup
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           System.Console.ANSI

import           Groot.Data.Text.Display

type Style = [SGR]

noStyle, blueStyle, redStyle, yellowStyle :: Style
noStyle     = [Reset]
blueStyle   = [SetColor Foreground Dull Blue]
redStyle    = [SetColor Foreground Vivid Red]
yellowStyle = [SetColor Foreground Dull Yellow]

data StyledText =
    TextSpan Style Text
  | TextBlock [StyledText]
  deriving (Eq, Show)

styled :: Style -> Text -> StyledText
styled = TextSpan

empty :: StyledText
empty = styled noStyle T.empty

singleton :: Char -> StyledText
singleton ch = styled noStyle (T.singleton ch)

styleless :: Text -> StyledText
styleless = styled noStyle

instance IsString StyledText where
  fromString str = styled noStyle (T.pack str)

instance Semigroup StyledText where
  lhs@(TextSpan _ _) <> rhs@(TextSpan _ _) = TextBlock [lhs, rhs]
  lhs@(TextSpan _ _) <> (TextBlock bs)     = TextBlock (lhs:bs)
  (TextBlock bs)     <> rhs@(TextSpan _ _) = TextBlock (bs ++ [rhs])
  (TextBlock lhs)    <> (TextBlock rhs)    = TextBlock (lhs ++ rhs)

instance Monoid StyledText where
  mempty = empty
  mappend = (<>)

(<+>) :: StyledText -> StyledText -> StyledText
lhs <+> rhs = lhs <> (singleton ' ') <> rhs

instance Display StyledText where
  display (TextSpan style txt) = liftIO $ do
    setSGR style
    T.putStr txt
  display (TextBlock xs) = do
    display xs
    liftIO $ T.putStrLn ""
