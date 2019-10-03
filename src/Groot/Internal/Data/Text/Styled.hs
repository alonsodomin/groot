{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.Data.Text.Styled
     ( Style
     , noStyle
     , blueStyle
     , redStyle
     , yellowStyle
     , greenStyle
     , cyanStyle
     , StyledText(..)
     , styled
     , empty
     , singleton
     , styleless
     , (<+>)
     , (</>)
     ) where

import           Data.List
import           Data.Semigroup
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Network.AWS.Data.Text
import           System.Console.ANSI

type Style = [SGR]

noStyle, blueStyle, redStyle, yellowStyle, greenStyle, cyanStyle :: Style
noStyle     = [Reset]
blueStyle   = [SetColor Foreground Dull Blue]
redStyle    = [SetColor Foreground Vivid Red]
yellowStyle = [SetColor Foreground Dull Yellow]
greenStyle  = [SetColor Foreground Dull Green]
cyanStyle   = [SetColor Foreground Dull Cyan]

data StyledText =
    TextSpan Style Text
  | TextPhrase [StyledText]
  | TextBlock [StyledText]
  deriving (Eq, Show)

styled :: Style -> Text -> StyledText
styled = TextSpan
{-# INLINE styled #-}

empty :: StyledText
empty = styled noStyle T.empty
{-# INLINE empty #-}

singleton :: Char -> StyledText
singleton ch = styled noStyle (T.singleton ch)
{-# INLINE singleton #-}

styleless :: Text -> StyledText
styleless = styled noStyle
{-# INLINE styleless #-}

instance IsString StyledText where
  fromString str = styled noStyle (T.pack str)

instance Semigroup StyledText where
  lhs@(TextSpan _ _) <> rhs@(TextSpan _ _) = TextPhrase [lhs, rhs]
  lhs@(TextSpan _ _) <> (TextPhrase bs)    = TextPhrase (lhs:bs)
  lhs@(TextSpan _ _) <> (TextBlock bs)     = TextBlock (lhs:bs)
  (TextPhrase bs)    <> rhs@(TextSpan _ _) = TextPhrase (bs ++ [rhs])
  (TextPhrase lhs)   <> (TextPhrase rhs)   = TextPhrase (lhs ++ rhs)
  lhs@(TextPhrase _) <> (TextBlock bs)     = TextBlock (lhs:bs)
  (TextBlock bs)     <> rhs@(TextSpan _ _) = TextBlock (bs ++ [rhs])
  (TextBlock lhs)    <> (TextBlock rhs)    = TextBlock (lhs ++ rhs)

instance Monoid StyledText where
  mempty = empty
  mappend = (<>)

(<+>) :: StyledText -> StyledText -> StyledText
lhs <+> rhs = lhs <> (singleton ' ') <> rhs

(</>) :: StyledText -> StyledText -> StyledText
lhs </> rhs = TextBlock [lhs, rhs]

instance ToText StyledText where
  toText (TextSpan _ txt) = txt
  toText (TextPhrase xs)  = T.concat $ toText <$> xs
  toText (TextBlock xs)   = T.concat $ intersperse (T.singleton '\n') (toText <$> xs)
