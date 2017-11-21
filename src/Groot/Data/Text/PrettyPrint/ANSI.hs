{-# LANGUAGE LambdaCase #-}

module Groot.Data.Text.PrettyPrint.ANSI
     ( Color (..)
     , Intensity (..)
     , Layer (..)
     , StyleMod (..)
     , color
     , bgColor
     , bold
     , italic
     , AnsiStyle (..)
     , decorateStr
     ) where

import Control.Applicative
import Data.Maybe
import Data.Semigroup
import qualified System.Console.ANSI as ANSI

-- | The 8 ANSI terminal colors.
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Dull or vivid coloring, as supported by ANSI terminals.
data Intensity = Vivid | Dull
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Foreground (text) or background (paper) color
data Layer = Foreground | Background
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Text style modifier
data StyleMod = Bold | Italic | Underline
  deriving (Eq, Ord, Show, Enum, Bounded)

color :: Intensity -> Color -> AnsiStyle
color i c = mempty { ansiForeground = Just (i, c) }

bgColor :: Intensity -> Color -> AnsiStyle
bgColor i c = mempty { ansiBackground = Just (i, c) }

bold :: AnsiStyle
bold = mempty { ansiModifiers = [Bold] }

italic :: AnsiStyle
italic = mempty { ansiModifiers = [Italic] }

data AnsiStyle = AnsiStyle
  { ansiForeground :: Maybe (Intensity, Color)
  , ansiBackground :: Maybe (Intensity, Color)
  , ansiModifiers  :: [StyleMod]
  } deriving (Eq, Show)

instance Semigroup AnsiStyle where
  lhs <> rhs = AnsiStyle
    { ansiForeground = ansiForeground lhs <|> ansiForeground rhs
    , ansiBackground = ansiBackground lhs <|> ansiBackground rhs
    , ansiModifiers  = ansiModifiers lhs <> ansiModifiers rhs }

instance Monoid AnsiStyle where
  mempty = AnsiStyle Nothing Nothing []
  mappend = (<>)

styleCode :: AnsiStyle -> String
styleCode = ANSI.setSGRCode . styleToSGR
  where styleToSGR :: AnsiStyle -> [ANSI.SGR]
        styleToSGR (AnsiStyle fg bg mods) = catMaybes
          [ Just ANSI.Reset
          , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Foreground (convertIntensity intensity) (convertColor c)) fg
          , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Background (convertIntensity intensity) (convertColor c)) bg
          ] ++ (fmap convertStyleMod mods)

        convertIntensity :: Intensity -> ANSI.ColorIntensity
        convertIntensity = \case
          Vivid -> ANSI.Vivid
          Dull  -> ANSI.Dull

        convertColor :: Color -> ANSI.Color
        convertColor = \case
          Black   -> ANSI.Black
          Red     -> ANSI.Red
          Green   -> ANSI.Green
          Yellow  -> ANSI.Yellow
          Blue    -> ANSI.Blue
          Magenta -> ANSI.Magenta
          Cyan    -> ANSI.Cyan
          White   -> ANSI.White

        convertStyleMod :: StyleMod -> ANSI.SGR
        convertStyleMod = \case
          Bold      -> ANSI.SetConsoleIntensity ANSI.BoldIntensity
          Italic    -> ANSI.SetItalicized True
          Underline -> ANSI.SetUnderlining ANSI.SingleUnderline

decorateStr :: AnsiStyle -> String -> String
decorateStr style str = (styleCode style) ++ str 