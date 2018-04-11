{-# LANGUAGE OverloadedStrings #-}

module Groot.Internal.PrettyPrint (
    -- Types
    Doc, SimpleDoc,
    -- Lines
    line, linebreak, softline, softbreak, hardline, hyphen,
    -- Custom utils
    time, defaultTime, status,
    -- Infix operators
    (<>), (<+>), (<$>), (</>), (<$$>), (<//>),
    -- Style
    black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
    dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite, onblack, onred,
    ongreen, onyellow, onblue, onmagenta, oncyan, onwhite, ondullblack,
    ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan,
    ondullwhite, bold, debold, underline, deunderline, plain,
    -- Layout
    defaultIndent, label, field, field', listField, listField',
    -- Modules
    module Data.Text.Prettyprint.Doc,
    module Data.Text.Prettyprint.Doc.Render.Terminal
  ) where

import           Prelude                                   hiding ((<$>))

import           Data.Monoid
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc                 hiding (Doc,
                                                            hardline, line,
                                                            softline, (<+>),
                                                            (<>))
import qualified Data.Text.Prettyprint.Doc                 as New
import           Data.Text.Prettyprint.Doc.Render.Terminal hiding (bold)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as NewT
import           Data.Time

import           Groot.Data.Text                           hiding ((<+>))

type Doc = New.Doc NewT.AnsiStyle
type SimpleDoc = New.SimpleDocStream NewT.AnsiStyle

line :: Doc
line = New.line

linebreak :: Doc
linebreak = New.flatAlt New.line mempty

softline :: Doc
softline = New.softline

softbreak :: Doc
softbreak = New.group linebreak

hardline :: Doc
hardline = New.hardline

(<+>), (<$>), (</>), (<$$>), (<//>) :: Doc -> Doc -> Doc
(<+>) = (New.<+>)
(<$>) = \x y -> x <> New.line <> y
(</>) = \x y -> x <> softline <> y
(<$$>) = \x y -> x <> linebreak <> y
(<//>) = \x y -> x <> softbreak <> y

-- Text Style

black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
  dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite, onblack,
  onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite, ondullblack,
  ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan,
  ondullwhite, bold, debold, underline, deunderline :: Doc -> Doc
black         = New.annotate (NewT.color       NewT.Black)
red           = New.annotate (NewT.color       NewT.Red)
green         = New.annotate (NewT.color       NewT.Green)
yellow        = New.annotate (NewT.color       NewT.Yellow)
blue          = New.annotate (NewT.color       NewT.Blue)
magenta       = New.annotate (NewT.color       NewT.Magenta)
cyan          = New.annotate (NewT.color       NewT.Cyan)
white         = New.annotate (NewT.color       NewT.White)
dullblack     = New.annotate (NewT.colorDull   NewT.Black)
dullred       = New.annotate (NewT.colorDull   NewT.Red)
dullgreen     = New.annotate (NewT.colorDull   NewT.Green)
dullyellow    = New.annotate (NewT.colorDull   NewT.Yellow)
dullblue      = New.annotate (NewT.colorDull   NewT.Blue)
dullmagenta   = New.annotate (NewT.colorDull   NewT.Magenta)
dullcyan      = New.annotate (NewT.colorDull   NewT.Cyan)
dullwhite     = New.annotate (NewT.colorDull   NewT.White)
onblack       = New.annotate (NewT.bgColor     NewT.Black)
onred         = New.annotate (NewT.bgColor     NewT.Red)
ongreen       = New.annotate (NewT.bgColor     NewT.Green)
onyellow      = New.annotate (NewT.bgColor     NewT.Yellow)
onblue        = New.annotate (NewT.bgColor     NewT.Blue)
onmagenta     = New.annotate (NewT.bgColor     NewT.Magenta)
oncyan        = New.annotate (NewT.bgColor     NewT.Cyan)
onwhite       = New.annotate (NewT.bgColor     NewT.White)
ondullblack   = New.annotate (NewT.bgColorDull NewT.Black)
ondullred     = New.annotate (NewT.bgColorDull NewT.Red)
ondullgreen   = New.annotate (NewT.bgColorDull NewT.Green)
ondullyellow  = New.annotate (NewT.bgColorDull NewT.Yellow)
ondullblue    = New.annotate (NewT.bgColorDull NewT.Blue)
ondullmagenta = New.annotate (NewT.bgColorDull NewT.Magenta)
ondullcyan    = New.annotate (NewT.bgColorDull NewT.Cyan)
ondullwhite   = New.annotate (NewT.bgColorDull NewT.White)
bold = New.annotate NewT.bold
debold = id
{-# WARNING debold "Debold does not do anything" #-}
underline = New.annotate NewT.underlined
deunderline = id
{-# WARNING deunderline "Deunderline does not do anything" #-}

hyphen :: Doc
hyphen = New.pretty ("-" :: Text)

-- Groot specific functions

defaultIndent :: Int
defaultIndent = 3

plain :: Doc -> Doc
plain = New.unAnnotate

label :: Pretty a => a -> Doc
label = dullblue . New.pretty

field :: (a -> Doc) -> Text -> a -> Doc
field f lbl x = (label . toText $ lbl) <+> (f x)

field' :: Pretty a => Text -> a -> Doc
field' = field New.pretty

listField :: (a -> Doc) -> Text -> [a] -> Maybe Doc
listField _ _   [] = Nothing
listField f lbl xs = Just $ New.vsep [
    label lbl,
    New.indent defaultIndent (New.vsep $ fmap f xs)
  ]

listField' :: Pretty a => Text -> [a] -> Maybe Doc
listField' = listField New.pretty

time :: FormatTime t => TimeLocale -> String -> t -> Doc
time tl fmt t = New.pretty $ formatTime tl fmt t

defaultTime :: FormatTime t => t -> Doc
defaultTime = time defaultTimeLocale "%d/%m/%Y %T"

status :: Text -> Doc
status txt@"ACTIVE"   = bold . green . New.pretty $ txt
status txt@"DRAINING" = bold . yellow . New.pretty $ txt
status txt            = bold . red . New.pretty $ txt
