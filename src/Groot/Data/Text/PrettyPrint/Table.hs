{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Groot.Data.Text.PrettyPrint.Table
     ( PrettyColumn (..)
     , printTable
     ) where

import Control.Lens
import Data.Monoid
import Data.List (transpose)
import qualified Data.Text as T
import Groot.Data.Text
import Groot.Data.Text.PrettyPrint.ANSI
import qualified Text.PrettyPrint.Boxes as B

data ColumnAlignment =
    Left
  | Center
  | Right
  deriving (Eq, Show, Enum, Bounded)

data ColumnStyle = ColumnStyle AnsiStyle ColumnAlignment

styled :: ToText a => AnsiStyle -> a -> B.Box
styled style txt =
  let str = T.unpack . toText $ txt
      dec = decorateStr style str
      pad = (length dec) - (length str)
  in B.hsep pad B.left [(B.text dec), B.emptyBox 0 1]

alignColumns :: [[B.Box]] -> B.Box
alignColumns bs = B.hsep 3 B.left cols
  where cols = (B.vcat B.top) <$> transpose bs

class PrettyColumn a where
  type PrettyItemOf a :: *

  columnHeader :: a -> Text

  columnCell :: forall m b. (Monoid m, ToText b) => a -> Getting m (PrettyItemOf a) b

  columnStyle :: a -> Text -> AnsiStyle
  columnStyle _ _ = mempty

headerStyle :: AnsiStyle
--headerStyle = bold <> color Vivid White
headerStyle = mempty

tableHeaders :: PrettyColumn a => [a] -> [B.Box]
tableHeaders cols = (styled headerStyle . columnHeader) <$> cols

tableRow :: PrettyColumn a => [a] -> PrettyItemOf a -> [B.Box]
tableRow cols item = renderCol <$> cols
  where renderCol col =
          let content = maybe "" id $ item ^? (columnCell col)
          in styled (columnStyle col content) content

printTable :: PrettyColumn a => [a] -> [PrettyItemOf a] -> IO ()
printTable cols items =
  B.printBox . alignColumns $ (tableHeaders cols) : (tableRow cols <$> items)