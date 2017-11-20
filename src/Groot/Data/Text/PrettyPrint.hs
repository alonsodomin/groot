{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Groot.Data.Text.PrettyPrint where

import qualified Data.Text.Lazy.IO as TL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

class PrettyColumn a where
  type PrettyItemOf a :: *

  columnHeader :: a -> Doc AnsiStyle

  columnCell :: a -> PrettyItemOf a -> Doc AnsiStyle

tableHeaders :: PrettyColumn a => [a] -> Doc AnsiStyle
tableHeaders cols = hsep $ columnHeader <$> cols

tableRow :: PrettyColumn a => [a] -> PrettyItemOf a -> Doc AnsiStyle
tableRow cols item = hsep $ (\x -> columnCell x item) <$> cols

table :: PrettyColumn a => [a] -> [PrettyItemOf a] -> Doc AnsiStyle
table cols items = align . vsep $ (tableHeaders cols) : (tableRow cols <$> items)

renderAnsi :: SimpleDocStream AnsiStyle -> IO ()
renderAnsi = TL.putStrLn . renderLazy

renderAnsiDefault :: Doc AnsiStyle -> IO ()
renderAnsiDefault = renderAnsi . layoutPretty defaultLayoutOptions
