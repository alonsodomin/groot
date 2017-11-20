{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Groot.App.List.Base where

import Control.Monad.State.Lazy
import Control.Lens
import Data.Conduit
import Data.Foldable
import Data.Hashable
import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.Text (Text)
import qualified Data.Text as T
import Groot.Data.Text
import Text.PrettyPrint.ANSI.Leijen ((<$$>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.Tabulate

class (Hashable a, Ord a) => SummaryAttr a where
  type AttrResource a :: *

  attrName :: a -> Text

  attrGetter :: a -> Getter (AttrResource a) (Maybe Text)

  readAttr :: a -> AttrResource a -> Text
  readAttr attr res = maybe T.empty id $ res ^. (attrGetter attr)

  printAttr :: a -> Text -> PP.Doc

toTextGetter :: (Functor f, ToText b) => Getter a (f b) -> Getter a (f Text)
toTextGetter g = to (\x -> toText <$> x ^. g)

extractAttrs :: (Foldable f, SummaryAttr a) => f a -> AttrResource a -> Map a Text
extractAttrs attrs item = foldr (\attr m -> Map.insert attr (readAttr attr item) m) Map.empty attrs

ppheaders :: (Traversable f, SummaryAttr a) => f a -> PP.Doc
ppheaders hdrs = foldl' (<+>) PP.empty $ (PP.text . T.unpack . attrName) <$> hdrs

pprintRow :: (Foldable f, SummaryAttr a) => f a -> AttrResource a -> PP.Doc
pprintRow attrs item = foldl' (\doc attr -> doc <+> attrDoc attr) PP.empty attrs
  where attrDoc attr = printAttr attr $ readAttr attr item

pprintResources :: (Traversable t, Traversable t', SummaryAttr a) => t a -> t' (AttrResource a) -> PP.Doc
pprintResources attrs items = foldl' (<$$>) PP.empty $ pprintSingle <$> items
  where pprintSingle item = pprintRow attrs item

pprintColumn :: SummaryAttr a => Map Int a -> [AttrResource a] -> PP.Doc
pprintColumn cols items = PP.column pprintSingleCol
  where pprintSingleCol idx = maybe PP.empty drawCol $ Map.lookup idx cols
        drawCol attr = foldl' (\doc item -> doc <$$> (printAttr attr $ readAttr attr item)) PP.empty items

pprintSink :: (Traversable t, SummaryAttr a) => t a -> Sink [AttrResource a] IO ()
pprintSink attrs = transPipe (\x -> evalStateT x False) $ pprintSink' attrs

pprintSink' :: (Traversable t, SummaryAttr a) => t a -> Sink [AttrResource a] (StateT Bool IO) ()
pprintSink' attrs = do
  mitems <- await
  case mitems of
    Nothing    -> return ()
    Just items -> do
      hasHeaders <- get
      if (not hasHeaders)
      then do
        hdrs <- state (\_ -> (ppheaders attrs, True))
        rows <- return $ pprintResources attrs items
        liftIO . PP.putDoc $ hdrs <$$> rows        
      else liftIO . PP.putDoc $ pprintResources attrs items
      --pprintSink attrs

class HasSummary a b where
  summarize :: a -> Maybe b

asString :: ToText a => a -> String
asString = T.unpack . toText

printTable' :: Tabulate a => Text -> [a] -> IO ()
printTable' txt []       = putStrLn $ T.unpack txt
printTable' _   xs@(_:_) = ppTable xs