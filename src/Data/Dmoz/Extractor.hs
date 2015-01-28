{-# LANGUAGE OverloadedStrings, Rank2Types, KindSignatures #-}

module Data.Dmoz.Extractor
  ( extract ) where

import Control.Monad.Trans.Resource (MonadThrow, ResourceT, runResourceT)
import Data.Conduit (ConduitM, ($$))
import qualified Data.Text as T
import Data.Text.Lazy (Text, fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.XML.Types (Event)
import Text.XML (nameLocalName)
import Text.XML.Stream.Parse

import Data.Dmoz.Page

extract :: Text -> IO [Maybe Page]
extract ts = do
  pages <- runResourceT $
    parseLBS def (encodeUtf8 ts) $$ force "RDF element required" parseRdf
  return pages

parseRdf :: forall o. ConduitM Event o (ResourceT IO) (Maybe [Maybe Page])
parseRdf = invariantTagName "RDF" ignoreAttrs $ const $ many parseExternalPage

parseExternalPage :: forall o. ConduitM Event o (ResourceT IO) (Maybe (Maybe Page))
parseExternalPage = invariantTagName "ExternalPage" (requireAttr "about") $ \url -> do
  let page = defaultPage { pageUrl = fromStrict url }
  page' <- parseTopic page
  return page'

parseTopic :: forall o (m :: * -> *). MonadThrow m => Page -> ConduitM Event o m (Maybe Page)
parseTopic page = invariantTagName "topic" ignoreAttrs $ \_ -> do
  topic <- content
  return $ page { pageTopic = fromStrict topic }

invariantTagName :: forall a o (m :: * -> *) b. MonadThrow m => T.Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
invariantTagName n = tagPredicate ((== n) . nameLocalName)