module Data.Dmoz.Crawler
  ( crawl ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Lifted (catch)
import Data.ByteString.Lazy (ByteString, empty)
import Data.Text.Lazy (unpack)
import Network.HTTP.Conduit (HttpException, simpleHttp)

import Data.Dmoz.Page

crawl :: [Page] -> IO [Maybe Page]
crawl [] = return []
crawl ps = mapConcurrently getPageContent ps

getPageContent :: Page -> IO (Maybe Page)
getPageContent page = do
  response <- (simpleHttp $ unpack $ pageUrl page) `catch` handleException
  case response of
    x | x == empty -> return Nothing
      | otherwise -> return $ Just $ page { pageContent = response }

handleException :: HttpException -> IO ByteString
handleException _ = return empty