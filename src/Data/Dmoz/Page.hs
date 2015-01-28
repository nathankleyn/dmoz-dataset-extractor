{-# LANGUAGE OverloadedStrings #-}

module Data.Dmoz.Page
  ( Page (..)
  , defaultPage
  ) where

import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString, empty)

data Page = Page
  { pageUrl :: Text
  , pageTopic :: Text
  , pageContent :: ByteString
  } deriving (Show)

defaultPage :: Page
defaultPage = Page
  { pageUrl = ""
  , pageTopic = ""
  , pageContent = empty
  }