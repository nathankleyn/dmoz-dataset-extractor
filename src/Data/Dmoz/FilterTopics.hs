{-# LANGUAGE OverloadedStrings #-}

module Data.Dmoz.FilterTopics
  ( filterTopics ) where

import Data.List.Split (splitOn)
import Data.Text.Lazy (isInfixOf, pack)

import Data.Dmoz.Page (Page(..))

filterTopics :: String -> [Maybe Page] -> [Maybe Page]
filterTopics topics pages =
  let
    topics' = map pack $ splitOn "," topics
    filterPredicate Nothing = False
    filterPredicate (Just page) = any (\topic -> topic `isInfixOf` (pageTopic page)) topics'
  in
    filter filterPredicate pages