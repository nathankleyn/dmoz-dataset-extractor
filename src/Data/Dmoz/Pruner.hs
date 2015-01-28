{-# LANGUAGE OverloadedStrings #-}

module Data.Dmoz.Pruner
  ( prune ) where

import Prelude hiding (lines, readFile, unlines)
import Data.Text.Lazy (Text, isInfixOf, lines, unlines)
import Data.Text.Lazy.IO (readFile)

prune :: String -> IO Text
prune file = do
  content <- readFile file
  return $ unlines $ processLines $ lines content

processLines :: [Text] -> [Text]
processLines [] = []
processLines ls =
  let
    l = head ls
    ls' = tail ls
    predicate = any (\t -> t `isInfixOf` l) matchersToKeep

  in
    if predicate
    then l : (processLines ls')
    else processLines ls'

matchersToKeep :: [Text]
matchersToKeep = ["<?xml", "<RDF", "<ExternalPage", "<topic", "</ExternalPage>", "</RDF>"]