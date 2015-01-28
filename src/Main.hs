module Main
    ( main ) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import Data.Dmoz.Crawler (crawl)
import Data.Dmoz.Extractor (extract)
import Data.Dmoz.FilterTopics (filterTopics)
import Data.Dmoz.Options (Options(..), getOpts)
import Data.Dmoz.Pruner (prune)
import Data.Dmoz.Storer (storeInFile, storeInDb)

main :: IO ()
main = do
  args <- getArgs
  options <- getOpts args

  prunedXml <- prune $ optInputPath options
  partialPages <- liftM (filterTopics $ optTopics options) $ extract prunedXml
  pages <- crawl $ catMaybes partialPages

  let pages' = catMaybes pages

  case optOutputPath options of
    Nothing -> storeInDb (optDbPath options) (optDbTable options) pages'
    Just outputPath -> storeInFile outputPath pages'

  print $ show $ length partialPages
  print $ show partialPages
  return ()