module Data.Dmoz.Options
  ( Options (..)
  , getOpts
  ) where

import System.Console.GetFlag

data Options = Options
  { optInputPath :: String
  , optOutputPath :: Maybe String
  , optTopics :: String
  , optDbPath :: String
  , optDbTable :: String
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optInputPath = "content.rdf.u8"
  , optOutputPath = Nothing
  , optTopics = "Computers/Robotics/Research" -- "Top/Arts/Animation,Recreation/Outdoors/Guides_and_Outfitters"
  , optDbPath = "dmoz_dataset_extractor.sqlite"
  , optDbTable = "dmoz_dataset_extractor"
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "f"
      (ReqArg (\x opts -> opts { optInputPath = x }) "FILE")
      "DMOZ RDF XML File Path"
  , Option "o"
      (ReqArg (\x opts -> opts { optOutputPath = Just x }) "FILE")
      "Output File Path"
  , Option "c"
      (ReqArg (\x opts -> opts { optTopics = x }) "TOPIC,TOPIC,...")
      "Topics/Categories To Spider"
  , Option "db"
      (ReqArg (\x opts -> opts { optDbPath = x }) "FILE")
      "SQLite Database Path"
  , Option "t"
      (ReqArg (\x opts -> opts { optDbTable = x }) "TABLE")
      "SQLite Database Table"
  ]

getOpts :: [String] -> IO Options
getOpts argv =
    case getOpt Permute options argv of
      (o, _, []) -> return $ foldl (flip id) defaultOptions o
      (_, _, errs) -> ioError $ userError $ concat errs ++ info
  where
    header = "Usage: dmoz-dataset-extractor [OPTION...] < DMOZ_RDF_FILE"
    info = usageInfo header options