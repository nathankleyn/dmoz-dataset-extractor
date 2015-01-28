{-# LANGUAGE OverloadedStrings #-}

module Data.Dmoz.Storer
  ( storeInFile
  , storeInDb
  ) where

import Codec.Text.IConv (Fuzzy(Transliterate), convertFuzzy)
import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (unpack, intercalate)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.HDBC (Statement, commit, prepare, execute, toSql, fetchRow)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import System.IO (IOMode(WriteMode), withFile, hPutStrLn)

import Data.Dmoz.Page

type Table = String

storeInFile :: FilePath -> [Page] -> IO ()
storeInFile _ [] = return ()
storeInFile path ps = do
    withFile path WriteMode $ \h -> do
      mapM_ (writePage h) ps
  where
    writePage h p = hPutStrLn h $ unpack $ intercalate "\n" [pageUrl p, pageTopic p, decodeUtf8 $ encodeContent $ pageContent p, "---"]

storeInDb :: FilePath -> Table -> [Page] -> IO ()
storeInDb _ _ [] = return ()
storeInDb dbPath table ps = do
    c <- connectSqlite3 dbPath
    _ <- createTableInDb table c
    _ <- mapConcurrently (storePageInDb table c) ps
    commit c
    return ()

createTableInDb :: Table -> Connection -> IO Integer
createTableInDb table c = do
  create <- sqlCreateTable table c
  execute create []

storePageInDb :: Table -> Connection -> Page -> IO Integer
storePageInDb table c page = do
  find <- sqlFind table c
  insert <- sqlInsert table c

  _ <- execute find [toSql $ pageUrl page]
  result <- fetchRow find

  case result of
    Just _ -> return 0
    Nothing -> execute insert [ toSql $ pageUrl page
                              , toSql $ pageTopic page
                              , toSql $ encodeContent $ pageContent page
                              ]

sqlCreateTable :: Table -> Connection -> IO Statement
sqlCreateTable table c = prepare c ("CREATE TABLE IF NOT EXISTS " ++ table ++ " (url TEXT PRIMARY KEY NOT NULL, topic TEXT NOT NULL, content TEXT NOT NULL);")

sqlFind :: Table -> Connection -> IO Statement
sqlFind table c = prepare c ("SELECT * FROM " ++ table ++ " WHERE url = ?;")

sqlInsert :: Table -> Connection -> IO Statement
sqlInsert table c = prepare c ("INSERT INTO " ++ table ++ " VALUES (?, ?, ?);")

encodeContent :: ByteString -> ByteString
encodeContent bs = convertFuzzy Transliterate "UTF-8" "UTF-8" bs