{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aywins.Discord (aywins)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (runMigration)
import Aywins.Entities (migrateAll)
import System.Environment (getEnv)
import Configuration.Dotenv (onMissingFile, loadFile, defaultConfig)
import Control.Monad (void)
import Data.Text (pack)

main :: IO ()
main = do
  onMissingFile (void $ loadFile defaultConfig) (pure ())
  token <- pack <$> getEnv "DISCORD_TOKEN"
  runSqlite "db.sqlite3" (runMigration migrateAll)
  aywins token
