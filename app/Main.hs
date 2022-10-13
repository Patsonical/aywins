{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aywins.Discord
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (runMigration)
import Aywins.Entities (migrateAll)

main :: IO ()
main = runSqlite "db.sqlite3" (runMigration migrateAll) >> aywins
