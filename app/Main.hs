{-# LANGUAGE OverloadedStrings #-}

module Main where

import Entities

import Database.Persist.Sqlite
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(..))

main :: IO ()
main = runSqlite "db.sqlite3" $ do
  t <- liftIO getCurrentTime
  runMigration migrateAll
  btId <- insert $ Game "Betrayal" 0
  bt <- get btId
  usId <- insert $ User "test discord id" 0 False
  us <- get usId
  wId  <- insert $ Win usId btId t
  w <- get wId
  liftIO $ do
    print bt
    print us
    print w
