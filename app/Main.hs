{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Aywins.Entities
import Aywins.DBActions

import Database.Persist.Sqlite
import Control.Monad.IO.Class (MonadIO(..))

main :: IO ()
main = runSqlite "db.sqlite3" $ do
  runMigration migrateAll
  let test = User "test discord id" False
      btry = Game "Betrayal"
  testId <- insert test
  btryId <- insert btry
  wId  <- addWin (Entity testId test) (Entity btryId btry)
  w <- get wId
  liftIO $ do
    print w
