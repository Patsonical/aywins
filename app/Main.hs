module Main where

import Entities

import Database.Persist.Sqlite
import Data.ByteString.Conversion (toByteString')
import Data.Text (pack)
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(..))

main :: IO ()
main = runSqlite (pack "db.sqlite3") $ do
  t <- liftIO getCurrentTime
  runMigration migrateAll
  btId <- insert $ Game (pack "Betrayal") 0
  bt <- get btId
  usId <- insert $ User (toByteString' "test discord id") 0
  us <- get usId
  wId  <- insert $ Win usId btId t
  w <- get wId
  liftIO $ do
    print bt
    print us
    print w
