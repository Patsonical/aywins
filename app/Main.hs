{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Entities

import Database.Persist.Sqlite
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Text (Text)
import Data.Word (Word64)
import Data.ByteString.Conversion (toByteString')

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type ErrorMsg = Text
type IsAywinsAdmin = Bool

getOrInsert :: (PersistEntity record,
  PersistEntityBackend record ~ SqlBackend) =>
  Unique record -> record -> SqlAction (Entity record)
getOrInsert unique default_ = do
  maybeUnique <- getBy unique
  case maybeUnique of
    Nothing -> do
      recordId <- insert default_
      pure $ Entity recordId default_
    Just entity -> pure entity

setScore :: IsAywinsAdmin -> Text -> Word64 -> (Int -> Int) -> SqlAction (Either ErrorMsg ())
setScore isAdmin gameNameRaw discordIdRaw scoreMod = do

  let discordIdBytestring = toByteString' discordIdRaw

  Entity gameId game <- getOrInsert (UniqueGameName gameNameRaw)
                                    (Game gameNameRaw 0)
  Entity userId user <- getOrInsert (UniqueDiscordId discordIdBytestring)
                                    (User discordIdBytestring 0 False)

  let oldUserScore = userTotalWins user
      newUserScore = max 0 $ scoreMod oldUserScore
      difference   = newUserScore - oldUserScore
      newGameScore = max 0 $ gameTotalWins game + difference

  if userBanned user && not isAdmin then
    pure $ Left "User is banned, ignoring setting"
  else do
    timeNow <- liftIO getCurrentTime
    _ <- insert $ SetScore userId gameId timeNow difference
    update gameId [ GameTotalWins =. newGameScore ]
    update userId [ UserTotalWins =. newUserScore ]
    pure $ Right ()

addWin :: IsAywinsAdmin -> Text -> Word64 -> SqlAction (Either ErrorMsg ())
addWin isAdmin gameNameRaw discordIdRaw = do

  let discordIdBytestring = toByteString' discordIdRaw

  Entity gameId _    <- getOrInsert (UniqueGameName gameNameRaw)
                                    (Game gameNameRaw 0)
  Entity userId user <- getOrInsert (UniqueDiscordId discordIdBytestring)
                                    (User discordIdBytestring 0 False)

  if userBanned user && not isAdmin then
    pure $ Left "User is banned, ignoring win"
  else do
    timeNow <- liftIO getCurrentTime
    _ <- insert $ Win userId gameId timeNow
    update gameId [ GameTotalWins +=. 1 ]
    update userId [ UserTotalWins +=. 1 ]
    pure $ Right ()

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
