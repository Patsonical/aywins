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

data ScoreMod = Set Int | Inc Int | Dec Int
scoreModToFunc :: ScoreMod -> (Int -> Int)
scoreModToFunc (Set n) = const n
scoreModToFunc (Inc n) = (+ n)
scoreModToFunc (Dec n) = \x -> x - n

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

setScore :: ScoreMod -> Word64 -> Text -> SqlAction (Either ErrorMsg ())
setScore scoreMod discordIdRaw gameNameRaw = do

  let discordIdBytestring = toByteString' discordIdRaw
      smfn = scoreModToFunc scoreMod
      isInc :: ScoreMod -> Bool
      isInc (Inc _) = True
      isInc _       = False

  Entity userId user <- getOrInsert (UniqueUserDiscordId discordIdBytestring)
                                    (User discordIdBytestring False)
  Entity gameId _    <- getOrInsert (UniqueGameName gameNameRaw)
                                    (Game gameNameRaw)
  Entity winsId wins <- getOrInsert (UniqueWinUserGame userId gameId)
                                    (Wins userId gameId 0 Nothing)

  if userBanned user then
    pure $ Left "User is banned, ignoring setting"

  else do
    let currentScore = winsScore wins
    timeNow <- liftIO getCurrentTime

    if isInc scoreMod then
      update winsId [ WinsScore =. smfn currentScore, WinsLastWinDate =. Just timeNow ]

    else
      update winsId [ WinsScore =. smfn currentScore ]

    pure $ Right ()

addWin :: Word64 -> Text -> SqlAction (Either ErrorMsg ())
addWin = setScore (Inc 1)

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
