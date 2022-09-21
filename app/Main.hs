{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Entities

import Database.Persist.Sqlite
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Text (Text)

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type ErrorMsg = Text

type Status = Maybe ErrorMsg

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

--  Authenticate user before setScore
--  Entity userId user <- getOrInsert (UniqueUserDiscordId discordIdBytestring)
--                                    (User discordIdBytestring False)
--  Entity gameId _    <- getOrInsert (UniqueGameName gameNameRaw)
--                                    (Game gameNameRaw)
--  if userBanned user then
--    pure $ Left "User is banned, ignoring setting"


setScore :: ScoreMod -> Entity User -> Entity Game -> SqlAction (Key Wins)
setScore scoreMod (Entity userId _) (Entity gameId _) = do

  let scoreModFn = scoreModToFunc scoreMod
      isInc :: ScoreMod -> Bool
      isInc (Inc _) = True
      isInc _       = False

  Entity winsId wins <- getOrInsert (UniqueWinUserGame userId gameId)
                                    (Wins userId gameId 0 Nothing)

  let currentScore = winsScore wins
  timeNow <- liftIO getCurrentTime

  if isInc scoreMod then
    update winsId [ WinsScore =. scoreModFn currentScore
                  , WinsLastWinDate =. Just timeNow ]

  else
    update winsId [ WinsScore =. scoreModFn currentScore ]

  pure winsId

addWin :: Entity User -> Entity Game -> SqlAction (Key Wins)
addWin = setScore (Inc 1)

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
