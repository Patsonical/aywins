{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Aywins.DBActions
  ( getOrInsert
  , setScore
  , addWin
  ) where

import Aywins.Types
import Aywins.Entities

import Database.Persist.Sqlite
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (getCurrentTime)

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

  let scoreModFn = scoreModToFn scoreMod
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
