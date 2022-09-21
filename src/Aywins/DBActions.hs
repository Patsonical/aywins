{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Aywins.DBActions where

import Aywins.Types
import Aywins.Entities

import Database.Persist.Sqlite
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (getCurrentTime)
import Data.ByteString (ByteString)
import Data.Text (Text)

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

getOrDefaultUser :: ByteString -> SqlAction (Entity User)
getOrDefaultUser idbs = getOrInsert (UniqueUserDiscordId idbs)
                                    (User idbs False)

getOrDefaultGame :: Text -> SqlAction (Entity Game)
getOrDefaultGame gtxt = getOrInsert (UniqueGameName gtxt)
                                    (Game gtxt)

getOrDefaultWins :: Key User -> Key Game -> SqlAction (Entity Wins)
getOrDefaultWins user game = getOrInsert (UniqueWinUserGame user game)
                                         (Wins user game 0 Nothing)

setScore :: ScoreMod -> Entity User -> Entity Game -> SqlAction (Key Wins)
setScore scoreMod (Entity userId _) (Entity gameId _) = do

  let scoreModFn = scoreModToFn scoreMod
      isInc :: ScoreMod -> Bool
      isInc (Inc _) = True
      isInc _       = False

  Entity winsId wins <- getOrDefaultWins userId gameId

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
