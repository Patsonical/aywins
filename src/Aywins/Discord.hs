{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Aywins.Discord where
import Database.Persist.Sqlite (runSqlite)
import Aywins.Types
import qualified Discord.Types as D
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Aywins.Entities
import Database.Persist
import Aywins.DBActions

-- Temp "global" variables --
isAdmin :: Bool
isAdmin = True

discordUser :: D.UserId
discordUser = D.DiscordId . D.Snowflake $ (123456789 :: Word64)
-- --

idToByteString :: D.DiscordId a -> ByteString
idToByteString = toByteString' . D.unSnowflake . D.unId

notAdminError :: ErrorMsg
notAdminError = "ERROR: Action forbidden to non-admins"

userBannedError :: ErrorMsg
userBannedError = "ERROR: User is banned"

cmdTest :: Command -> IO Status
cmdTest cmd = runSqlite "db.sqlite3" $ case cmd of

  Iwon gName -> do
    user <- getOrDefaultUser (idToByteString discordUser)
    if (userBanned . entityVal) user then
      pure $ Just userBannedError
    else do
      game <- getOrDefaultGame gName
      _ <- addWin user game
      pure Nothing

  Setscore uId_maybe scoreMod gName -> do
    targeting <- case uId_maybe of
      Just target -> if isAdmin then 
          Right <$> getOrDefaultUser (idToByteString target)
        else 
          pure $ Left notAdminError
      Nothing -> do 
        user <- getOrDefaultUser (idToByteString discordUser)
        if (userBanned . entityVal) user && not isAdmin then
          pure $ Left userBannedError
        else
          pure $ Right user
    case targeting of
      Left err   -> pure $ Just err
      Right user -> do
        game <- getOrDefaultGame gName
        _ <- setScore scoreMod user game
        pure Nothing

  Amiwinning gName_maybe -> pure Nothing

  Aretheywinning uId gName_maybe -> pure Nothing

  Whoiswinning gName_maybe -> pure Nothing

  Rmself -> pure Nothing

  AywinsHelp -> pure Nothing

  Theywon uId gName -> pure Nothing

  Addgame gName -> pure Nothing

  Rmgame gName -> pure Nothing

  Adduser uId -> pure Nothing

  Rmuser uId -> pure Nothing

  Banuser uId -> pure Nothing

  Unbanuser uId -> pure Nothing

  Mergegames gNames -> pure Nothing

  Renamegame gOldName gNewName -> pure Nothing
