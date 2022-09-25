{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Traversable (forM)
import Data.Maybe (isJust, fromJust)
import Data.List (partition)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (Text)
import qualified Data.Text as T

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
      pure $ Error userBannedError
    else do
      game <- getOrDefaultGame gName
      _ <- addWin user game
      pure Success

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
      Left err   -> pure $ Error err
      Right user -> do
        game <- getOrDefaultGame gName
        _ <- setScore scoreMod user game
        pure Success

  -- TODO
  Amiwinning gName_maybe -> pure Success

  -- TODO
  Aretheywinning uId gName_maybe -> pure Success

  -- TODO
  Whoiswinning gName_maybe -> pure Success

  -- TODO
  Rmself -> pure Success

  -- TODO
  AywinsHelp -> pure Success

  Theywon uId gName -> do
    user <- getOrDefaultUser (idToByteString uId)
    if (userBanned . entityVal) user then
      pure $ Error userBannedError
    else do
      game <- getOrDefaultGame gName
      _ <- addWin user game
      pure Success

  Addgame gName -> getBy (UniqueGameName gName) >>= \case
    Just _  -> pure $ Error "Game already exists"
    Nothing -> insert (Game gName) >> pure Success

  -- TODO
  Rmgame gName -> pure Success

  Adduser uId -> let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Just _  -> pure $ Error "User already exists"
      Nothing -> insert (User uBS False) >> pure Success

  -- TODO
  Rmuser uId -> pure Success

  Banuser uId -> let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing -> pure $ Error "User does not exist"
      Just (Entity userId user) -> if userBanned user then
          pure $ Error "User already banned"
        else
          update userId [ UserBanned =. True ] >> pure Success

  Unbanuser uId -> let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing -> pure $ Error "User does not exist"
      Just (Entity userId user) -> if userBanned user then
          update userId [ UserBanned =. False ] >> pure Success
        else
          pure $ Error "User not banned"

  -- TODO
  Mergegames gNames -> do
    gamesQueried  <- forM gNames $ \name -> getBy (UniqueGameName name)
    let gamesZip   = zip gamesQueried gNames
        gamesFound :: ([Entity Game], [Text])
        gamesFound = bimap (map (fromJust . fst)) (map snd) $
                       partition (isJust . fst) gamesZip
    pure $ if null (snd gamesFound) then Success
           else Warning $ T.append "Some games were not found: "
                                   (T.unwords (snd gamesFound))

  Renamegame gOldName gNewName -> getBy (UniqueGameName gOldName) >>= \case
    Nothing -> pure $ Error "Game does not exist"
    Just (Entity gameId _) ->
      update gameId [ GameName =. gNewName ] >> pure Success
