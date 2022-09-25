{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Discord where

import Aywins.DBActions
import Aywins.Entities
import Aywins.Types
import Data.Bifunctor (Bifunctor(bimap))
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.List (partition, uncons)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Word (Word64)
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Discord.Types as D

-- Temp "global" variables --
isAdmin :: Bool
isAdmin = True

discordUser :: D.UserId
discordUser = D.DiscordId . D.Snowflake $ (123456789 :: Word64)
-- --

idToByteString :: D.DiscordId a -> ByteString
idToByteString = toByteString' . D.unSnowflake . D.unId

adminCheck :: SqlAction Status -> SqlAction Status
adminCheck authAction = if isAdmin then authAction else pure $ Error notAdminError

notAdminError :: ErrorMsg
notAdminError = "ERROR: Action forbidden to non-admins"

userBannedError :: ErrorMsg
userBannedError = "ERROR: User is banned"

helpMessageUser :: Text
helpMessageUser = ""

helpMessageAdmin :: Text
helpMessageAdmin = ""

cmdTest :: Command -> IO Status
cmdTest cmd = runSqlite "db.sqlite3" $ case cmd of

  Iwon gName -> do
    user <- getOrDefaultUser (idToByteString discordUser)
    if (userBanned . entityVal) user then
      pure $ Error userBannedError
    else do
      game <- getOrDefaultGame gName
      _    <- addWin user game
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
        _    <- setScore scoreMod user game
        pure Success


-- amiwinningResponse0     :: [(Game, Score)]
-- amiwinningResponse1     :: Score
  -- TODO
  Amiwinning gName_maybe -> pure Success

-- aretheywinningResponse0 :: [(Game, Score)]
-- aretheywinningResponse1 :: Score
  -- TODO
  Aretheywinning uId gName_maybe -> pure Success

-- whoiswinningResponse0   :: [(Game, [(User, Score)])]
-- whoiswinningResponse1   :: [(User, Score)]
  -- TODO
  Whoiswinning gName_maybe -> pure Success

  Rmself -> let uBS = idToByteString discordUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing -> pure $ Error "User does not exist"
      Just (Entity userId _) -> delete userId >> pure Success

  AywinsHelp -> pure . Response $
    if isAdmin then helpMessageAdmin
               else helpMessageUser

  -- ADMIN STUFF --
  Theywon uId gName -> adminCheck $ do
    user <- getOrDefaultUser (idToByteString uId)
    if (userBanned . entityVal) user then
      pure $ Error userBannedError
    else do
      game <- getOrDefaultGame gName
      _    <- addWin user game
      pure Success

  Addgame gName -> adminCheck $
    getBy (UniqueGameName gName) >>= \case
      Just _  -> pure $ Error "Game already exists"
      Nothing -> insert (Game gName) >> pure Success

  Rmgame gName -> adminCheck $
    getBy (UniqueGameName gName) >>= \case
      Nothing                 -> pure $ Error "Game does not exists"
      Just (Entity gameId _)  -> delete gameId >> pure Success

  Adduser uId -> adminCheck $ let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Just _  -> pure $ Error "User already exists"
      Nothing -> insert (User uBS False) >> pure Success

  Rmuser uId -> adminCheck $ let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing                -> pure $ Error "User does not exist"
      Just (Entity userId _) -> delete userId >> pure Success

  Banuser uId -> adminCheck $ let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing                   -> pure $ Error "User does not exist"
      Just (Entity userId user) -> if userBanned user then
          pure $ Error "User already banned"
        else
          update userId [ UserBanned =. True ] >> pure Success

  Unbanuser uId -> adminCheck $ let uBS = idToByteString uId
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing                   -> pure $ Error "User does not exist"
      Just (Entity userId user) -> if userBanned user then
          update userId [ UserBanned =. False ] >> pure Success
        else
          pure $ Error "User not banned"

  -- TODO
  Mergegames gNames -> adminCheck $ do
    gamesQueried  <- forM gNames $ \name -> getBy (UniqueGameName name)
    let gamesZip   = zip gamesQueried gNames
        gamesFound :: ([Entity Game], [Text])
        gamesFound = bimap (map (fromJust . fst)) (map snd) $
                       partition (isJust . fst) gamesZip
        (master, rest) = fromJust . uncons . fst $ gamesFound
    pure $ if null (snd gamesFound) then Success
           else Warning $ T.append "Some games were not found: "
                                   (T.unwords (snd gamesFound))

  Renamegame gOldName gNewName -> adminCheck $
    getBy (UniqueGameName gOldName) >>= \case
      Nothing                -> pure $ Error "Game does not exist"
      Just (Entity gameId _) ->
        update gameId [ GameName =. gNewName ] >> pure Success
