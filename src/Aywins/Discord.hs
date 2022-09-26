{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE TupleSections #-}

module Aywins.Discord where

import Aywins.DBActions
import Aywins.Entities
import Aywins.Types
import Data.Bifunctor (Bifunctor(bimap, second))
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.List (partition, uncons)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Word (Word64)
import Database.Persist.Sqlite (runSqlite)
import qualified Data.Text as T
import qualified Discord.Types as D
import qualified Database.Esqueleto.Experimental as E
import Database.Esqueleto.Experimental hiding (Entity(..), insert, update, delete, (=.))
import qualified Database.Persist as P
import Database.Persist (Entity (..), insert, update, delete, (=.))

-- Temp "global" variables --
isAdmin :: Bool
isAdmin = True

globalUserId :: D.UserId
globalUserId = D.DiscordId . D.Snowflake $ (123456789 :: Word64)

globalUser :: D.User
globalUser = error "USER"
-- --

idToByteString :: D.DiscordId a -> ByteString
idToByteString = toByteString' . D.unSnowflake . D.unId

discordUserToBS :: D.User -> ByteString
discordUserToBS = idToByteString . D.userId

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

getSingleUserScores :: D.User -> Maybe Text -> SqlAction Status
getSingleUserScores discordUser = \case
  Nothing -> do
  -- SELECT       game, score
  --   FROM       Wins
  --   INNER_JOIN User ON Wins.user = User.id
  --   INNER_JOIN Game ON Wins.game = Game.id
  --   WHERE      User.discordId == idToByteString discordUser
    results <- select $ do
      (wins :& user :& game) <- from $ table @Wins
        `innerJoin` table @User `on` (\(w :& u)      -> w ^. #user ==. u ^. #id)
        `innerJoin` table @Game `on` (\(w :& _ :& g) -> w ^. #game ==. g ^. #id)
      where_ (user ^. #discordId ==. val (discordUserToBS discordUser))
      pure (game ^. #name, wins ^. #score)
    pure . Message
         . fmtResponse
         . SingleUserResponse discordUser
         . map (bimap unValue unValue)
         $ results

  Just gName -> do
  -- SELECT       score
  --   FROM       Wins
  --   INNER_JOIN User ON Wins.user = User.id
  --   INNER_JOIN Game ON Wins.game = Game.id
  --   WHERE      User.discordId == idToByteString discordUser
  --     AND      Game.name == gName
    results <- selectOne $ do
      (wins :& user :& game) <- from $ table @Wins
        `innerJoin` table @User `on` (\(w :& u)      -> w ^. #user ==. u ^. #id)
        `innerJoin` table @Game `on` (\(w :& _ :& g) -> w ^. #game ==. g ^. #id)
      where_ (user ^. #discordId ==. val (discordUserToBS discordUser))
      where_ (game ^. #name ==. val gName)
      pure (wins ^. #score)
    pure . Message
         . fmtResponse
         . ScoreResponse discordUser gName
         . unValue
         . fromMaybe (Value 0)
         $ results

cmdTest :: Command -> IO Status
cmdTest cmd = runSqlite "db.sqlite3" $ case cmd of

  Iwon gName -> do
    user <- getOrDefaultUser (idToByteString globalUserId)
    if (userBanned . entityVal) user then
      pure $ Error userBannedError
    else do
      game <- getOrDefaultGame gName
      _    <- addWin user game
      pure Success

  Setscore discordUser_maybe scoreMod gName -> do
    targeting <- case discordUser_maybe of
      Just target -> if isAdmin then
          Right <$> getOrDefaultUser (discordUserToBS target)
        else
          pure $ Left notAdminError
      Nothing -> do
        user <- getOrDefaultUser (idToByteString globalUserId)
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


  Amiwinning gName_maybe -> getSingleUserScores globalUser gName_maybe

  Aretheywinning discordUser gName_maybe -> getSingleUserScores discordUser gName_maybe

  Whoiswinning gName_maybe -> case gName_maybe of
    Nothing -> do
      games <- fmap (map unValue) . select $ do
        game <- from $ table @Game
        pure (game ^. #name)
      results <- forM games $ \gameLookup ->
        fmap (gameLookup,) . select $ do
          (wins :& user :& game) <- from $ table @Wins
            `innerJoin` table @User `on` (\(w :& u)      -> w ^. #user ==. u ^. #id)
            `innerJoin` table @Game `on` (\(w :& _ :& g) -> w ^. #game ==. g ^. #id)
          where_ (game ^. #name ==. val gameLookup)
          pure (user ^. #discordId, wins ^. #score)
      pure . Message
           . fmtResponse
           . FullLeaderboardResponse
           . map (second (map (bimap unValue unValue)))
           $ results
    Just gName -> do
      results <- select $ do
        (wins :& user :& game) <- from $ table @Wins
          `innerJoin` table @User `on` (\(w :& u)      -> w ^. #user ==. u ^. #id)
          `innerJoin` table @Game `on` (\(w :& _ :& g) -> w ^. #game ==. g ^. #id)
        where_ (game ^. #name ==. val gName)
        pure (user ^. #discordId, wins ^. #score)
      pure . Message
           . fmtResponse
           . GameLeaderboardResponse gName
           . map (bimap unValue unValue)
           $ results

  Rmself -> let uBS = discordUserToBS globalUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing -> pure $ Error "User does not exist"
      Just (Entity userId _) -> delete userId >> pure Success

  AywinsHelp -> pure . Message $
    if isAdmin then helpMessageAdmin
               else helpMessageUser

  -- ADMIN STUFF --
  Theywon discordUser gName -> adminCheck $ do
    user <- getOrDefaultUser (discordUserToBS discordUser)
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

  Adduser discordUser -> adminCheck $ let uBS = discordUserToBS discordUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Just _  -> pure $ Error "User already exists"
      Nothing -> insert (User uBS False) >> pure Success

  Rmuser discordUser -> adminCheck $ let uBS = discordUserToBS discordUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing                -> pure $ Error "User does not exist"
      Just (Entity userId _) -> delete userId >> pure Success

  Banuser discordUser -> adminCheck $ let uBS = discordUserToBS discordUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing                   -> pure $ Error "User does not exist"
      Just (Entity userId user) -> if userBanned user then
          pure $ Error "User already banned"
        else
          update userId [ UserBanned =. True ] >> pure Success

  Unbanuser discordUser -> adminCheck $ let uBS = discordUserToBS discordUser
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
