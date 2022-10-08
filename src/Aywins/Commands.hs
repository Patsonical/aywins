{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aywins.Commands (handleCommand) where

import Aywins.DBActions
import Aywins.Entities
import Aywins.Types
import Control.Monad (when, forM, forM_)
import Data.Bifunctor (Bifunctor(bimap, second))
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Text (Text)
import Database.Esqueleto.Experimental hiding (
         Entity(..), insert, update, delete, (=.), (<&>)
       )
import Database.Persist (Entity (..), insert, update, delete, (=.))
import Database.Persist.Sqlite (runSqlite)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Discord.Types as D

-- Messages {{{
notAdminError, userBannedError, helpMessageUser, helpMessageAdmin  :: ErrorMsg

notAdminError = "ERROR: Action forbidden to non-admins"
userBannedError = "ERROR: User is banned"
helpMessageUser = ""
helpMessageAdmin = ""
-- }}}

-- Helper functions {{{
idToByteString :: D.DiscordId a -> ByteString
idToByteString = toByteString' . D.unSnowflake . D.unId

discordUserToBS :: D.User -> ByteString
discordUserToBS = idToByteString . D.userId

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
-- }}}

handleCommand :: D.GuildMember -> D.RoleId -> Command -> IO Status
handleCommand thisMember adminRoleId cmd = let

  thisUser = (fromJust . D.memberUser) thisMember

  isAdmin = adminRoleId `elem` D.memberRoles thisMember

  adminCheck :: SqlAction Status -> SqlAction Status
  adminCheck authAction = if isAdmin then authAction else pure $ Error notAdminError

  in runSqlite "db.sqlite3" $ case cmd of

  Iwon gName -> do
    user <- getOrDefaultUser (discordUserToBS thisUser)
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
        user <- getOrDefaultUser (discordUserToBS thisUser)
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

  Amiwinning gName_maybe -> getSingleUserScores thisUser gName_maybe

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

  Rmself -> let uBS = discordUserToBS thisUser
    in getBy (UniqueUserDiscordId uBS) >>= \case
      Nothing -> pure $ Error "User does not exist"
      Just (Entity userId _) -> delete userId >> pure Success

  Lsgames -> do
    games <- fmap (map unValue) . select $ do
      game <- from $ table @Game
      pure (game ^. #name)
    pure . Message
         . fmtResponse
         . GamesList
         $ games

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

  Mergegames gNames -> adminCheck $
    if N.length gNames < 2 then
      pure $ Error "Insufficient arguments"
    else do
      gamesQueried  <- forM gNames $ getBy . UniqueGameName
      let (targetName   :| _) = gNames
          (target_maybe :| _) = gamesQueried
          gamesZip   = N.zip gamesQueried gNames
          gamesFound :: ([Entity Game], [Text]) -- ([Games found], [Game names missing])
          gamesFound = bimap (map (fromJust . fst)) (map snd) $
                         N.partition (isJust . fst) gamesZip
      (Entity targetId _, toMerge, new) <- case target_maybe of
        Just t  -> pure (t, tail (fst gamesFound), False)
        Nothing -> insertEntity (Game targetName) <&> (, fst gamesFound, True)
      if null toMerge then do
        when new $ delete targetId
        pure $ Error "Merge targets not found"
      else do
        forM_ toMerge $ \(Entity gameId _) -> do
          gameWinsList <- select $ do
            wins <- from $ table @Wins
            where_ (wins ^. #game ==. val gameId)
            pure wins
          forM_ gameWinsList $ \(Entity winsId _) ->
            update winsId [ WinsGame =. targetId ]
        pure $ if null (snd gamesFound) then Success
               else Warning $ T.append "Some games were not found: "
                                       (T.unwords (snd gamesFound))

  Renamegame gOldName gNewName -> adminCheck $
    getBy (UniqueGameName gOldName) >>= \case
      Nothing                -> pure $ Error "Game does not exist"
      Just (Entity gameId _) ->
        update gameId [ GameName =. gNewName ] >> pure Success
