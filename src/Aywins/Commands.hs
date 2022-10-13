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
import Aywins.Lib
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
import Discord
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Discord.Types as D
import Control.Monad.IO.Class (MonadIO(..))
import Discord.Types (GuildMember(..))

-- Messages {{{
helpMessageUser, helpMessageAdmin  :: Text

helpMessageUser = ""
helpMessageAdmin = ""
-- }}}

-- Helper functions {{{
idToByteString :: D.DiscordId a -> ByteString
idToByteString = toByteString' . D.unSnowflake . D.unId

discordUserToBS :: D.User -> ByteString
discordUserToBS = idToByteString . D.userId

getSingleUserScores :: D.UserId -> Maybe Text -> SqlAction Status
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
      where_ (user ^. #discordId ==. val (idToByteString discordUser))
      pure (game ^. #name, wins ^. #score)
    pure . Reply
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
      where_ (user ^. #discordId ==. val (idToByteString discordUser))
      where_ (game ^. #name ==. val gName)
      pure (wins ^. #score)
    pure . Reply
         . ScoreResponse discordUser gName
         . unValue
         . fromMaybe (Value 0)
         $ results
-- }}}

handleCommand :: D.GuildMember -> D.RoleId -> Command -> DiscordHandler Status
handleCommand thisMember adminRoleId cmd = let
  thisUser              = (fromJust . D.memberUser) thisMember
  isAdmin               = adminRoleId `elem` D.memberRoles thisMember
  adminCheck authAction = if isAdmin then authAction else pure $ Failure UserNotAdminError
  in runSqlite "db.sqlite3" $ case cmd of
    Iwon gName -> do
      user <- getOrDefaultUser (discordUserToBS thisUser)
      if (userBanned . entityVal) user then
        pure $ Failure UserBannedError
      else do
        game <- getOrDefaultGame gName
        _    <- addWin user game
        pure Success

    Setscore discordUser_maybe scoreMod gName -> do
      targeting <- case discordUser_maybe of
        Just target -> if isAdmin then
            Right <$> getOrDefaultUser (idToByteString target)
          else
            pure $ Left UserNotAdminError
        Nothing -> do
          user <- getOrDefaultUser (discordUserToBS thisUser)
          if (userBanned . entityVal) user && not isAdmin then
            pure $ Left UserBannedError
          else
            pure $ Right user
      case targeting of
        Left err   -> pure $ Failure err
        Right user -> do
          game <- getOrDefaultGame gName
          _    <- setScore scoreMod user game
          pure Success

    Amiwinning gName_maybe -> getSingleUserScores (D.userId thisUser) gName_maybe

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
        pure . Reply
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
        pure . Reply
             . GameLeaderboardResponse gName
             . map (bimap unValue unValue)
             $ results

    Rmself -> let uBS = discordUserToBS thisUser
      in getBy (UniqueUserDiscordId uBS) >>= \case
        Nothing -> pure $ Failure UserMissingError
        Just (Entity userId _) -> delete userId >> pure Success

    Lsgames -> do
      games <- fmap (map unValue) . select $ do
        game <- from $ table @Game
        pure (game ^. #name)
      pure . Reply
           . GamesListResponse
           $ games

    --AywinsHelp -> pure . Message $
    --  if isAdmin then helpMessageAdmin
    --             else helpMessageUser
    AywinsHelp -> pure (Failure NotImplError)

    Shoutatpatryk complaint -> do
      let signature = fromMaybe "NO NAME" (memberNick thisMember)
          signedComplaint = T.concat [ signature, ": ", complaint ]
      liftIO $ logToFile "log/aywins_problems.log" signedComplaint
      pure Success

    -- ADMIN STUFF --
    Theywon discordUser gName -> adminCheck $ do
      user <- getOrDefaultUser (idToByteString discordUser)
      if (userBanned . entityVal) user then
        pure $ Failure UserBannedError
      else do
        game <- getOrDefaultGame gName
        _    <- addWin user game
        pure Success

    Addgame gName -> adminCheck $
      getBy (UniqueGameName gName) >>= \case
        Just _  -> pure $ Failure GameExistsError
        Nothing -> insert (Game gName) >> pure Success

    Rmgame gName -> adminCheck $
      getBy (UniqueGameName gName) >>= \case
        Nothing                 -> pure $ Failure GameMissingError
        Just (Entity gameId _)  -> delete gameId >> pure Success

    Adduser discordUser -> adminCheck $ let uBS = idToByteString discordUser
      in getBy (UniqueUserDiscordId uBS) >>= \case
        Just _  -> pure $ Failure UserExistsError
        Nothing -> insert (User uBS False) >> pure Success

    Rmuser discordUser -> adminCheck $ let uBS = idToByteString discordUser
      in getBy (UniqueUserDiscordId uBS) >>= \case
        Nothing                -> pure $ Failure UserMissingError
        Just (Entity userId _) -> delete userId >> pure Success

    Banuser discordUser -> adminCheck $ let uBS = idToByteString discordUser
      in getBy (UniqueUserDiscordId uBS) >>= \case
        Nothing                   -> pure $ Failure UserMissingError
        Just (Entity userId user) -> if userBanned user then
            pure $ Warning "Target user is already banned"
          else
            update userId [ UserBanned =. True ] >> pure Success

    Unbanuser discordUser -> adminCheck $ let uBS = idToByteString discordUser
      in getBy (UniqueUserDiscordId uBS) >>= \case
        Nothing                   -> pure $ Failure UserMissingError
        Just (Entity userId user) -> if userBanned user then
            update userId [ UserBanned =. False ] >> pure Success
          else
            pure $ Warning "Target user is not banned"

    Mergegames gNames -> adminCheck $ do
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
        pure $ Failure MergeTargetsMissingError
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
        Nothing                -> pure $ Failure GameMissingError
        Just (Entity gameId _) ->
          update gameId [ GameName =. gNewName ] >> pure Success
