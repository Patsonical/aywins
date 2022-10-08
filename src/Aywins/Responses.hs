{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Aywins.Responses (handleStatus, fmtResponse) where

import Aywins.Types
import Discord.Interactions (InteractionResponse, interactionResponseBasic)
import Data.Text (Text, append, pack)
import Discord (DiscordHandler)

handleStatus :: Status -> DiscordHandler InteractionResponse
handleStatus = fmap interactionResponseBasic . \case
  Success     -> pure "Success :thumbsup:"
  Failure err -> pure ("***ERROR:*** " `append` translateError err)
  Warning msg -> pure ("**Warning:** " `append` msg)
  Message msg -> pure msg
  Reply   rp  -> fmtResponse rp

translateError :: Error -> Text
translateError = \case
  NotImplError             -> "This function hasn't been implemented yet because my creator is a lazy bitch"
  AdminRoleMissingError    -> "This server does not implement the `AywinsAdmin` role, please go yell at the server admins"
  MergeTargetsMissingError -> "Too many requested games were not found in the database, aborting merge"
  ParseFailureError        -> "Error parsing arguments (I literally have no idea what you're saying)"
  GameExistsError          -> "Requested game already exists in the database"
  GameMissingError         -> "Requested game does not exist in the database"
  UserNotAdminError        -> "User is not an admin, access denied (nice try :P)"
  UserExistsError          -> "Requested user already exists in the database"
  UserMissingError         -> "Requested user does not exist in the database"
  UserBannedError          -> "User is banned from Aywins"
  OtherError t             -> t

tShow :: Show a => a -> Text
tShow = pack . show

-- TODO
fmtResponse :: Response -> DiscordHandler Text
fmtResponse = \case
  SingleUserResponse      user gameScores -> pure $ tShow gameScores
  ScoreResponse           user game score -> pure $ tShow score
  GameLeaderboardResponse game userScores -> pure $ tShow userScores
  FullLeaderboardResponse leaderboard     -> pure $ tShow leaderboard
  GamesListResponse       games           -> pure $ tShow games
