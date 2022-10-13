{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Aywins.Responses (handleStatus, fmtResponse) where

import Aywins.Types
import Discord.Interactions (InteractionResponse, interactionResponseBasic)
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import qualified Discord.Types as D
import Aywins.Lib
import Discord.Internal.Rest.User (UserRequest(..))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, fromJust)
import Discord.Internal.Rest (User(userMember), GuildMember (..))
import Data.ByteString.Conversion (fromByteString)
import Control.Monad (forM)

handleStatus :: Status -> DiscordHandler InteractionResponse
handleStatus = fmap interactionResponseBasic . \case
  Success     -> pure "Success :thumbsup:"
  Failure err -> pure ("***ERROR:*** " `T.append` translateError err)
  Warning msg -> pure ("**Warning:** " `T.append` msg)
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
tShow = T.pack . show

getUser :: D.UserId -> DiscordHandler (Maybe D.User)
getUser uid = rightToMaybe <$> restCall (GetUser uid)

userToName :: D.UserId -> Maybe D.User -> Text
userToName uid user_maybe = NE.last (defaultName :| names)
  where defaultName = T.pack . show . D.unSnowflake . D.unId $ uid
        names = catMaybes [ username
                          , usernameDiscrim
                          , guildmemberNick
                          ]
        username        = D.userName <$> user_maybe
        usernameDiscrim = T.append <$> username <*> (user_maybe >>= D.userDiscrim)
        guildmemberNick = user_maybe >>= userMember >>= memberNick

userIdToName :: D.UserId -> DiscordHandler Text
userIdToName uid = userToName uid <$> getUser uid

bold, italic, boldItalic, code, codeBlock :: Text -> Text
bold t       = T.concat [ "**"   , T.strip t, "**"    ]
italic t     = T.concat [ "*"    , T.strip t, "*"     ]
boldItalic t = T.concat [ "***"  , T.strip t, "***"   ]
code t       = T.concat [ "`"    , t        , "`"     ]
codeBlock t  = T.concat [ "```\n", t        , "\n```" ]

-- TODO
fmtResponse :: Response -> DiscordHandler Text
fmtResponse = let
  gameLeaderboard game userScores = do
    let header = T.concat [ "Scores for ", bold game, ":" ]
    body      <- forM userScores $ \(userBS, score) -> do
      let userId = D.DiscordId . D.Snowflake . fromJust . fromByteString $ userBS
      username <- userIdToName userId
      pure $ T.concat [ "- ", username, ": ", code (tShow score) ]
    pure $ header `T.append` T.unlines body
  in \case
  SingleUserResponse      userId gameScores -> do
    username <- userIdToName userId
    let header = T.concat [ bold username, "'s scores:\n" ]
        body   = T.unlines . flip map gameScores $ \(game, score) ->
                   T.concat [ "- ", game, ": ", code (tShow score) ]
    pure $ header `T.append` body
  ScoreResponse           userId game score -> do
    username <- userIdToName userId
    pure $ T.concat [ bold username, "'s score in ", game, ": ", code (tShow score) ]
  GameLeaderboardResponse game userScores -> gameLeaderboard game userScores
  FullLeaderboardResponse leaderboard     -> fmap T.unlines . forM leaderboard 
                                                            $ uncurry gameLeaderboard
  GamesListResponse       games           -> pure . T.unlines . map ("- " `T.append`) $ games
