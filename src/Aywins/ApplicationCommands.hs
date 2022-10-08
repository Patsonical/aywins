{-# LANGUAGE OverloadedStrings #-}

module Aywins.ApplicationCommands (applicationCommands) where

import Discord.Interactions
import Data.Text (Text)

mkAppCommand :: Text -> Text -> Maybe Options -> CreateApplicationCommand
mkAppCommand name description options = CreateApplicationCommandChatInput {
  createName = name
, createLocalizedName = Nothing
, createDescription = description
, createLocalizedDescription = Nothing
, createOptions = options
, createDefaultMemberPermissions = Nothing
, createDMPermission = Just False
}

mkOptStr, mkOptUser :: Bool  -> Text -> Text -> OptionValue
mkOptStr required name description = OptionValueString {
  optionValueName = name
, optionValueLocalizedName = Nothing
, optionValueDescription = description
, optionValueLocalizedDescription = Nothing
, optionValueRequired = required
, optionValueStringChoices = Left False
, optionValueStringMinLen = Nothing
, optionValueStringMaxLen = Nothing
}

mkOptUser required name description = OptionValueUser {
  optionValueName = name
, optionValueLocalizedName = Nothing
, optionValueDescription = description
, optionValueLocalizedDescription = Nothing
, optionValueRequired = required
}

applicationCommands :: [CreateApplicationCommand]
applicationCommands = let
  iwonOpts = Just $ OptionsValues [
      mkOptStr True "game" "Name of the game you won"
    ]
  setscoreOpts = Just $ OptionsValues [
      mkOptStr True "scoremod" "Score modifier: n | +n | -n"
    , mkOptStr True "game" "Name of the game to modify the score for"
    , mkOptUser False "target" "User to modify the score for (admin only)"
    ]
  amiwinningOpts = Just $ OptionsValues [
      mkOptStr False "game" "Name of the game to get scores for (optional)"
    ]
  aretheywinningOpts = Just $ OptionsValues [
      mkOptUser True "target" "User to get scores for"
    , mkOptStr False "game" "Name of the game to get scores for (optional)"
    ]
  whoiswinningOpts = Just $ OptionsValues [
      mkOptStr False "game" "Name of the game to get scores for (optional)"
    ]
  rmselfOpts = Nothing
  lsgamesOpts = Nothing
  aywinshelpOpts = Nothing
  theywonOpts = Just $ OptionsValues [
      mkOptUser True "target" "User to add a win for"
    , mkOptStr True "game" "Name of the game to add a win for"
    ]
  addgameOpts = Just $ OptionsValues [
      mkOptStr True "game" "Name of the game to add"
    ]
  rmgameOpts = Just $ OptionsValues [
      mkOptStr True "game" "Name of the game to remove"
    ]
  adduserOpts = Just $ OptionsValues [
      mkOptUser True "user" "User to add"
    ]
  rmuserOpts = Just $ OptionsValues [
      mkOptUser True "user" "User to remove"
    ]
  banuserOpts = Just $ OptionsValues [
      mkOptUser True "user" "User to ban"
    ]
  unbanuserOpts = Just $ OptionsValues [
      mkOptUser True "user" "User to unban"
    ]
  mergegamesOpts = Just $ OptionsValues [
      mkOptStr True  "targetgame" "Name of target game"
    , mkOptStr True  "mergegame1" "Name of game to merge 1"
    , mkOptStr False "mergegame2" "Name of game to merge 2"
    , mkOptStr False "mergegame3" "Name of game to merge 3"
    , mkOptStr False "mergegame4" "Name of game to merge 4"
    ]
  renamegameOpts = Just $ OptionsValues [
      mkOptStr True "oldname" "Original game name"
    , mkOptStr True "newname" "New game name"
    ]
  in [
    mkAppCommand "iwon"           "Increment your score in <game>"                              iwonOpts
  , mkAppCommand "setscore"       "Set your score in <game> (or another user's if admin)"       setscoreOpts
  , mkAppCommand "amiwinning"     "Get your score for all games or a specific <game>"           amiwinningOpts
  , mkAppCommand "aretheywinning" "Get another user's score for all games or a specific <game>" aretheywinningOpts
  , mkAppCommand "whoiswinning"   "Get global leaderboards for all games or a specific <game>"  whoiswinningOpts
  , mkAppCommand "rmself"         "Remove self from Aywins leaderboards"                        rmselfOpts
  , mkAppCommand "lsgames"        "List games in Aywins leaderboards"                           lsgamesOpts
  , mkAppCommand "aywinshelp"     "Display Aywins help message`"                                aywinshelpOpts
  , mkAppCommand "theywon"        "Increment <user>'s score in <game>"                          theywonOpts
  , mkAppCommand "addgame"        "Add <game> to Aywins"                                        addgameOpts
  , mkAppCommand "rmgame"         "Remove <game> from Aywins"                                   rmgameOpts
  , mkAppCommand "adduser"        "Add <user> to Aywins"                                        adduserOpts
  , mkAppCommand "rmuser"         "Remove <user> from Aywins"                                   rmuserOpts
  , mkAppCommand "banuser"        "Ban <user> from Aywins"                                      banuserOpts
  , mkAppCommand "unbanuser"      "Unban <user> from Aywins"                                    unbanuserOpts
  , mkAppCommand "mergegames"     "Consolidate two or more games into one"                      mergegamesOpts
  , mkAppCommand "renamegame"     "Change name of a game"                                       renamegameOpts
  ]
